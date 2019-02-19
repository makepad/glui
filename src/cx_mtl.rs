use std::mem;
use std::ffi::CStr;

use cocoa::base::id as cocoa_id;
use cocoa::foundation::{NSRange, NSAutoreleasePool};
use cocoa::appkit::{NSWindow, NSView};
use core_graphics::geometry::CGSize;
use objc::runtime::YES;
use objc::{msg_send, sel, sel_impl};
use metal::*;
use winit::os::macos::WindowExt;

use crate::shader::*;
use crate::cxdrawing::*;
use crate::cxshaders::*;
use crate::cxfonts::*;
use crate::cxtextures::*;
use crate::cxturtle::*;

fn prepare_render_pass_descriptor(descriptor: &RenderPassDescriptorRef, texture: &TextureRef) {
    //descriptor.color_attachments().set_object_at(0, MTLRenderPassColorAttachmentDescriptor::alloc());
    //let color_attachment: MTLRenderPassColorAttachmentDescriptor = unsafe { msg_send![descriptor.color_attachments().0, _descriptorAtIndex:0] };//descriptor.color_attachments().object_at(0);
    let color_attachment = descriptor.color_attachments().object_at(0).unwrap();

    color_attachment.set_texture(Some(texture));
    color_attachment.set_load_action(MTLLoadAction::Clear);
    color_attachment.set_clear_color(MTLClearColor::new(0.5, 0.2, 0.2, 1.0));
    color_attachment.set_store_action(MTLStoreAction::Store);
}


#[derive(Clone)]
pub struct Cx{
    pub title:String,
    pub running:bool,

    pub turtle:CxTurtle,
    pub shaders:CxShaders,
    pub drawing:CxDrawing,
    pub fonts:CxFonts,
    pub textures:CxTextures,

    pub uniforms:Vec<f32>
}

impl Default for Cx{
    fn default()->Self{
        let mut uniforms = Vec::<f32>::new();
        uniforms.resize(CX_UNI_SIZE, 0.0);
        Self{
            turtle:CxTurtle{..Default::default()},
            fonts:CxFonts{..Default::default()},
            drawing:CxDrawing{..Default::default()},
            shaders:CxShaders{..Default::default()},
            textures:CxTextures{..Default::default()},
            title:"Hello World".to_string(),
            running:true,
            uniforms:uniforms
        }
    }
}

const CX_UNI_PROP1:usize = 0;
const CX_UNI_SIZE:usize = 1;

impl Cx{
    pub fn def_shader(sh:&mut Shader){
        Shader::def_df(sh);
        Shader::def_builtins(sh);
        Cx::def_uniforms(sh);
        DrawList::def_uniforms(sh);
    }

    pub fn def_uniforms(_sh: &mut Shader){
        //sh.cx_uniform("prop1", Kind::Float);
    }

    pub fn uniform_prop1(&mut self, v:f32){
        self.uniforms[CX_UNI_PROP1] = v;
    }

    pub fn exec_draw_list(&mut self, id: usize, device:&Device, encoder:&RenderCommandEncoderRef){
        
        // tad ugly otherwise the borrow checker locks 'self' and we can't recur
        for ci in 0..self.drawing.draw_lists[id].draws_len{
            let sub_list_id = self.drawing.draw_lists[id].draws[ci].sub_list_id;
            if sub_list_id != 0{
                self.exec_draw_list(sub_list_id, device, encoder);
            }
            else{
                let draw_list = &mut self.drawing.draw_lists[id];
                let draw = &mut draw_list.draws[ci];

                let sh = &self.shaders.shaders[draw.shader_id];
                let shc = &self.shaders.compiled_shaders[draw.shader_id];
                

                let uni_cx  = device.new_buffer_with_data(
                        unsafe { mem::transmute(self.uniforms.as_ptr()) },
                        (self.uniforms.len() * mem::size_of::<f32>()) as u64,
                        MTLResourceOptions::CPUCacheModeDefaultCache);

                let uni_dl  = device.new_buffer_with_data(
                        unsafe { mem::transmute(draw_list.uniforms.as_ptr()) },
                        (draw_list.uniforms.len() * mem::size_of::<f32>()) as u64,
                        MTLResourceOptions::CPUCacheModeDefaultCache);

                let uni_dr  = device.new_buffer_with_data(
                        unsafe { mem::transmute(draw.uniforms.as_ptr()) },
                        (draw.uniforms.len() * mem::size_of::<f32>()) as u64,
                        MTLResourceOptions::CPUCacheModeDefaultCache);

                let inst_vbuf  = device.new_buffer_with_data(
                        unsafe { mem::transmute(draw.instance.as_ptr()) },
                        (draw.instance.len() * mem::size_of::<f32>()) as u64,
                        MTLResourceOptions::CPUCacheModeDefaultCache);

                let geom_vbuf  = device.new_buffer_with_data(
                        unsafe { mem::transmute(sh.geometry_vertices.as_ptr()) },
                        (sh.geometry_vertices.len() * mem::size_of::<f32>()) as u64,
                        MTLResourceOptions::CPUCacheModeDefaultCache);

                let geom_ibuf  = device.new_buffer_with_data(
                        unsafe { mem::transmute(sh.geometry_indices.as_ptr()) },
                        (sh.geometry_indices.len() * mem::size_of::<u32>()) as u64,
                        MTLResourceOptions::CPUCacheModeDefaultCache);

                let instances = (draw.instance.len() / shc.assembled_shader.instance_slots) as u64;
                if let Some(pipeline_state) = &shc.pipeline_state{
                    encoder.set_render_pipeline_state(pipeline_state);
                    encoder.set_vertex_buffer(0, Some(&geom_vbuf), 0);
                    encoder.set_vertex_buffer(1, Some(&inst_vbuf), 0);
                    encoder.set_vertex_buffer(2, Some(&uni_cx), 0);
                    encoder.set_vertex_buffer(3, Some(&uni_dl), 0);
                    encoder.set_vertex_buffer(4, Some(&uni_dr), 0);
                    encoder.set_fragment_buffer(0, Some(&uni_cx), 0);
                    encoder.set_fragment_buffer(1, Some(&uni_dl), 0);
                    encoder.set_fragment_buffer(2, Some(&uni_dr), 0);
                    encoder.draw_indexed_primitives_instanced(
                        MTLPrimitiveType::Triangle,
                        3, // Index Count
                        MTLIndexType::UInt32, // indexType,
                        &geom_ibuf, // index buffer
                        0, // index buffer offset
                        instances, // instance count
                    )
                }
                // hang on to the buffers
                /*
                draw.buffers.uni_cx = Some(uni_cx);
                draw.buffers.uni_dl = Some(uni_dl);
                draw.buffers.uni_dr = Some(uni_dr);
                draw.buffers.inst_vbuf = Some(inst_vbuf);
                draw.buffers.geom_vbuf = Some(geom_vbuf);
                draw.buffers.geom_ibuf = Some(geom_ibuf);*/

            }
        }
    }

    pub unsafe fn gl_string(raw_string: *const gl::types::GLubyte) -> String {
        if raw_string.is_null() { return "(NULL)".into() }
        String::from_utf8(CStr::from_ptr(raw_string as *const _).to_bytes().to_vec()).ok()
                                    .expect("gl_string: non-UTF8 string")
    }

    pub fn event_loop<F>(&mut self, mut callback:F)
    where F: FnMut(&mut Cx, Ev),
    { 

        let mut events_loop = winit::EventsLoop::new();
        let glutin_window = winit::WindowBuilder::new()
            .with_dimensions((800, 600).into())
            .with_title("Metal".to_string())
            .build(&events_loop).unwrap();

        let window: cocoa_id = unsafe { mem::transmute(glutin_window.get_nswindow()) };
        let device = Device::system_default();

        let layer = CoreAnimationLayer::new();
        layer.set_device(&device);
        layer.set_pixel_format(MTLPixelFormat::BGRA8Unorm);
        layer.set_presents_with_transaction(false);

        unsafe {
            let view = window.contentView();
            view.setWantsBestResolutionOpenGLSurface_(YES);
            view.setWantsLayer(YES);
            view.setLayer(mem::transmute(layer.as_ref()));
        }

        let draw_size = glutin_window.get_inner_size().unwrap();
        layer.set_drawable_size(CGSize::new(draw_size.width as f64, draw_size.height as f64));

        let command_queue = device.new_command_queue();

        let mut pool = unsafe { NSAutoreleasePool::new(cocoa::base::nil) };
        
        self.shaders.compile_all_shaders(&device);

        while self.running {

            events_loop.poll_events(|event|{
                match event{
                    winit::Event::WindowEvent{ event, .. } => match event {
                        winit::WindowEvent::CloseRequested => self.running = false,
                        winit::WindowEvent::Resized(logical_size) => {
                            let dpi_factor = glutin_window.get_hidpi_factor();
                            let draw_size = logical_size.to_physical(dpi_factor);
                            layer.set_drawable_size(
                               CGSize::new(draw_size.width as f64, draw_size.height as f64));
                        },
                        _ => ()
                    },
                    _ => ()
                }
            });
            
            callback(self, Ev::Redraw);

            if let Some(drawable) = layer.next_drawable() {
                let render_pass_descriptor = RenderPassDescriptor::new();
                let _a = prepare_render_pass_descriptor(&render_pass_descriptor, drawable.texture());

                let command_buffer = command_queue.new_command_buffer();
 
                render_pass_descriptor.color_attachments().object_at(0).unwrap().set_load_action(MTLLoadAction::Clear);

                let parallel_encoder = command_buffer.new_parallel_render_command_encoder(&render_pass_descriptor);
                let encoder = parallel_encoder.render_command_encoder();

                // ok now we should call our render thing
                self.exec_draw_list(0, &device, encoder);

                encoder.end_encoding();
                parallel_encoder.end_encoding();

                command_buffer.present_drawable(&drawable);
                command_buffer.commit();

                unsafe { 
                    msg_send![pool, release];
                    pool = NSAutoreleasePool::new(cocoa::base::nil);
                }
            }
        }
    }
  
}
