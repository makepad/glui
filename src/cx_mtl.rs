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

fn prepare_pipeline_state<'a>(device: &DeviceRef, library: &LibraryRef) -> RenderPipelineState {
    let vert = library.get_function("triangle_vertex", None).unwrap();
    let frag = library.get_function("triangle_fragment", None).unwrap();

    let pipeline_state_descriptor = RenderPipelineDescriptor::new();
    pipeline_state_descriptor.set_vertex_function(Some(&vert));
    pipeline_state_descriptor.set_fragment_function(Some(&frag));
    pipeline_state_descriptor.color_attachments().object_at(0).unwrap().set_pixel_format(MTLPixelFormat::BGRA8Unorm);

    device.new_render_pipeline_state(&pipeline_state_descriptor).unwrap()
}

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

    pub fn exec_draw_list(&mut self, id: usize){
        /*
        // tad ugly otherwise the borrow checker locks 'self' and we can't recur
        for ci in 0..self.drawing.draw_lists[id].draws_len{
            let sub_list_id = self.drawing.draw_lists[id].draws[ci].sub_list_id;
            if sub_list_id != 0{
                self.exec_draw_list(sub_list_id);
            }
            else{
                let draw_list = &self.drawing.draw_lists[id];
                let draw = &draw_list.draws[ci];
                if draw.update_frame_id == self.drawing.frame_id{
                    // update the instance buffer data
                    unsafe{
                        gl::BindBuffer(gl::ARRAY_BUFFER, draw.vao.vb);
                        gl::BufferData(gl::ARRAY_BUFFER,
                                        (draw.instance.len() * mem::size_of::<f32>()) as gl::types::GLsizeiptr,
                                        draw.instance.as_ptr() as *const _, gl::STATIC_DRAW);
                    }
                }

                let sh = &self.shaders.shaders[draw.shader_id];
                let shgl = &self.shaders.glshaders[draw.shader_id];

                unsafe{
                    gl::UseProgram(shgl.program);
                    gl::BindVertexArray(draw.vao.vao);
                    let instances = draw.instance.len() / shgl.assembled_shader.instance_slots;
                    let indices = sh.geometry_indices.len();
                    CxShaders::set_uniform_buffer_fallback(&shgl.uniforms_cx, &self.uniforms);
                    CxShaders::set_uniform_buffer_fallback(&shgl.uniforms_dl, &draw_list.uniforms);
                    CxShaders::set_uniform_buffer_fallback(&shgl.uniforms_dr, &draw.uniforms);
                    CxShaders::set_samplers(&shgl.samplers, &draw.samplers, &self.textures);
                    gl::DrawElementsInstanced(gl::TRIANGLES, indices as i32, gl::UNSIGNED_INT, ptr::null(), instances as i32);
                }
            }
        }*/
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

        let library = device.new_library_with_file("default.metallib").unwrap();
        let pipeline_state = prepare_pipeline_state(&device, &library);
        let command_queue = device.new_command_queue();
        //let nc: () = msg_send![command_queue.0, setExecutionEnabled:true];

        let vbuf = {
            let vertex_data = [
                0.0f32,  0.5, 1.0, 0.0, 0.0,
                -0.5, -0.5, 0.0, 1.0, 0.0,
                0.5,  0.5, 0.0, 0.0, 1.0,
            ];

            device.new_buffer_with_data(
                unsafe { mem::transmute(vertex_data.as_ptr()) },
                (vertex_data.len() * mem::size_of::<f32>()) as u64,
                MTLResourceOptions::CPUCacheModeDefaultCache)
        };

        let mut pool = unsafe { NSAutoreleasePool::new(cocoa::base::nil) };
        let mut r = 0.0f32;
        
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
            //callback(self, Ev::Redraw);

            if let Some(drawable) = layer.next_drawable() {
                let render_pass_descriptor = RenderPassDescriptor::new();
                let _a = prepare_render_pass_descriptor(&render_pass_descriptor, drawable.texture());

                let command_buffer = command_queue.new_command_buffer();
                let parallel_encoder = command_buffer.new_parallel_render_command_encoder(&render_pass_descriptor);
                let encoder = parallel_encoder.render_command_encoder();
                encoder.set_render_pipeline_state(&pipeline_state);
                encoder.set_vertex_buffer(0, Some(&vbuf), 0);
                encoder.draw_primitives(MTLPrimitiveType::Triangle, 0, 3);
                encoder.end_encoding();
                parallel_encoder.end_encoding();

                render_pass_descriptor.color_attachments().object_at(0).unwrap().set_load_action(MTLLoadAction::DontCare);

                let parallel_encoder = command_buffer.new_parallel_render_command_encoder(&render_pass_descriptor);
                let encoder = parallel_encoder.render_command_encoder();
                use std::mem;
                let p = vbuf.contents();
                let vertex_data: &[u8; 60] = unsafe { mem::transmute(&[
                    0.0f32,  0.5, 1.0, 0.0-r, 0.0,
                    -0.5, -0.5, 0.0, 1.0-r, 0.0,
                    0.5,  0.5, 0.0, 0.0, 1.0+r,
                ]) };
                use std::ptr;

                unsafe {
                    ptr::copy(vertex_data.as_ptr(), p as *mut u8, (vertex_data.len() * mem::size_of::<f32>()) as usize);
                }
                vbuf.did_modify_range(NSRange::new(0 as u64, (vertex_data.len() * mem::size_of::<f32>()) as u64));


                encoder.set_render_pipeline_state(&pipeline_state);
                encoder.set_vertex_buffer(0, Some(&vbuf), 0);
                encoder.draw_primitives(MTLPrimitiveType::Triangle, 0, 3);
                encoder.end_encoding();
                parallel_encoder.end_encoding();

                command_buffer.present_drawable(&drawable);
                command_buffer.commit();

                r += 0.01f32;
                //let _: () = msg_send![command_queue.0, _submitAvailableCommandBuffers];
                unsafe { 
                    msg_send![pool, release];
                    pool = NSAutoreleasePool::new(cocoa::base::nil);
                }
            }
        }
    }
    /*
        let gl_request = GlRequest::Latest;
        let gl_profile = GlProfile::Core;

        let mut events_loop = glutin::EventsLoop::new();
        let window = glutin::WindowBuilder::new()
            .with_title(self.title.clone())
            .with_dimensions(LogicalSize::new(640.0, 480.0));
        let context = glutin::ContextBuilder::new()
            .with_vsync(true)
            .with_gl(gl_request)
            .with_gl_profile(gl_profile);
        let gl_window = glutin::GlWindow::new(window, context, &events_loop).unwrap();

        unsafe {
            gl_window.make_current().unwrap();
            gl::load_with(|symbol| gl_window.get_proc_address(symbol) as *const _);
            gl::ClearColor(0.3, 0.3, 0.3, 1.0);
            gl::Enable(gl::DEPTH_TEST);
            gl::DepthFunc(gl::LESS);

            //let mut num_extensions = 0;
            //gl::GetIntegerv(gl::NUM_EXTENSIONS, &mut num_extensions);
            //let extensions: Vec<_> = (0 .. num_extensions).map(|num| {
            //   Cx::gl_string(gl::GetStringi(gl::EXTENSIONS, num as gl::types::GLuint))
            //}).collect();
            //println!("Extensions   : {}", extensions.join(", "))

        }

        // lets compile all shaders
        self.shaders.compile_all_shaders();

        while self.running == true{
            events_loop.poll_events(|event|{
                match event{
                    glutin::Event::WindowEvent{ event, .. } => match event {
                        glutin::WindowEvent::CloseRequested => self.running = false,
                        glutin::WindowEvent::Resized(logical_size) => {
                            let dpi_factor = gl_window.get_hidpi_factor();
                            gl_window.resize(logical_size.to_physical(dpi_factor));
                            // lets resize the fractal framebuffer
                        },
                        _ => ()
                    },
                    _ => ()
                }
            });
            callback(self, Ev::Redraw);
            
            unsafe{
                gl::Clear(gl::COLOR_BUFFER_BIT|gl::DEPTH_BUFFER_BIT);
            }

            // lets paint the drawcommand tree
            self.exec_draw_list(0);

            gl_window.swap_buffers().unwrap();
        }
    }*/

}
