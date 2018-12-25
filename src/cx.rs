use glutin::dpi::*;
use glutin::GlContext;
use std::mem;
use std::ptr;

use crate::shader::*;
use crate::cxdrawing::*;
use crate::cxshaders::*;
use crate::cxfonts::*;
use crate::cxtextures::*;

#[derive(Clone)]
pub struct Cx{
    pub title:String,
    pub running:bool,

    pub shaders:CxShaders,
    pub drawing:CxDrawing,
    pub fonts:CxFonts,
    pub textures:CxTextures,

    pub uniforms:Vec<f32>
}

pub enum Ev{
    Redraw,
    Animate,
    FingerMove{x:f32, y:f32},
    FingerDown{x:f32, y:f32},
    FingerUp{x:f32, y:f32},
}

impl Default for Cx{
    fn default()->Self{
        let mut uniforms = Vec::<f32>::new();
        uniforms.resize(CX_UNI_SIZE, 0.0);
        Self{
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
        Shader::def_constants(sh);
        Cx::def_uniforms(sh);
        DrawList::def_uniforms(sh);
    }

    pub fn def_uniforms(sh: &mut Shader){
        sh.cx_uniform("prop1", Kind::Float);
    }

    pub fn uniform_prop1(&mut self, v:f32){
        self.uniforms[CX_UNI_PROP1] = v;
    }

    pub fn exec_draw_list(&mut self, id: usize){
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
                    let instances = draw.instance.len() / shgl.compiled_shader.inst_slots;
                    let indices = sh.geometry_indices.len();

                    CxShaders::set_uniform_buffer_fallback(&shgl.cx_uniforms, &self.uniforms);
                    CxShaders::set_uniform_buffer_fallback(&shgl.dl_uniforms, &draw_list.uniforms);
                    CxShaders::set_uniform_buffer_fallback(&shgl.dr_uniforms, &draw.uniforms);
                    CxShaders::set_samplers(&shgl.samplers, &draw.samplers, &self.textures);
                    gl::DrawElementsInstanced(gl::TRIANGLES, indices as i32, gl::UNSIGNED_INT, ptr::null(), instances as i32);
                }
            }
        }
    }

    pub fn event_loop<F>(&mut self, mut callback:F)
    where F: FnMut(&mut Cx, Ev),
    { 
        let mut events_loop = glutin::EventsLoop::new();
        let window = glutin::WindowBuilder::new()
            .with_title(self.title.clone())
            .with_dimensions(LogicalSize::new(640.0, 480.0));
        let context = glutin::ContextBuilder::new()
            .with_vsync(true);
        let gl_window = glutin::GlWindow::new(window, context, &events_loop).unwrap();
        
        unsafe {
            gl_window.make_current().unwrap();
            gl::load_with(|symbol| gl_window.get_proc_address(symbol) as *const _);
            gl::ClearColor(0.3, 0.3, 0.3, 1.0);
            gl::Enable(gl::DEPTH_TEST);
            gl::DepthFunc(gl::LESS);            
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
    }

}

pub trait Style{
    fn style(cx:&mut Cx) -> Self;
}

#[derive(Default,Clone)]
pub struct DrawNode{ // draw info per UI element
    pub id:usize,
    pub x:f32,
    pub y:f32,
    pub w:f32,
    pub h:f32,
    pub frame_id:usize,
    pub initialized:bool,
    // the set of shader_id + 
    pub draw_list_id:usize 
}

impl DrawNode{
    pub fn begin(&mut self, cx:&mut Cx){
        if !self.initialized{ // draw node needs initialization
            if cx.drawing.draw_lists_free.len() != 0{
                self.draw_list_id = cx.drawing.draw_lists_free.pop().unwrap();
            }
            else{
                self.draw_list_id = cx.drawing.draw_lists.len();
                cx.drawing.draw_lists.push(DrawList{..Default::default()});
            }
            self.initialized = true;
            let draw_list = &mut cx.drawing.draw_lists[self.draw_list_id];
            draw_list.initialize();
        }
        else{
            // set len to 0
            let draw_list = &mut cx.drawing.draw_lists[self.draw_list_id];
            draw_list.draws_len = 0;
        }
        // push ourselves up the parent draw_stack
        if let Some(parent_draw) = cx.drawing.draw_node_stack.last(){

            // we need a new draw
            let parent_draw_list = &mut cx.drawing.draw_lists[parent_draw.draw_list_id];

            let id = parent_draw_list.draws_len;
            parent_draw_list.draws_len = parent_draw_list.draws_len + 1;
            
            // see if we need to add a new one
            if parent_draw_list.draws_len > parent_draw_list.draws.len(){
                parent_draw_list.draws.push({
                    Draw{
                        sub_list_id:self.draw_list_id,
                        ..Default::default()
                    }
                })
            }
            else{// or reuse a sub list node
                let draw = &mut parent_draw_list.draws[id];
                if draw.sub_list_id == 0{ // we used to be a drawcmd
                    CxShaders::destroy_vao(&mut draw.vao);
                    draw.sub_list_id = self.draw_list_id;
                }
                else{ // used to be a sublist
                    draw.sub_list_id = self.draw_list_id;
                }
            }
        }

        cx.drawing.draw_list_id = self.draw_list_id;
        cx.drawing.draw_node_stack.push(self.clone());
    }

    pub fn end(&mut self, cx:&mut Cx){
        cx.drawing.draw_node_stack.pop();
    }
}