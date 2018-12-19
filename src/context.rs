use glutin::dpi::*;
use glutin::GlContext;
use std::mem;
use std::ptr;

use crate::shader::*;

#[derive(Default,Clone)]
pub struct GLShader{
    program: gl::types::GLuint
}

#[derive(Default,Clone)]
pub struct Cx{
    pub title:String,
    pub running:bool,
    pub shaders: Vec<Shader>,
    pub draw_cmd_lists: Vec<DrawCmdList>,
    pub draw_cmd_lists_free: Vec<usize>,
    pub draw_stack: Vec<Draw>,
    pub draw_cmd_list_id: usize,
    pub frame_id: usize,
    pub shaders_gl: Vec<GLShader>
}

pub struct InstanceWriter{
    draw_cmd_id:usize
}

impl InstanceWriter{
    pub fn float(&mut self, cx: &mut Cx, v:f32){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        if let DrawCmd::Instance{ shader_id:_, instance_data, .. } = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ] {
            instance_data.push(v);
        }
    }
    pub fn vec2(&mut self, cx: &mut Cx, x:f32, y:f32){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        if let DrawCmd::Instance{ shader_id:_, instance_data, .. } = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ] {
            instance_data.push(x);
            instance_data.push(y);
        }
    }
    pub fn vec3(&mut self, cx: &mut Cx, x:f32, y:f32, z:f32){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        if let DrawCmd::Instance{ shader_id:_, instance_data, .. } = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ] {
            instance_data.push(x);
            instance_data.push(y);
            instance_data.push(z);
        }
    }
    pub fn vec4(&mut self, cx: &mut Cx, x:f32, y:f32, z:f32, w:f32){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        if let DrawCmd::Instance{ shader_id:_, instance_data, .. } = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ] {
            instance_data.push(x);
            instance_data.push(y);
            instance_data.push(z);
            instance_data.push(w);
        }
    }
}

impl Drop for InstanceWriter{
    fn drop(&mut self){
        // verify if we have written the complete instance propset
        // and not too much/too little
    }
}

#[derive(Clone)]
pub enum DrawCmd{
    SubList{
        cmd_list_id:usize
    },
    Instance{
        shader_id:usize,
        instance_data:Vec<f32>,
        gl_buffer:i32
    }
}

#[derive(Default,Clone)]
pub struct DrawCmdList{
    pub draw_cmds:Vec<DrawCmd>
}

pub enum Ev{
    Redraw,
    Animate,
    FingerMove{x:f32, y:f32},
    FingerDown{x:f32, y:f32},
    FingerUp{x:f32, y:f32},
}

impl Cx{
    pub fn add_shader(&mut self, sh:&Shader)->usize{
        let id = self.shaders.len();
        // lets compile this sh
        self.shaders.push(sh.clone());
        id
    }

    pub fn add_draw_cmd_list(&mut self)->usize{
        let id = self.draw_cmd_lists.len();
        self.draw_cmd_lists.push(DrawCmdList{..Default::default()});
        id
    }

    pub fn new()->Cx{
        return Cx{..Default::default()}
    }

    fn has_shader_error(compile:bool, shader:gl::types::GLuint, source:&str)->Option<String>{
        unsafe{
            let mut success = i32::from(gl::FALSE);
           
            if compile{
                gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut success);
            }
            else{
                gl::GetProgramiv(shader, gl::LINK_STATUS, &mut success);
            };
           
            if success != i32::from(gl::TRUE) {
                 let mut info_log = Vec::<u8>::with_capacity(2048);
                info_log.set_len(2047);
                for i in 0..2047{
                    info_log[i] = 0;
                };
                if compile{
                    gl::GetShaderInfoLog(shader, 2048, ptr::null_mut(),
                        info_log.as_mut_ptr() as *mut gl::types::GLchar)
                }
                else{
                    gl::GetProgramInfoLog(shader, 2048, ptr::null_mut(),
                        info_log.as_mut_ptr() as *mut gl::types::GLchar)
                }
                let mut r = "".to_string();
                r.push_str(&String::from_utf8(info_log).unwrap());
                r.push_str("\n");
                let split = source.split("\n");
                for (line,chunk) in split.enumerate(){
                    r.push_str(&(line+1).to_string());
                    r.push_str(":");
                    r.push_str(chunk);
                    r.push_str("\n");
                }
                Some(r)
            }
            else{
                None
            }
        }
    }

    fn compile_gl_shader(sh:&Shader)->Option<GLShader>{
        let csh = sh.compile();
        // now we have a pixel and a vertex shader
        // so lets now pass it to GL
        unsafe{
            
            let vs = gl::CreateShader(gl::VERTEX_SHADER);
            gl::ShaderSource(vs, 1, [csh.vertex.as_ptr() as *const _].as_ptr(), ptr::null());
            gl::CompileShader(vs);
            if let Some(error) = Cx::has_shader_error(true, vs, &csh.vertex){
                println!(
                    "ERROR::SHADER::VERTEX::COMPILATION_FAILED\n{}",
                    error
                );
                return None
            }

            let fs = gl::CreateShader(gl::FRAGMENT_SHADER);
            gl::ShaderSource(fs, 1, [csh.fragment.as_ptr() as *const _].as_ptr(), ptr::null());
            gl::CompileShader(fs);
            if let Some(error) = Cx::has_shader_error(true, fs, &csh.fragment){
                println!(
                    "ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n{}",
                    error
                );
                return None
            }

            let program = gl::CreateProgram();
            gl::AttachShader(program, vs);
            gl::AttachShader(program, fs);
            gl::LinkProgram(program);
            if let Some(error) = Cx::has_shader_error(false, program, ""){
                println!(
                    "ERROR::SHADER::LINK::COMPILATION_FAILED\n{}",
                    error
                );
                return None
            }
            gl::DeleteShader(vs);
            gl::DeleteShader(fs);

            return Some(GLShader{
                program:program
            })
            // ok lets get the attributes
            // and the uniforms
        }
    }

    pub fn instance(&mut self, inst_shader_id:usize)->InstanceWriter{
        //let sh = &mut self.shaders[inst_shader_id];
        let draw_cmd_list = &mut self.draw_cmd_lists[self.draw_cmd_list_id];
        
        let index = draw_cmd_list.draw_cmds.iter().position(|dc| {
            if let DrawCmd::Instance { shader_id, .. } = dc{
                *shader_id == inst_shader_id
            } else {
                false
            }
        });

        let draw_cmd_id = if let Some(index) = index{
            index
        }
        else{
            let id = draw_cmd_list.draw_cmds.len();
            draw_cmd_list.draw_cmds.push(DrawCmd::Instance{
                shader_id:inst_shader_id,
                instance_data:Vec::new(),
                gl_buffer:0
            });
            id
        };
        
        // see if we need to compile shader
        InstanceWriter{draw_cmd_id:draw_cmd_id}
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
        }

        // lets compile all shaders
        for sh in &self.shaders{
            let glsh = Cx::compile_gl_shader(&sh);
            if let Some(glsh) = glsh{
                self.shaders_gl.push(glsh);
            }
            else{
                self.shaders_gl.push(
                    GLShader{..Default::default()}
                )
            }
        };

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
            // lets paint the drawcommand tree

        }
    }
}

pub trait Style{
    fn style(cx:&mut Cx) -> Self;
}

#[derive(Default,Clone)]
pub struct Draw{ // draw info per UI element
    pub id:usize,
    pub x:f32,
    pub y:f32,
    pub w:f32,
    pub h:f32,
    pub frame_id:usize,
    pub initialized:bool,
    // the set of shader_id + 
    pub draw_cmd_list_id:usize 
}

impl Draw{
    pub fn begin(&mut self, cx:&mut Cx){
        if !self.initialized{ // draw node needs initialization
            if cx.draw_cmd_lists_free.len() != 0{
                self.draw_cmd_list_id = cx.draw_cmd_lists_free.pop().unwrap();
            }
            else{
                self.draw_cmd_list_id = cx.draw_cmd_lists.len();
                cx.draw_cmd_lists.push(DrawCmdList{..Default::default()});
            }
            self.initialized = true;
        }
        cx.draw_cmd_list_id = self.draw_cmd_list_id;
        cx.draw_stack.push(self.clone());
    }

    pub fn end(&mut self, cx:&mut Cx){
        cx.draw_stack.pop();
    }
}
