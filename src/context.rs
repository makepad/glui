use glutin::dpi::*;
use glutin::GlContext;

use crate::shader::*;

#[derive(Default,Clone)]
pub struct Cx{
    pub title:String,
    pub running:bool,
    pub shaders: Vec<Shader>,
    pub draw_cmd_lists: Vec<DrawCmdList>,
    pub draw_cmd_lists_free: Vec<usize>,
    pub draw_stack: Vec<Draw>,
    pub draw_cmd_list_id: usize,
    pub frame_id: usize
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

    pub fn compile_all_shaders(&mut self){
        for sh in &self.shaders{
            let csh = sh.compile();
            // now we have a pixel and a vertex shader
            // so lets now pass it to GL
            
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
