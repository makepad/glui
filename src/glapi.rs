use glutin::dpi::*;
use glutin::GlContext;

use crate::shader::*;
use crate::context::*;

#[derive(Default,Clone)]
pub struct GLApi{
    pub title:String,
    pub running:bool
}

impl GLApi{
    pub fn new(title:&str)->GLApi{
        return GLApi{
            title:title.to_string(),
            running:true
        }
    }

    pub fn compile_shader(&mut self, sh:&Shader){

    }

    pub fn event_loop<F>(cx: &mut Cx, callback:F)
    where F: FnMut(&mut Cx, Ev)
    { 
        let mut events_loop = glutin::EventsLoop::new();
        let window = glutin::WindowBuilder::new()
            .with_title(cx.glapi.title.clone())
            .with_dimensions(LogicalSize::new(640.0, 480.0));
        let context = glutin::ContextBuilder::new()
            .with_vsync(true);
        let gl_window = glutin::GlWindow::new(window, context, &events_loop).unwrap();
        
        unsafe {
            gl_window.make_current().unwrap();
            gl::load_with(|symbol| gl_window.get_proc_address(symbol) as *const _);
        }

        let mut running = true;
        while running == true{
            events_loop.poll_events(|event|{
                match event{
                    glutin::Event::WindowEvent{ event, .. } => match event {
                        glutin::WindowEvent::CloseRequested => running = false,
                        glutin::WindowEvent::Resized(logical_size) => {
                            let dpi_factor = gl_window.get_hidpi_factor();
                            gl_window.resize(logical_size.to_physical(dpi_factor));
                            // lets resize the fractal framebuffer

                        },
                        _ => ()
                    },
                    _ => ()
                }
            })
        }
    }
}