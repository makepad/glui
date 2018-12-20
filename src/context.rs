use glutin::dpi::*;
use glutin::GlContext;
use std::mem;
use std::ptr;

use crate::shader::*;
use crate::math::*;

#[derive(Default,Clone)]
pub struct GLShaderAttrib{
    loc:gl::types::GLuint,
    size:gl::types::GLsizei,
    offset:gl::types::GLsizei,
    stride:gl::types::GLsizei
}

#[derive(Default,Clone)]
pub struct GLShader{
    program: gl::types::GLuint,
    geom_attribs: Vec<GLShaderAttrib>,
    inst_attribs: Vec<GLShaderAttrib>,
    geom_vb: gl::types::GLuint,
    geom_ib: gl::types::GLuint,
    csh: CompiledShader
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
        let draw_cmd = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ];
        draw_cmd.instance.push(v);
    }
    pub fn vec2f(&mut self, cx: &mut Cx, x:f32, y:f32){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        let draw_cmd = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ];
        draw_cmd.instance.push(x);
        draw_cmd.instance.push(y);
    }
    pub fn vec3f(&mut self, cx: &mut Cx, x:f32, y:f32, z:f32){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        let draw_cmd = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ];
        draw_cmd.instance.push(x);
        draw_cmd.instance.push(y);
        draw_cmd.instance.push(z);
    }
    pub fn vec4f(&mut self, cx: &mut Cx, x:f32, y:f32, z:f32, w:f32){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        let draw_cmd = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ];
        draw_cmd.instance.push(x);
        draw_cmd.instance.push(y);
        draw_cmd.instance.push(z);
        draw_cmd.instance.push(w);
    }
    pub fn vec2(&mut self, cx: &mut Cx, v:&Vec2){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        let draw_cmd = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ];
        draw_cmd.instance.push(v.x);
        draw_cmd.instance.push(v.y);
    }
    pub fn vec3(&mut self, cx: &mut Cx, v:&Vec3){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        let draw_cmd = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ];
        draw_cmd.instance.push(v.x);
        draw_cmd.instance.push(v.y);
        draw_cmd.instance.push(v.z);
    }
    pub fn vec4(&mut self, cx: &mut Cx, v:&Vec4){
        let draw_cmd_list = &mut cx.draw_cmd_lists[cx.draw_cmd_list_id];
        let draw_cmd = &mut draw_cmd_list.draw_cmds[self.draw_cmd_id ];
        draw_cmd.instance.push(v.x);
        draw_cmd.instance.push(v.y);
        draw_cmd.instance.push(v.z);
        draw_cmd.instance.push(v.w);
    }
}

impl Drop for InstanceWriter{
    fn drop(&mut self){
        // verify if we have written the complete instance propset
        // and not too much/too little
    }
}

#[derive(Default,Clone)]
pub struct GLInstanceVAO{
    vao:gl::types::GLuint,
    inst_vb:gl::types::GLuint
}

#[derive(Default,Clone)]
pub struct DrawCmd{
    sub_list_id:usize, // if not 0, its a subnode
    shader_id:usize, // if shader_id changed, delete gl vao
    instance:Vec<f32>,
    update_frame_id: usize,
    vao:GLInstanceVAO,
}

#[derive(Default,Clone)]
pub struct DrawCmdList{
    pub draw_cmds:Vec<DrawCmd>,
    pub draw_cmds_len: usize
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

    fn get_attribs(prefix:&str, slots:usize, num_attr:usize, program:gl::types::GLuint)->Vec<GLShaderAttrib>{
        let mut attribs = Vec::new();
        let stride = (slots * mem::size_of::<f32>()) as gl::types::GLsizei;
        for i in 0..num_attr{
            let mut name = prefix.to_string();
            name.push_str(&i.to_string());
            name.push_str("\0");
            
            let mut size = ((slots - i*4)) as gl::types::GLsizei;
            if size > 4{
                size = 4;
            }
            unsafe{
                attribs.push(
                    GLShaderAttrib{
                        loc: gl::GetAttribLocation(program, name.as_ptr() as *const _) as gl::types::GLuint,
                        offset: (i * 4 * mem::size_of::<f32>()) as i32,
                        size:  size,
                        stride: stride
                    }
                )
            }
        }
        attribs
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

            let geom_attribs = Cx::get_attribs("geomattr", csh.geom_slots, csh.geom_attribs, program);
            let inst_attribs = Cx::get_attribs("instattr", csh.inst_slots, csh.inst_attribs, program);

            // lets create static geom and index buffers for this shader
            let mut geom_vb = mem::uninitialized();
            gl::GenBuffers(1, &mut geom_vb);
            gl::BindBuffer(gl::ARRAY_BUFFER, geom_vb);
            gl::BufferData(gl::ARRAY_BUFFER,
                            (sh.geometry_vertices.len() * mem::size_of::<f32>()) as gl::types::GLsizeiptr,
                            sh.geometry_vertices.as_ptr() as *const _, gl::STATIC_DRAW);

            let mut geom_ib = mem::uninitialized();
            gl::GenBuffers(1, &mut geom_ib);
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, geom_ib);
            gl::BufferData(gl::ELEMENT_ARRAY_BUFFER,
                            (sh.geometry_indices.len() * mem::size_of::<u32>()) as gl::types::GLsizeiptr,
                            sh.geometry_indices.as_ptr() as *const _, gl::STATIC_DRAW);

            return Some(GLShader{
                program:program,
                geom_attribs:geom_attribs,
                inst_attribs:inst_attribs,
                geom_vb:geom_vb,
                geom_ib:geom_ib,
                csh:csh,
                ..Default::default()
            })
        }
    }

    fn create_vao(shgl:&GLShader)->GLInstanceVAO{
        // create the VAO
        let mut vao;
        let mut inst_vb;
        unsafe{
            vao = mem::uninitialized();
            gl::GenVertexArrays(1, &mut vao);
            gl::BindVertexArray(vao);
            
            // bind the vertex and indexbuffers
            gl::BindBuffer(gl::ARRAY_BUFFER, shgl.geom_vb);
            for attr in &shgl.geom_attribs{
                gl::VertexAttribPointer(attr.loc, attr.size, gl::FLOAT, 0, attr.stride, attr.offset as *const () as *const _);
                gl::EnableVertexAttribArray(attr.loc);
            }

            // create and bind the instance buffer
            inst_vb = mem::uninitialized();
            gl::GenBuffers(1, &mut inst_vb);
            gl::BindBuffer(gl::ARRAY_BUFFER, inst_vb);
            
            for attr in &shgl.inst_attribs{
                gl::VertexAttribPointer(attr.loc, attr.size, gl::FLOAT, 0, attr.stride, attr.offset as *const () as *const _);
                gl::EnableVertexAttribArray(attr.loc);
                gl::VertexAttribDivisor(attr.loc, 1 as gl::types::GLuint);
            }

            // bind the indexbuffer
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, shgl.geom_ib);
            gl::BindVertexArray(0);
        }
        GLInstanceVAO{
            vao:vao,
            inst_vb:inst_vb
        }
    }

    fn destroy_vao(glivao:&mut GLInstanceVAO){
        unsafe{
            gl::DeleteVertexArrays(1, &mut glivao.vao);
            gl::DeleteBuffers(1, &mut glivao.inst_vb);
        }
    }

    pub fn instance(&mut self, inst_shader_id:usize)->InstanceWriter{
        //let sh = &mut self.shaders[inst_shader_id];
        let draw_cmd_list = &mut self.draw_cmd_lists[self.draw_cmd_list_id];
        
        // find our drawcall in the filled draw_cmds
        for i in (0..draw_cmd_list.draw_cmds_len).rev(){
            let dc = &draw_cmd_list.draw_cmds[i];
            if dc.shader_id == inst_shader_id{
                // reuse this drawcmd.
                return InstanceWriter{draw_cmd_id:i}
            }
        }  

        // we need a new draw_cmd
        let id = draw_cmd_list.draw_cmds_len;
        draw_cmd_list.draw_cmds_len = draw_cmd_list.draw_cmds_len + 1;
        let shgl = &self.shaders_gl[inst_shader_id];
        
        // see if we need to add a new one
        if draw_cmd_list.draw_cmds_len > draw_cmd_list.draw_cmds.len(){
            draw_cmd_list.draw_cmds.push(DrawCmd{
                sub_list_id:0,
                shader_id:inst_shader_id,
                instance:Vec::new(),
                update_frame_id:self.frame_id,
                vao:Cx::create_vao(shgl)
            });
            return InstanceWriter{draw_cmd_id:id}
        }

        // reuse a sub list node
        let draw_cmd = &mut draw_cmd_list.draw_cmds[id];
        if draw_cmd.sub_list_id != 0{ // we used to be a sublist
            draw_cmd.shader_id = inst_shader_id;
            draw_cmd.instance.truncate(0);
            draw_cmd.update_frame_id = self.frame_id;
            draw_cmd.vao = Cx::create_vao(shgl);
            return InstanceWriter{draw_cmd_id:id}
        }
 
        // re use another shader
        if draw_cmd.shader_id != inst_shader_id{
            Cx::destroy_vao(&mut draw_cmd.vao);
            draw_cmd.shader_id = inst_shader_id;
            draw_cmd.instance.truncate(0);
            draw_cmd.update_frame_id = self.frame_id;
            draw_cmd.vao = Cx::create_vao(shgl);
            return InstanceWriter{draw_cmd_id:id}
        }

        // we are the same shader, so just truncate instance and go
        draw_cmd.instance.truncate(0);
        draw_cmd.update_frame_id = self.frame_id;
        
        InstanceWriter{draw_cmd_id:id}
    }

    fn exec_draw_cmd_list(&mut self, id: usize){
        // tad ugly otherwise the borrow checker locks 'self'
        for ci in 0..self.draw_cmd_lists[id].draw_cmds_len{
            let sub_list_id = self.draw_cmd_lists[id].draw_cmds[ci].sub_list_id;
            if sub_list_id != 0{
                self.exec_draw_cmd_list(sub_list_id);
            }
            else{
                let draw_cmd = &self.draw_cmd_lists[id].draw_cmds[ci];
                if draw_cmd.update_frame_id == self.frame_id{
                    // update the instance buffer data
                    unsafe{
                        gl::BindBuffer(gl::ARRAY_BUFFER, draw_cmd.vao.inst_vb);
                        gl::BufferData(gl::ARRAY_BUFFER,
                                        (draw_cmd.instance.len() * mem::size_of::<f32>()) as gl::types::GLsizeiptr,
                                        draw_cmd.instance.as_ptr() as *const _, gl::STATIC_DRAW);
                    }
                }

                let sh = &self.shaders[draw_cmd.shader_id];
                let shgl = &self.shaders_gl[draw_cmd.shader_id];

                unsafe{
                    gl::UseProgram(shgl.program);
                    gl::BindVertexArray(draw_cmd.vao.vao);
                    let instances = draw_cmd.instance.len() / shgl.csh.inst_slots;
                    let indices = sh.geometry_indices.len();
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
            
            unsafe{
                gl::Clear(gl::COLOR_BUFFER_BIT|gl::DEPTH_BUFFER_BIT);
            }

            // lets paint the drawcommand tree
            self.exec_draw_cmd_list(0);

            gl_window.swap_buffers().unwrap();
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
        else{
            // set len to 0
            let draw_cmd_list = &mut cx.draw_cmd_lists[self.draw_cmd_list_id];
            draw_cmd_list.draw_cmds_len = 0;
        }
        // push ourselves up the parent draw_stack
        if let Some(draw) = cx.draw_stack.last(){

            // we need a new draw_cmd
            let draw_cmd_list = &mut cx.draw_cmd_lists[draw.draw_cmd_list_id];

            let id = draw_cmd_list.draw_cmds_len;
            draw_cmd_list.draw_cmds_len = draw_cmd_list.draw_cmds_len + 1;
            
            // see if we need to add a new one
            if draw_cmd_list.draw_cmds_len > draw_cmd_list.draw_cmds.len(){
                draw_cmd_list.draw_cmds.push({
                    DrawCmd{
                        sub_list_id:self.draw_cmd_list_id,
                        ..Default::default()
                    }
                })
            }
            else{// or reuse a sub list node
                let draw_cmd = &mut draw_cmd_list.draw_cmds[id];
                if draw_cmd.sub_list_id == 0{ // we used to be a drawcmd
                    Cx::destroy_vao(&mut draw_cmd.vao);
                    draw_cmd.sub_list_id = self.draw_cmd_list_id;
                }
                else{ // used to be a sublist
                    draw_cmd.sub_list_id = self.draw_cmd_list_id;
                }
            }
        }

        cx.draw_cmd_list_id = self.draw_cmd_list_id;
        cx.draw_stack.push(self.clone());
    }

    pub fn end(&mut self, cx:&mut Cx){
        cx.draw_stack.pop();
    }
}
