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
pub struct GLShaderUniform{
    loc:gl::types::GLuint,
    name:String
}

#[derive(Default,Clone)]
pub struct GLShader{
    program: gl::types::GLuint,
    geom_attribs: Vec<GLShaderAttrib>,
    inst_attribs: Vec<GLShaderAttrib>,
    uniforms: Vec<GLShaderUniform>,
    geom_vb: gl::types::GLuint,
    geom_ib: gl::types::GLuint,
    csh: CompiledShader
}

#[derive(Default,Clone)]
pub struct Cx{
    pub title:String,
    pub running:bool,
    pub shaders: Vec<Shader>,
    pub draw_lists: Vec<DrawList>,
    pub draw_lists_free: Vec<usize>,
    pub draw_cx_stack: Vec<DrawCx>,
    pub draw_list_id: usize,
    pub frame_id: usize,
    pub shaders_gl: Vec<GLShader>,
    pub uniforms:CxUniforms
}

#[derive(Default,Clone)]
pub struct DrawCx{ // draw info per UI element
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

pub struct InstanceWriter{
    draw_id:usize,
    pub uniforms:bool
}

#[derive(Default,Clone)]
pub struct GLInstanceVAO{
    vao:gl::types::GLuint,
    inst_vb:gl::types::GLuint
}

#[derive(Default,Clone)]
pub struct Draw{
    sub_list_id:usize, // if not 0, its a subnode
    shader_id:usize, // if shader_id changed, delete gl vao
    instance:Vec<f32>,
    uniforms:Vec<f32>,  // drawcmd uniforms
    update_frame_id: usize,
    vao:GLInstanceVAO,
}


#[derive(Default,Clone)]
pub struct DrawList{
    pub draws:Vec<Draw>,
    pub draws_len: usize,
    pub uniforms:DrawListUniforms // cmdlist uniforms
}

pub enum Ev{
    Redraw,
    Animate,
    FingerMove{x:f32, y:f32},
    FingerDown{x:f32, y:f32},
    FingerUp{x:f32, y:f32},
}

impl InstanceWriter{
    pub fn float(&mut self, cx: &mut Cx, _name: &str, v:f32){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.instance.push(v);
    }
    pub fn vec2f(&mut self, cx: &mut Cx, _name: &str, x:f32, y:f32){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.instance.push(x);
        draw_cmd.instance.push(y);
    }
    pub fn vec3f(&mut self, cx: &mut Cx, _name: &str, x:f32, y:f32, z:f32){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.instance.push(x);
        draw_cmd.instance.push(y);
        draw_cmd.instance.push(z);
    }
    pub fn vec4f(&mut self, cx: &mut Cx, _name: &str, x:f32, y:f32, z:f32, w:f32){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.instance.push(x);
        draw_cmd.instance.push(y);
        draw_cmd.instance.push(z);
        draw_cmd.instance.push(w);
    }
    pub fn vec2(&mut self, cx: &mut Cx, _name: &str, v:&Vec2){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.instance.push(v.x);
        draw_cmd.instance.push(v.y);
    }
    pub fn vec3(&mut self, cx: &mut Cx, _name: &str, v:&Vec3){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.instance.push(v.x);
        draw_cmd.instance.push(v.y);
        draw_cmd.instance.push(v.z);
    }
    pub fn vec4(&mut self, cx: &mut Cx, _name: &str, v:&Vec4){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.instance.push(v.x);
        draw_cmd.instance.push(v.y);
        draw_cmd.instance.push(v.z);
        draw_cmd.instance.push(v.w);
    }
    pub fn ufloat(&mut self, cx: &mut Cx, _name: &str, v:f32){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.uniforms.push(v);
    }
    pub fn uvec2f(&mut self, cx: &mut Cx, _name: &str, x:f32, y:f32){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.uniforms.push(x);
        draw_cmd.uniforms.push(y);
    }
    pub fn uvec3f(&mut self, cx: &mut Cx, _name: &str, x:f32, y:f32, z:f32){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.uniforms.push(x);
        draw_cmd.uniforms.push(y);
        draw_cmd.uniforms.push(z);
    }
    pub fn uvec4f(&mut self, cx: &mut Cx, _name: &str, x:f32, y:f32, z:f32, w:f32){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.uniforms.push(x);
        draw_cmd.uniforms.push(y);
        draw_cmd.uniforms.push(z);
        draw_cmd.uniforms.push(w);
    }
    pub fn uvec2(&mut self, cx: &mut Cx, _name: &str, v:&Vec2){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.uniforms.push(v.x);
        draw_cmd.uniforms.push(v.y);
    }
    pub fn uvec3(&mut self, cx: &mut Cx, _name: &str, v:&Vec3){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.uniforms.push(v.x);
        draw_cmd.uniforms.push(v.y);
        draw_cmd.uniforms.push(v.z);
    }
    pub fn uvec4(&mut self, cx: &mut Cx, _name: &str, v:&Vec4){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        draw_cmd.uniforms.push(v.x);
        draw_cmd.uniforms.push(v.y);
        draw_cmd.uniforms.push(v.z);
        draw_cmd.uniforms.push(v.w);
    }
    pub fn umat4(&mut self, cx: &mut Cx, _name: &str, v:&Mat4){
        let draw_list = &mut cx.draw_lists[cx.draw_list_id];
        let draw_cmd = &mut draw_list.draws[self.draw_id ];
        for i in 0..16{
            draw_cmd.uniforms.push(v.v[i]);
        }
    }
}

impl Drop for InstanceWriter{
    fn drop(&mut self){
        // verify if we have written the complete instance propset
        // and not too much/too little
    }
}

#[derive(Default,Clone)]
pub struct DrawListUniforms{
    pub prop2:f32
}

#[derive(Default,Clone)]
pub struct CxUniforms{
    prop1:f32
}

impl Cx{


    pub fn def_shader(&mut self, sh:&mut Shader){
        Shader::def_df(sh);
        Shader::def_constants(sh);
        // ok so cant we make uniforms a struct?
        sh.list_uniform("prop2", Kind::Float);
        sh.cx_uniform("prop1", Kind::Float);
    }

    pub fn set_uniforms(&mut self, draw_list_id:usize, draw_id:usize){
        let draw_list = &mut self.draw_lists[draw_list_id];
        let draw_cmd = &mut draw_list.draws[draw_id];
        // first we set the CxUniforms
        // then the drawcmdlistUniforms
        // then the DrawCmd uniforms
    }

    pub fn add_shader(&mut self, sh:&Shader)->usize{
        let id = self.shaders.len();
        // lets compile this sh
        self.shaders.push(sh.clone());
        id
    }

    pub fn add_draw_list(&mut self)->usize{
        let id = self.draw_lists.len();
        self.draw_lists.push(DrawList{..Default::default()});
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

            // lets fetch the uniform positions for our uniforms


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
        let draw_list = &mut self.draw_lists[self.draw_list_id];
        
        // find our drawcall in the filled draws
        for i in (0..draw_list.draws_len).rev(){
            let dc = &draw_list.draws[i];
            if dc.shader_id == inst_shader_id{
                // reuse this drawcmd.
                return InstanceWriter{draw_id:i, uniforms:false}
            }
        }  

        // we need a new draw_cmd
        let id = draw_list.draws_len;
        draw_list.draws_len = draw_list.draws_len + 1;
        let shgl = &self.shaders_gl[inst_shader_id];
        
        // see if we need to add a new one
        if draw_list.draws_len > draw_list.draws.len(){
            draw_list.draws.push(Draw{
                sub_list_id:0,
                shader_id:inst_shader_id,
                instance:Vec::new(),
                uniforms:Vec::new(),
                update_frame_id:self.frame_id,
                vao:Cx::create_vao(shgl)
            });
            return InstanceWriter{draw_id:id, uniforms:true}
        }

        // reuse a sub list node
        let draw_cmd = &mut draw_list.draws[id];
        // we used to be a sublist, construct vao
        if draw_cmd.sub_list_id != 0{ 
            draw_cmd.shader_id = inst_shader_id;
            draw_cmd.vao = Cx::create_vao(shgl);
        }
        // used to be another shader, destroy/construct vao
        else if draw_cmd.shader_id != inst_shader_id{ 
            Cx::destroy_vao(&mut draw_cmd.vao);
            draw_cmd.vao = Cx::create_vao(shgl);
            draw_cmd.shader_id = inst_shader_id;
        }
        // truncate buffers and set update frame
        draw_cmd.instance.truncate(0);
        draw_cmd.uniforms.truncate(0);
        draw_cmd.update_frame_id = self.frame_id;
        InstanceWriter{draw_id:id, uniforms:true}
    }

    fn exec_draw_list(&mut self, id: usize){
        // tad ugly otherwise the borrow checker locks 'self'
        for ci in 0..self.draw_lists[id].draws_len{
            let sub_list_id = self.draw_lists[id].draws[ci].sub_list_id;
            if sub_list_id != 0{
                self.exec_draw_list(sub_list_id);
            }
            else{
                let draw_cmd = &self.draw_lists[id].draws[ci];
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

                    // set up the global uniforms (once per program)

                    // set up the list uniforms (once per program per list)

                    // set up the draw uniforms (always)
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
            self.exec_draw_list(0);

            gl_window.swap_buffers().unwrap();
        }
    }

}

pub trait Style{
    fn style(cx:&mut Cx) -> Self;
}

impl DrawCx{
    pub fn begin(&mut self, cx:&mut Cx){
        if !self.initialized{ // draw node needs initialization
            if cx.draw_lists_free.len() != 0{
                self.draw_list_id = cx.draw_lists_free.pop().unwrap();
            }
            else{
                self.draw_list_id = cx.draw_lists.len();
                cx.draw_lists.push(DrawList{..Default::default()});
            }
            self.initialized = true;
        }
        else{
            // set len to 0
            let draw_list = &mut cx.draw_lists[self.draw_list_id];
            draw_list.draws_len = 0;
        }
        // push ourselves up the parent draw_stack
        if let Some(draw) = cx.draw_cx_stack.last(){

            // we need a new draw_cmd
            let draw_list = &mut cx.draw_lists[draw.draw_list_id];

            let id = draw_list.draws_len;
            draw_list.draws_len = draw_list.draws_len + 1;
            
            // see if we need to add a new one
            if draw_list.draws_len > draw_list.draws.len(){
                draw_list.draws.push({
                    Draw{
                        sub_list_id:self.draw_list_id,
                        ..Default::default()
                    }
                })
            }
            else{// or reuse a sub list node
                let draw_cmd = &mut draw_list.draws[id];
                if draw_cmd.sub_list_id == 0{ // we used to be a drawcmd
                    Cx::destroy_vao(&mut draw_cmd.vao);
                    draw_cmd.sub_list_id = self.draw_list_id;
                }
                else{ // used to be a sublist
                    draw_cmd.sub_list_id = self.draw_list_id;
                }
            }
        }

        cx.draw_list_id = self.draw_list_id;
        cx.draw_cx_stack.push(self.clone());
    }

    pub fn end(&mut self, cx:&mut Cx){
        cx.draw_cx_stack.pop();
    }
}
