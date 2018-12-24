use glutin::dpi::*;
use glutin::GlContext;
use std::mem;
use std::ptr;

use crate::shader::*;
use crate::math::*;
use crate::font::*;

#[derive(Default,Clone)]
pub struct GLAttribute{
    loc:gl::types::GLuint,
    size:gl::types::GLsizei,
    offset:gl::types::GLsizei,
    stride:gl::types::GLsizei
}

#[derive(Default,Clone)]
pub struct GLUniform{
    loc:gl::types::GLint,
    name:String,
    size:usize
}

#[derive(Default,Clone)]
pub struct GLShader{
    shader_id: usize,
    program: gl::types::GLuint,
    geom_attribs: Vec<GLAttribute>,
    inst_attribs: Vec<GLAttribute>,
    geom_vb: gl::types::GLuint,
    geom_ib: gl::types::GLuint,
    compiled_shader: CompiledShader,
    dr_uniforms: Vec<GLUniform>,
    dl_uniforms: Vec<GLUniform>,
    cx_uniforms: Vec<GLUniform>,
}

#[derive(Clone, Default)]
pub struct CxDrawing{
    pub draw_lists: Vec<DrawList>,
    pub draw_lists_free: Vec<usize>,
    pub draw_cx_stack: Vec<DrawCx>,
    pub draw_list_id: usize,
    pub frame_id: usize
}

impl CxDrawing{
    pub fn draw_list(&mut self)->&mut DrawList{
        &mut self.draw_lists[self.draw_list_id]
    }

    pub fn instance(&mut self, sh:&GLShader)->&mut Draw{
        let draw_list = &mut self.draw_lists[self.draw_list_id];
        
        // find our drawcall in the filled draws
        for i in (0..draw_list.draws_len).rev(){
            if draw_list.draws[i].shader_id == sh.shader_id{
                // reuse this drawcmd.
                let dc = &mut draw_list.draws[i];
                dc.first = false;
                return dc
            }
        }  

        // we need a new draw
        let id = draw_list.draws_len;
        draw_list.draws_len = draw_list.draws_len + 1;
        
        // see if we need to add a new one
        if draw_list.draws_len > draw_list.draws.len(){
            draw_list.draws.push(Draw{
                sub_list_id:0,
                shader_id:sh.shader_id,
                instance:Vec::new(),
                uniforms:Vec::new(),
                first:true,
                update_frame_id:self.frame_id,
                vao:CxShaders::create_vao(sh)
            });
            return &mut draw_list.draws[id];
        }

        // reuse a sub list node
        let draw = &mut draw_list.draws[id];
        // we used to be a sublist, construct vao
        if draw.sub_list_id != 0{ 
            draw.shader_id = sh.shader_id;
            draw.vao = CxShaders::create_vao(sh);
        }
        // used to be another shader, destroy/construct vao
        else if draw.shader_id != sh.shader_id{ 
            CxShaders::destroy_vao(&mut draw.vao);
            draw.vao = CxShaders::create_vao(sh);
            draw.shader_id = sh.shader_id;
        }
        // truncate buffers and set update frame
        draw.instance.truncate(0);
        draw.uniforms.truncate(0);
        draw.update_frame_id = self.frame_id;
        draw.first = true;
        draw
    }
}

#[derive(Clone, Default)]
pub struct CxFonts{
    pub fonts:Vec<Font>
}

impl CxFonts{
    pub fn get(&self, id:usize)->&Font{
        &self.fonts[id]
    }

    pub fn load(&mut self, file_name: &str)->usize{
        0
    }
}

#[derive(Clone, Default)]
pub struct CxShaders{
    pub glshaders: Vec<GLShader>,
    pub shaders: Vec<Shader>,
}

impl CxShaders{
    pub fn def(sh:&mut Shader){
        Shader::def_df(sh);
        Shader::def_constants(sh);
        Cx::def_uniforms(sh);
        DrawList::def_uniforms(sh);
    }

    pub fn get(&self, id:usize)->&GLShader{
        &self.glshaders[id]
    }

    pub fn add(&mut self, sh:Shader)->usize{
        let id = self.shaders.len();
        // lets compile this sh
        self.shaders.push(sh);
        id
    }

    pub fn compile_all_shaders(&mut self){
        for sh in &self.shaders{
            let glsh = CxShaders::compile_gl_shader(&sh);
            if let Some(glsh) = glsh{
                self.glshaders.push(glsh);
            }
            else{
                self.glshaders.push(
                    GLShader{..Default::default()}
                )
            }
        };
    }

    fn compile_has_shader_error(compile:bool, shader:gl::types::GLuint, source:&str)->Option<String>{
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

    fn compile_get_attributes(program:gl::types::GLuint, prefix:&str, slots:usize, num_attr:usize)->Vec<GLAttribute>{
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
                    GLAttribute{
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

    fn compile_get_uniforms(program:gl::types::GLuint, unis:&Vec<ShaderUniform>)->Vec<GLUniform>{
        let mut gl_uni = Vec::new();
        for uni in unis{
            let mut name0 = "".to_string();
            name0.push_str(&uni.name);
            name0.push_str("\0");
            unsafe{
                gl_uni.push(GLUniform{
                    loc:gl::GetUniformLocation(program, name0.as_ptr() as *const _),
                    name:uni.name.clone(),
                    size:uni.kind.slots()
                })
            }
        }
        gl_uni
    }

    fn compile_gl_shader(sh:&Shader)->Option<GLShader>{
        let csh = sh.compile();
        // now we have a pixel and a vertex shader
        // so lets now pass it to GL
        unsafe{
            
            let vs = gl::CreateShader(gl::VERTEX_SHADER);
            gl::ShaderSource(vs, 1, [csh.vertex.as_ptr() as *const _].as_ptr(), ptr::null());
            gl::CompileShader(vs);
            if let Some(error) = CxShaders::compile_has_shader_error(true, vs, &csh.vertex){
                println!(
                    "ERROR::SHADER::VERTEX::COMPILATION_FAILED\n{}",
                    error
                );
                return None
            }

            let fs = gl::CreateShader(gl::FRAGMENT_SHADER);
            gl::ShaderSource(fs, 1, [csh.fragment.as_ptr() as *const _].as_ptr(), ptr::null());
            gl::CompileShader(fs);
            if let Some(error) = CxShaders::compile_has_shader_error(true, fs, &csh.fragment){
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
            if let Some(error) = CxShaders::compile_has_shader_error(false, program, ""){
                println!(
                    "ERROR::SHADER::LINK::COMPILATION_FAILED\n{}",
                    error
                );
                return None
            }
            gl::DeleteShader(vs);
            gl::DeleteShader(fs);

            let geom_attribs = CxShaders::compile_get_attributes(program, "geomattr", csh.geom_slots, csh.geom_attribs);
            let inst_attribs = CxShaders::compile_get_attributes(program, "instattr", csh.inst_slots, csh.inst_attribs);

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
                compiled_shader:csh,
                cx_uniforms:CxShaders::compile_get_uniforms(program, &sh.cx_uniforms),
                dl_uniforms:CxShaders::compile_get_uniforms(program, &sh.dl_uniforms),
                dr_uniforms:CxShaders::compile_get_uniforms(program, &sh.dr_uniforms),
                ..Default::default()
            })
        }
    }

    fn create_vao(shgl:&GLShader)->GLInstanceVAO{
        // create the VAO
        let mut vao;
        let mut vb;
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
            vb = mem::uninitialized();
            gl::GenBuffers(1, &mut vb);
            gl::BindBuffer(gl::ARRAY_BUFFER, vb);
            
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
            vb:vb
        }
    }

    fn destroy_vao(glivao:&mut GLInstanceVAO){
        unsafe{
            gl::DeleteVertexArrays(1, &mut glivao.vao);
            gl::DeleteBuffers(1, &mut glivao.vb);
        }
    }

    fn set_uniform_buffer_fallback(locs:&Vec<GLUniform>, uni:&Vec<f32>){
        let mut o = 0;
        for loc in locs{
            if loc.loc >=0 {
                unsafe{
                    match loc.size{
                        1=>gl::Uniform1f(loc.loc, uni[o]),
                        2=>gl::Uniform2f(loc.loc, uni[o], uni[o+1]),
                        3=>gl::Uniform3f(loc.loc, uni[o], uni[o+1], uni[o+2]),
                        4=>gl::Uniform4f(loc.loc, uni[o], uni[o+1], uni[o+2], uni[o+3]),
                        16=>gl::UniformMatrix4fv(loc.loc, 1, 0, uni.as_ptr().offset((o*4) as isize)),
                        _=>()
                    }
                }
            };
            o = o + loc.size;
        }
    }
}

#[derive(Clone)]
pub struct Cx{
    pub title:String,
    pub running:bool,

    pub shaders:CxShaders,
    pub drawing:CxDrawing,
    pub fonts:CxFonts,

    pub uniforms:Vec<f32>
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

#[derive(Default,Clone)]
pub struct GLInstanceVAO{
    vao:gl::types::GLuint,
    vb:gl::types::GLuint
}

#[derive(Default,Clone)]
pub struct Draw{
    pub sub_list_id:usize, // if not 0, its a subnode
    pub shader_id:usize, // if shader_id changed, delete gl vao
    pub instance:Vec<f32>,
    pub uniforms:Vec<f32>,  // draw uniforms
    pub update_frame_id: usize,
    pub vao:GLInstanceVAO,
    pub first:bool
}

impl Draw{
    pub fn float(&mut self, _name: &str, v:f32){
        self.instance.push(v);
    }

    pub fn vec2f(&mut self, _name: &str, x:f32, y:f32){
        self.instance.push(x);
        self.instance.push(y);
    }

    pub fn vec3f(&mut self, _name: &str, x:f32, y:f32, z:f32){
        self.instance.push(x);
        self.instance.push(y);
        self.instance.push(z);
    }

    pub fn vec4f(&mut self, _name: &str, x:f32, y:f32, z:f32, w:f32){
        self.instance.push(x);
        self.instance.push(y);
        self.instance.push(z);
        self.instance.push(w);
    }

    pub fn vec2(&mut self, _name: &str, v:&Vec2){
        self.instance.push(v.x);
        self.instance.push(v.y);
    }

    pub fn vec3(&mut self, _name: &str, v:&Vec3){
        self.instance.push(v.x);
        self.instance.push(v.y);
        self.instance.push(v.z);
    }

    pub fn vec4(&mut self, _name: &str, v:&Vec4){
        self.instance.push(v.x);
        self.instance.push(v.y);
        self.instance.push(v.z);
        self.instance.push(v.w);
    }

    pub fn usampler2D(&mut self, _name: &str, texture_id: usize){
        
    }

    pub fn ufloat(&mut self, _name: &str, v:f32){
        self.uniforms.push(v);
    }

    pub fn uvec2f(&mut self, _name: &str, x:f32, y:f32){
        self.uniforms.push(x);
        self.uniforms.push(y);
    }

    pub fn uvec3f(&mut self, _name: &str, x:f32, y:f32, z:f32){
        self.uniforms.push(x);
        self.uniforms.push(y);
        self.uniforms.push(z);
    }

    pub fn uvec4f(&mut self, _name: &str, x:f32, y:f32, z:f32, w:f32){
        self.uniforms.push(x);
        self.uniforms.push(y);
        self.uniforms.push(z);
        self.uniforms.push(w);
    }

    pub fn uvec2(&mut self, _name: &str, v:&Vec2){
        self.uniforms.push(v.x);
        self.uniforms.push(v.y);
    }

    pub fn uvec3(&mut self, _name: &str, v:&Vec3){
        self.uniforms.push(v.x);
        self.uniforms.push(v.y);
        self.uniforms.push(v.z);
    }

    pub fn uvec4(&mut self, _name: &str, v:&Vec4){
        self.uniforms.push(v.x);
        self.uniforms.push(v.y);
        self.uniforms.push(v.z);
        self.uniforms.push(v.w);
    }

    pub fn umat4(&mut self, _name: &str, v:&Mat4){
        for i in 0..16{
            self.uniforms.push(v.v[i]);
        }
    }
}

// CX and DL uniforms
const DL_UNI_PROP2:usize = 0;
const DL_UNI_SIZE:usize = 1;

#[derive(Default,Clone)]
pub struct DrawList{
    pub draws:Vec<Draw>,
    pub draws_len: usize,
    pub uniforms:Vec<f32> // cmdlist uniforms
}

impl DrawList{
    fn def_uniforms(sh:&mut Shader){
        sh.dl_uniform("prop2", Kind::Float);
    }

    pub fn uniform_prop2(&mut self, v:f32){
        self.uniforms[DL_UNI_PROP2] = v;
    }
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
            title:"Hello World".to_string(),
            running:true,
            uniforms:uniforms
        }
    }
}

const CX_UNI_PROP1:usize = 0;
const CX_UNI_SIZE:usize = 1;

impl Cx{
    fn def_uniforms(sh: &mut Shader){
        sh.cx_uniform("prop1", Kind::Float);
    }

    pub fn uniform_prop1(&mut self, v:f32){
        self.uniforms[CX_UNI_PROP1] = v;
    }

    fn exec_draw_list(&mut self, id: usize){
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

                    // set up the global uniforms (once per program)
                    CxShaders::set_uniform_buffer_fallback(&shgl.cx_uniforms, &self.uniforms);
                    CxShaders::set_uniform_buffer_fallback(&shgl.dl_uniforms, &draw_list.uniforms);
                    CxShaders::set_uniform_buffer_fallback(&shgl.dr_uniforms, &draw.uniforms);

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

impl DrawCx{
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
            draw_list.uniforms.resize(DL_UNI_SIZE, 0.0);
        }
        else{
            // set len to 0
            let draw_list = &mut cx.drawing.draw_lists[self.draw_list_id];
            draw_list.draws_len = 0;
        }
        // push ourselves up the parent draw_stack
        if let Some(parent_draw) = cx.drawing.draw_cx_stack.last(){

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
        cx.drawing.draw_cx_stack.push(self.clone());
    }

    pub fn end(&mut self, cx:&mut Cx){
        cx.drawing.draw_cx_stack.pop();
    }
}
