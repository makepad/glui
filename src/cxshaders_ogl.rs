use std::mem;
use std::ptr;

use crate::math::*;
use crate::shader::*;
use crate::cx::*;
use crate::cxtextures::*;
use crate::cxdrawing::*;

#[derive(Default,Clone)]
pub struct GLAttribute{
    pub loc:gl::types::GLuint,
    pub size:gl::types::GLsizei,
    pub offset:gl::types::GLsizei,
    pub stride:gl::types::GLsizei
}

#[derive(Default,Clone)]
pub struct GLUniform{
    pub loc:gl::types::GLint,
    pub name:String,
    pub size:usize
}

#[derive(Default,Clone)]
pub struct GLSampler{
    pub loc:gl::types::GLint,
    pub name:String,
    pub sampler:Sampler
}


#[derive(Default,Clone)]
pub struct GLShader{
    pub shader_id: usize,
    pub program: gl::types::GLuint,
    pub geom_attribs: Vec<GLAttribute>,
    pub inst_attribs: Vec<GLAttribute>,
    pub geom_vb: gl::types::GLuint,
    pub geom_ib: gl::types::GLuint,
    pub assembled_shader: AssembledShader,
    pub dr_uniforms: Vec<GLUniform>,
    pub dl_uniforms: Vec<GLUniform>,
    pub cx_uniforms: Vec<GLUniform>,
    pub samplers: Vec<GLSampler>
}


#[derive(Default,Clone)]
pub struct AssembledShader{
    pub geom_slots:usize,
    pub inst_slots:usize,
    pub geom_attribs:usize,
    pub inst_attribs:usize,
    pub fragment:String,
    pub vertex:String
}

#[derive(Default,Clone)]
pub struct GLTexture2D{
    pub texture_id: usize
}

#[derive(Clone, Default)]
pub struct CxShaders{
    pub glshaders: Vec<GLShader>,
    pub shaders: Vec<Shader>,
}

impl CxShaders{

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
            let glsh = Self::compile_shader(&sh);
            if let Some(glsh) = glsh{
                self.glshaders.push(GLShader{
                    shader_id:self.glshaders.len(),
                    ..glsh
                });
            }
            else{
                self.glshaders.push(
                    GLShader{..Default::default()}
                )
            }
        };
    }

    pub fn compile_has_shader_error(compile:bool, shader:gl::types::GLuint, source:&str)->Option<String>{
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

    pub fn compile_get_attributes(program:gl::types::GLuint, prefix:&str, slots:usize, num_attr:usize)->Vec<GLAttribute>{
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

    pub fn compile_get_uniforms(program:gl::types::GLuint, unis:&Vec<ShaderUniform>)->Vec<GLUniform>{
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

    pub fn compile_get_samplers(program:gl::types::GLuint, sams:&Vec<ShaderSampler>)->Vec<GLSampler>{
        let mut gl_samplers = Vec::new();
        for sam in sams{
            let mut name0 = "".to_string();
            name0.push_str(&sam.name);
            name0.push_str("\0");
            unsafe{
                gl_samplers.push(GLSampler{
                    loc:gl::GetUniformLocation(program, name0.as_ptr() as *const _),
                    name:sam.name.clone(),
                    sampler:sam.sampler.clone()
                })
            }
        }
        gl_samplers
    }

    pub fn find_method<'a>(sh:&'a Shader, name:&str)->Option<&'a str>{
        for method in &sh.methods{
            if let Some(index) = method.find('('){
                if &method[(index - name.len())..index] == name{
                    return Some(method)
                }
            }
        }
        None
    }

    pub fn gather_locals(sh:&Shader, deps:&Vec<String>, locals:&mut Vec<ShaderVar>){
        for dep in deps{
            let sm = CxShaders::find_method(sh, dep);
            if let Some(sm) = sm{ 
                // lets find all the locals in sm
                for local in &sh.locals{
                    
                    if sm.contains(&local.name) && !locals.contains(local){
                        locals.push(local.clone());
                    }
                }
            }
        }
        println!("{:?}", locals);
    }

    pub fn gather_deps(sh:&Shader, body:&str, deps:&mut Vec<String>){
        let mut scan = Vec::<char>::new();
        for (i, c) in body.chars().enumerate() {
            if c == '('{
                let found = scan[0..(i-1)].iter().rposition(|v|{
                    !(*v>='A' && *v<='Z' || *v>='a' && *v<='z' || *v>='0' && *v<='9' || *v=='_')
                });
                if let Some(found) = found{
                    let st = scan[(found+1)..i].iter().map(|c| *c).collect::<String>();
                    
                    if !deps.contains(&st){
                        let sm = CxShaders::find_method(sh, &st);
                        if let Some(sm) = sm{
                            deps.push(st);
                            CxShaders::gather_deps(sh, &sm, deps);
                        }
                    }
                }
            }
            scan.push(c);
        }
    }

    fn combine_deps(sh:&Shader, deps:&Vec<String>)->String{
        let mut r = "".to_string();
        for dep in deps.iter().rev(){
            let m = CxShaders::find_method(sh, dep).unwrap();
            r.push_str(&m);
            r.push_str("\n");
        }
        r
    }

    fn locals_def(locals:&Vec<ShaderVar>)->String{
         let mut r = "".to_string();
        for local in locals{
            r.push_str(&CxShaders::variable_gl_def(local));
        }
        r
    }

    fn ceil_div4(base:usize)->usize{
        let r = base >> 2;
        if base&3 != 0{
            return r + 1
        }
        r
    }

    fn variable_gl_def(sv:&ShaderVar)->String{
        let mut s = "".to_string();
        s.push_str(sv.kind.name());
        s.push_str(" ");
        s.push_str(&sv.name);
        s.push_str(";\n");
        s
    }

    fn variable_type(i:usize, total:usize, left:usize)->String{
        if i == total - 1{
            match left{
                1=>"float",
                2=>"vec2",
                3=>"vec3",
                _=>"vec4"
            }.to_string()
        }
        else{
            "vec4".to_string()
        }
    }
    
    fn variable_def(thing: &str, base: &str, slots:usize)->String{
        // ok lets do a ceil
        let mut r = "".to_string();
        let total = CxShaders::ceil_div4(slots);
        for i in 0..total{
            r.push_str(thing);
            r.push_str(" ");
            r.push_str(&CxShaders::variable_type(i, total, slots&3));
            r.push_str(" ");
            r.push_str(base);
            r.push_str(&i.to_string());
            r.push_str(";\n");
        }
        r
    }

    fn uniforms_def(uniforms: &Vec<ShaderUniform> )->String{
        // ok lets do a ceil
        let mut r = "".to_string();
        for u in uniforms{
            r.push_str("uniform ");
            r.push_str(u.kind.name());
            r.push_str(" ");
            r.push_str(&u.name);
            r.push_str(";\n");
        }
        r
    }

   fn samplers_def(samplers: &Vec<ShaderSampler> )->String{
        // ok lets do a ceil
        let mut r = "".to_string();
        for s in samplers{
            r.push_str("uniform sampler2D ");
            r.push_str(" ");
            r.push_str(&s.name);
            r.push_str(";\n");
        }
        r
    }

    fn variable_exist_check(sv:&ShaderVar, list:&Vec<ShaderVar>)->bool{
        list.iter().any(|v|{
            if v.name == sv.name {
                assert!(v.kind == sv.kind);
                true
            }
            else{
                false
            }
        })
    }

    fn variable_unpack(base: &str, slot:usize, total_slots:usize,sv:&ShaderVar)->String{
        let mut r = "".to_string();
        // ok we have the slot we start at
        r.push_str("    ");
        r.push_str(&sv.name);
        r.push_str("=");
        let id = (slot)>>2;

        // just splat directly
        if sv.kind == Kind::Vec2{
            match slot&3{
                0=>{
                    r.push_str(base);
                    r.push_str(&id.to_string());
                    r.push_str(".xy;\r\n");
                    return r
                }
                1=>{
                    r.push_str(base);
                    r.push_str(&id.to_string());
                    r.push_str(".yz;\r\n");
                    return r
                }            
                2=>{
                    r.push_str(base);
                    r.push_str(&id.to_string());
                    r.push_str(".zw;\r\n");
                    return r
                }
                _=>()            
            }
        }
        if sv.kind == Kind::Vec3{
            match slot&3{
                0=>{
                    r.push_str(base);
                    r.push_str(&id.to_string());
                    r.push_str(".xyz;\r\n");
                    return r
                }
                1=>{
                    r.push_str(base);
                    r.push_str(&id.to_string());
                    r.push_str(".yzw;\r\n");
                    return r
                }            
                _=>()            
            }
        }        
        if sv.kind == Kind::Vec4{
            if slot&3 == 0{
                r.push_str(base);
                r.push_str(&id.to_string());
                r.push_str(".xyzw;\r\n");
                return r
            }
        }          
        if sv.kind != Kind::Float{
            r.push_str(&sv.kind.name());
            r.push_str("(");       
        } 
        // splat via loose props
        for i in 0..sv.kind.slots(){
            if i != 0{
                r.push_str(", ");
            }
            r.push_str(base);

            let id = (slot+i)>>2;
            let ext = (slot+i)&3;

            r.push_str(&id.to_string());
            r.push_str(
                match ext{
                    0=>{
                        if (id == total_slots>>2) && total_slots&3 == 1{
                            ""
                        }
                        else{
                            ".x"
                        }
                    }
                    1=>".y",
                    2=>".z",
                    _=>".w"
                }
            );
        }
        if sv.kind != Kind::Float{
            r.push_str(")");
        }
        r.push_str(";\n");
        r
    }

    fn variable_pack_chunk(r: &mut String, base: &str, id:usize, chunk:&str, sv:&ShaderVar){
        r.push_str("    ");
        r.push_str(base);
        r.push_str(&id.to_string());
        r.push_str(chunk);
        r.push_str(&sv.name);
        r.push_str(";\n");
    }

    fn variable_pack(base: &str, slot:usize, total_slots:usize,sv:&ShaderVar)->String{
        // now we go the other way. we take slot and assign ShaderVar into it
        let mut r = "".to_string();
        let id = (slot)>>2;

        // just splat directly
        if sv.kind == Kind::Vec2{
            match slot&3{
                0=>{
                    CxShaders::variable_pack_chunk(&mut r, base, id, ".xy =", &sv);
                    return r
                }
                1=>{
                    CxShaders::variable_pack_chunk(&mut r, base, id, ".yz =", &sv);
                    return r
                }            
                2=>{
                    CxShaders::variable_pack_chunk(&mut r, base, id, ".zw =", &sv);
                    return r
                }
                _=>()            
            }
        }
        if sv.kind == Kind::Vec3{
            match slot&3{
                0=>{
                    CxShaders::variable_pack_chunk(&mut r, base, id, ".xyz =", &sv);
                    return r
                }
                1=>{
                    CxShaders::variable_pack_chunk(&mut r, base, id, ".yzw =", &sv);
                    return r
                }            
                _=>()            
            }
        }        
        if sv.kind == Kind::Vec4{
            if slot&3 == 0{
                CxShaders::variable_pack_chunk(&mut r, base, id, ".xyzw =", &sv);
                return r
            }
        }          
        // splat via loose props
        for i in 0..sv.kind.slots(){
            r.push_str("    ");
            r.push_str(base);
            let id = (slot+i)>>2;
            let ext = (slot+i)&3;
            r.push_str(&id.to_string());
            r.push_str(
                match ext{
                    0=>{
                        if (id == total_slots>>2) && total_slots&3 == 1{ // we are at last slot
                            ""
                        }
                        else{
                            ".x"
                        }
                    }
                    1=>".y",
                    2=>".z",
                    _=>".w"
                }
            );
            r.push_str(" = ");
            r.push_str(&sv.name);
            r.push_str(
                match i{
                    0=>{
                        if sv.kind.slots() == 1{
                            ""
                        }
                        else{
                            ".x"
                        }
                    }
                    1=>".y",
                    2=>".z",
                    _=>".w"
                }
            );
            r.push_str(";\r\n");
        }
        r
    }

    pub fn assemble_shader(sh:&Shader)->AssembledShader{
        // lets take vertex and pixel
        let vtx_method = CxShaders::find_method(sh,"vertex").unwrap();
        let pix_method = CxShaders::find_method(sh,"pixel").unwrap();

        let mut vtx_deps = Vec::new();
        vtx_deps.push("vertex".to_string());
        CxShaders::gather_deps(sh, &vtx_method, &mut vtx_deps);

        let mut pix_deps = Vec::new();
        pix_deps.push("pixel".to_string());
        CxShaders::gather_deps(sh, &pix_method, &mut pix_deps);

        let mut vtx_locals = Vec::new();
        CxShaders::gather_locals(sh, &vtx_deps, &mut vtx_locals);
        
        let mut pix_locals = Vec::new();
        CxShaders::gather_locals(sh, &pix_deps, &mut pix_locals);
        

        let mut vtx_final = "#version 100\nprecision highp float;\n".to_string();
        let mut pix_final = "#version 100\n#extension GL_OES_standard_derivatives : enable\nprecision highp float;\n".to_string();

        vtx_final.push_str("\n// Samplers\n");
        vtx_final.push_str(&Self::samplers_def(&sh.samplers));
       
        pix_final.push_str("\n// Samplers\n");
        pix_final.push_str(&Self::samplers_def(&sh.samplers));


        vtx_final.push_str("\n// Cx Uniforms\n");
        vtx_final.push_str(&Self::uniforms_def(&sh.cx_uniforms));
       
        pix_final.push_str("\n// Cx Uniforms\n");
        pix_final.push_str(&Self::uniforms_def(&sh.cx_uniforms));

        vtx_final.push_str("\n// DrawList Uniforms\n");
        vtx_final.push_str(&Self::uniforms_def(&sh.dl_uniforms));
       
        pix_final.push_str("\n// DrawList Uniforms\n");
        pix_final.push_str(&Self::uniforms_def(&sh.dl_uniforms));

        vtx_final.push_str("\n// Draw Uniforms\n");
        vtx_final.push_str(&Self::uniforms_def(&sh.dr_uniforms));
       
        pix_final.push_str("\n// Draw Uniforms\n");
        pix_final.push_str(&Self::uniforms_def(&sh.dr_uniforms));

        // count slots
        let geom_slots:usize = sh.geometries.iter().map(|v| v.kind.slots()).sum();
        let inst_slots:usize = sh.instancing.iter().map(|v| v.kind.slots()).sum();
        let vary_slots:usize = sh.varyings.iter().map(|v| v.kind.slots()).sum();
      
        vtx_final.push_str("\n// Geometry attributes\n");
        vtx_final.push_str(&Self::variable_def("attribute", "geomattr", geom_slots));
  
        vtx_final.push_str("\n// Instance attributes\n");
        vtx_final.push_str(&Self::variable_def("attribute", "instattr", inst_slots));
      
        pix_final.push_str("\n// Varyings\n");
        pix_final.push_str(&Self::variable_def("varying", "varying", vary_slots));
        
        vtx_final.push_str("\n// Varyings\n");
        vtx_final.push_str(&Self::variable_def("varying", "varying", vary_slots));
       
        pix_final.push_str("\n// Locals\n");
        pix_final.push_str(&Self::locals_def(&pix_locals));
        
        vtx_final.push_str("\n// Locals\n");
        vtx_final.push_str(&Self::locals_def(&vtx_locals));


        let mut vtx_main = "void main(){\n".to_string();
        let mut pix_main = "void main(){\n".to_string();

        vtx_final.push_str("\n// Geometry attributes local names\n");
        vtx_main.push_str("\n    // Geometry unpacking\n");
        let mut slot_id = 0;
        for var in &sh.geometries{
            vtx_final.push_str(&Self::variable_gl_def(var));
            vtx_main.push_str( &Self::variable_unpack("geomattr", slot_id, geom_slots, var));
            slot_id = slot_id + var.kind.slots();
        }
 
        vtx_final.push_str("\n// Instance attributes local names\n");
        vtx_main.push_str("\n    // Instance unpacking\n");
        let mut slot_id = 0;
        for var in &sh.instancing{
            vtx_final.push_str(&Self::variable_gl_def(var));
            vtx_main.push_str( &Self::variable_unpack("instattr", slot_id, inst_slots, var));
            slot_id = slot_id + var.kind.slots();
        } 

        vtx_final.push_str("\n// Varyings local names\n");
        pix_final.push_str("\n// Varyings local names\n");
        pix_main.push_str("\n    // Varying unpacking\n");
        vtx_main.push_str("\n    gl_Position = vertex();\n");
        vtx_main.push_str("\n    // Varying packing\n");
        let mut slot_id = 0;
        for var in &sh.varyings{
            // if it already exists, we'll use its local name 
            if !Self::variable_exist_check(var, &sh.geometries) &&
               !Self::variable_exist_check(var, &sh.instancing)  {
                vtx_final.push_str(&Self::variable_gl_def(var));
            }  
            // pack it in the vertexshader
            vtx_main.push_str( &Self::variable_pack("varying", slot_id, vary_slots, var));

            // define in pixelshader
            pix_final.push_str(&Self::variable_gl_def(var));
            // unpack it
            pix_main.push_str( &Self::variable_unpack("varying", slot_id, vary_slots, var));
            slot_id = slot_id + var.kind.slots();
        }
        pix_main.push_str("\n    gl_FragColor = pixel();\n");
        vtx_main.push_str("\n}\n");
        pix_main.push_str("\n}\n");

        vtx_final.push_str("\n// Methods\n");

        // push the method deps
        vtx_final.push_str(
            &Self::combine_deps(sh, &vtx_deps)
        );
        pix_final.push_str(
            &Self::combine_deps(sh, &pix_deps)
        );

        vtx_final.push_str(&vtx_main);

        pix_final.push_str(&pix_main);
        // push the main functions
        if sh.log != 0{
            println!("---------- Vertex Shader ------- {}",  vtx_final);
            // push the main functions
            println!("---------- Pixel Shader --------- {}",  pix_final);
        }
        vtx_final.push_str("\0");
        pix_final.push_str("\0");
        AssembledShader{
            geom_slots:geom_slots,
            inst_slots:inst_slots,
            geom_attribs:CxShaders::ceil_div4(geom_slots),
            inst_attribs:CxShaders::ceil_div4(inst_slots),
            fragment:pix_final,
            vertex:vtx_final
        }
    }

    pub fn compile_shader(sh:&Shader)->Option<GLShader>{
        let ash = Self::assemble_shader(sh);
        // now we have a pixel and a vertex shader
        // so lets now pass it to GL
        unsafe{
            
            let vs = gl::CreateShader(gl::VERTEX_SHADER);
            gl::ShaderSource(vs, 1, [ash.vertex.as_ptr() as *const _].as_ptr(), ptr::null());
            gl::CompileShader(vs);
            if let Some(error) = Self::compile_has_shader_error(true, vs, &ash.vertex){
                println!(
                    "ERROR::SHADER::VERTEX::COMPILATION_FAILED\n{}",
                    error
                );
                return None
            }

            let fs = gl::CreateShader(gl::FRAGMENT_SHADER);
            gl::ShaderSource(fs, 1, [ash.fragment.as_ptr() as *const _].as_ptr(), ptr::null());
            gl::CompileShader(fs);
            if let Some(error) = Self::compile_has_shader_error(true, fs, &ash.fragment){
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
            if let Some(error) = Self::compile_has_shader_error(false, program, ""){
                println!(
                    "ERROR::SHADER::LINK::COMPILATION_FAILED\n{}",
                    error
                );
                return None
            }
            gl::DeleteShader(vs);
            gl::DeleteShader(fs);

            let geom_attribs = Self::compile_get_attributes(program, "geomattr", ash.geom_slots, ash.geom_attribs);
            let inst_attribs = Self::compile_get_attributes(program, "instattr", ash.inst_slots, ash.inst_attribs);

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
                assembled_shader:ash,
                cx_uniforms:Self::compile_get_uniforms(program, &sh.cx_uniforms),
                dl_uniforms:Self::compile_get_uniforms(program, &sh.dl_uniforms),
                dr_uniforms:Self::compile_get_uniforms(program, &sh.dr_uniforms),
                samplers:Self::compile_get_samplers(program, &sh.samplers),
                ..Default::default()
            })
        }
    }

    pub fn create_vao(shgl:&GLShader)->GLInstanceVAO{
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

    pub fn destroy_vao(glivao:&mut GLInstanceVAO){
        unsafe{
            gl::DeleteVertexArrays(1, &mut glivao.vao);
            gl::DeleteBuffers(1, &mut glivao.vb);
        }
    }

    pub fn set_uniform_buffer_fallback(locs:&Vec<GLUniform>, uni:&Vec<f32>){
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

    pub fn set_samplers(locs:&Vec<GLSampler>, texture_ids:&Vec<usize>, cxtex:&CxTextures){
        let mut o = 0;
        for loc in locs{
            let id = texture_ids[o];
            unsafe{
                gl::ActiveTexture(gl::TEXTURE0 + o as u32);
            }        
            
            if loc.loc >=0{
                unsafe{
                    let tex = &cxtex.textures[id];
                    gl::BindTexture(gl::TEXTURE_2D, tex.gl_texture);
                }
            }
            else{
                unsafe{
                    gl::BindTexture(gl::TEXTURE_2D, 0);
                }
            }
            o = o +1;
        }
    }
}