use std::mem;
use std::ptr;

use crate::shader::*;
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
    pub name:String
    //pub sampler:Sampler
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
pub struct ShaderUniform{
    name:String,
    slots:usize
}

#[derive(Default,Clone)]
pub struct ShaderSampler{
    name:String
}

#[derive(Default,Clone)]
pub struct AssembledShader{
    pub geom_slots:usize,
    pub inst_slots:usize,
    pub geom_attribs:usize,
    pub inst_attribs:usize,
    pub dr_uniforms: Vec<ShaderUniform>,
    pub dl_uniforms: Vec<ShaderUniform>,
    pub cx_uniforms: Vec<ShaderUniform>,
    pub samplers:Vec<ShaderSampler>,
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
                    size:uni.slots
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
                    name:sam.name.clone()
                    //,sampler:sam.sampler.clone()
                })
            }
        }
        gl_samplers
    }

    pub fn assemble_shader(sh:&Shader)->AssembledShader{
        // alright we have a shader
        
        // lets composite our ShAst structure into a set of methods
        
        AssembledShader{
            cx_uniforms:Vec::new(),
            dl_uniforms:Vec::new(),
            dr_uniforms:Vec::new(),
            samplers:Vec::new(),
            geom_slots:0,
            inst_slots:0,
            geom_attribs:0,
            inst_attribs:0,
            fragment:"".to_string(),
            vertex:"".to_string()
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
                cx_uniforms:Self::compile_get_uniforms(program, &ash.cx_uniforms),
                dl_uniforms:Self::compile_get_uniforms(program, &ash.dl_uniforms),
                dr_uniforms:Self::compile_get_uniforms(program, &ash.dr_uniforms),
                samplers:Self::compile_get_samplers(program, &ash.samplers),
                assembled_shader:ash,
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