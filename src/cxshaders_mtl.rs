use std::mem;

use crate::shader::*;
use crate::cxtextures::*;
use crate::cxdrawing::*;
use crate::cxshaders_shared::*;

use metal::*;

impl<'a> SlCx<'a>{
    pub fn map_type(&self, ty:&str)->String{
        CxShaders::type_to_metal(ty)
    }

    pub fn map_var(&mut self, var:&ShVar)->String{
        let prefix;
        match var.store{
            ShVarStore::Uniform=>prefix = "_uni_dr.",
            ShVarStore::UniformDl=>prefix = "_uni_dl.",
            ShVarStore::UniformCx=>prefix = "_uni_cx.",
            ShVarStore::Instance=>{
                if let SlTarget::Pixel = self.target{
                    self.auto_vary.push(var.clone());
                    prefix = "_vary.";
                }
                else{
                    prefix = "_inst.";
                }
            },
            ShVarStore::Geometry=>{
                if let SlTarget::Pixel = self.target{
                    self.auto_vary.push(var.clone());
                    prefix = "_vary.";
                }
                else{
                    prefix = "_geom.";
                }
            },
            ShVarStore::Sampler2D=>prefix = "_sampler_2d.",
            ShVarStore::Local=>prefix = "_loc.",
            ShVarStore::Varying=>prefix = "_vary.",
        }
        format!("{}{}", prefix, var.name)
    }
}

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
pub struct AssembledMtlShader{
 //   pub geometry_slots:usize,
 //   pub instance_slots:usize,
 //   pub geometry_attribs:usize,
 //   pub instance_attribs:usize,

    pub uniforms_dr: Vec<ShVar>,
    pub uniforms_dl: Vec<ShVar>,
    pub uniforms_cx: Vec<ShVar>,
    pub samplers_2d:Vec<ShVar>,

    pub mtlsl:String,
}

#[derive(Default,Clone)]
pub struct CompiledShader{
    pub shader_id: usize,
    pub program: gl::types::GLuint,
    pub geom_attribs: Vec<GLAttribute>,
    pub inst_attribs: Vec<GLAttribute>,
    pub geom_vb: gl::types::GLuint,
    pub geom_ib: gl::types::GLuint,
    pub assembled_shader: AssembledMtlShader,
    pub uniforms_dr: Vec<GLUniform>,
    pub uniforms_dl: Vec<GLUniform>,
    pub uniforms_cx: Vec<GLUniform>,
    pub samplers: Vec<GLSampler>
}

#[derive(Default,Clone)]
pub struct GLTexture2D{
    pub texture_id: usize
}

#[derive(Clone, Default)]
pub struct CxShaders{
    pub mtlshaders: Vec<CompiledShader>,
    pub shaders: Vec<Shader>,
}

impl CxShaders{

    pub fn get(&self, id:usize)->&CompiledShader{
        &self.mtlshaders[id]
    }

    pub fn add(&mut self, sh:Shader)->usize{
        let id = self.shaders.len();
        // lets compile this sh
        self.shaders.push(sh);
        id
    }

    pub fn compile_all_shaders(&mut self, device:&Device){
        for sh in &self.shaders{
            let mtlsh = Self::compile_shader(&sh, device);
            if let Ok(glsh) = mtlsh{
                self.mtlshaders.push(CompiledShader{
                    shader_id:self.mtlshaders.len(),
                    ..glsh
                });
            }
            else if let Err(err) = mtlsh{
                println!("GOT ERROR: {}", err.msg);
                self.mtlshaders.push(
                    CompiledShader{..Default::default()}
                )
            }
        };
    }

    pub fn type_to_packed_metal(ty:&str)->String{
        match ty.as_ref(){
            "float"=>"float".to_string(),
            "vec2"=>"packed_float2".to_string(),
            "vec3"=>"packed_float3".to_string(),
            "vec4"=>"packed_float4".to_string(),
            "mat2"=>"packed_float2x2".to_string(),
            "mat3"=>"packed_float3x3".to_string(),
            "mat4"=>"packed_float4x4".to_string(),
            ty=>ty.to_string()
        }
    }

    pub fn type_to_metal(ty:&str)->String{
        match ty.as_ref(){
            "float"=>"float".to_string(),
            "vec2"=>"float2".to_string(),
            "vec3"=>"float3".to_string(),
            "vec4"=>"float4".to_string(),
            "mat2"=>"float2x2".to_string(),
            "mat3"=>"float3x3".to_string(),
            "mat4"=>"float4x4".to_string(),
            ty=>ty.to_string()
        }
    }

    pub fn assemble_struct(name:&str, vars:&Vec<ShVar>, packed:bool, field:&str)->String{
        let mut out = String::new();
        out.push_str("struct ");
        out.push_str(name);
        out.push_str("{\n");
        out.push_str(field);
        for var in vars{
            out.push_str("  ");
            out.push_str(
                &if packed{
                    Self::type_to_packed_metal(&var.ty)
                }
                else{
                    Self::type_to_metal(&var.ty)
                }
            );
            out.push_str(" ");
            out.push_str(&var.name);
            out.push_str(";\n")
        };
        out.push_str("};\n\n");
        out
    }

    pub fn assemble_shader(sh:&Shader)->Result<AssembledMtlShader, SlErr>{
        
        let mut mtl_out = "#include <metal_stdlib>\nusing namespace metal;\n".to_string();

        // ok now define samplers from our sh. 
        let samplers_2d = sh.flat_vars(ShVarStore::Sampler2D);
        let geometries = sh.flat_vars(ShVarStore::Geometry);
        let instances = sh.flat_vars(ShVarStore::Instance);
        let mut varyings = sh.flat_vars(ShVarStore::Varying);
        let locals = sh.flat_vars(ShVarStore::Local);
        let uniforms_cx = sh.flat_vars(ShVarStore::UniformCx);
        let uniforms_dl = sh.flat_vars(ShVarStore::UniformDl);
        let uniforms_dr = sh.flat_vars(ShVarStore::Uniform);

        // lets count the slots
        let geometry_slots = sh.compute_slot_total(&geometries);
        let instance_slots = sh.compute_slot_total(&instances);
        let varying_slots = sh.compute_slot_total(&varyings);

        mtl_out.push_str(&Self::assemble_struct("_Geom", &geometries, true, ""));
        mtl_out.push_str(&Self::assemble_struct("_Inst", &instances, true, ""));
        mtl_out.push_str(&Self::assemble_struct("_UniCx", &uniforms_cx, true, ""));
        mtl_out.push_str(&Self::assemble_struct("_UniDl", &uniforms_dl, true, ""));
        mtl_out.push_str(&Self::assemble_struct("_UniDr", &uniforms_dr, true, ""));
        mtl_out.push_str(&Self::assemble_struct("_Loc", &locals, false, ""));


        let mut vtx_cx = SlCx{
            depth:0,
            target:SlTarget::Vertex,
            defargs_fn:"thread _Loc &_loc, thread _Vary &_vary, thread _Geom &_geom, thread _Inst &_inst, device _UniCx &_uni_cx, device _UniDl &_uni_dl, device _UniDr &_uni_dr".to_string(),
            defargs_call:"_loc, _vary, _geom, _inst, _uni_cx, _uni_dl, _uni_dr".to_string(),
            call_prefix:"_".to_string(),
            shader:sh,
            scope:Vec::new(),
            fn_deps:vec!["vertex".to_string()],
            fn_done:Vec::new(),
            auto_vary:Vec::new()
        };
        let vtx_fns = assemble_fn_and_deps(sh, &mut vtx_cx)?;
        let mut pix_cx = SlCx{
            depth:0,
            target:SlTarget::Pixel,
            defargs_fn:"thread _Loc &_loc, thread _Vary &_vary, device _UniCx &_uni_cx, device _UniDl &_uni_dl, device _UniDr &_uni_dr".to_string(),
            defargs_call:"_loc, _vary, _uni_cx, _uni_dl, _uni_dr".to_string(),
            call_prefix:"_".to_string(),
            shader:sh,
            scope:Vec::new(),
            fn_deps:vec!["pixel".to_string()],
            fn_done:vtx_cx.fn_done,
            auto_vary:Vec::new()
        };        

        let pix_fns = assemble_fn_and_deps(sh, &mut pix_cx)?;

        // lets add the auto_vary ones to the varyings struct
        for auto in &pix_cx.auto_vary{
            varyings.push(auto.clone());
        }
        mtl_out.push_str(&Self::assemble_struct("_Vary", &varyings, false, "  float4 mtl_position [[position]];\n"));

        mtl_out.push_str("//Vertex shader\n");
        mtl_out.push_str(&vtx_fns);
        mtl_out.push_str("//Pixel shader\n");
        mtl_out.push_str(&pix_fns);

        // lets define the vertex shader
        mtl_out.push_str("vertex _Vary vertex_shader(device _Geom *in_geometries [[buffer(0)]], device _Inst *in_instances [[buffer(1)]], ");
        mtl_out.push_str("       device _UniCx &_uni_cx [[buffer(2)]], device _UniDl &_uni_dl [[buffer(3)]], device _UniDr &_uni_dr [[buffer(4)]], ");
        mtl_out.push_str("       uint vtx_id [[vertex_id]], uint inst_id [[instance_id]]){\n");
        mtl_out.push_str("       _Loc _loc;\n");
        mtl_out.push_str("       _Vary _vary;\n");
        mtl_out.push_str("       _Geom _geom = in_geometries[vtx_id];\n");
        mtl_out.push_str("       _Inst _inst = in_instances[inst_id];\n");
        mtl_out.push_str("       _vary.mtl_position = _vertex(");
        mtl_out.push_str(&vtx_cx.defargs_call);
        mtl_out.push_str(");\n");

        for auto in pix_cx.auto_vary{
            if let ShVarStore::Geometry = auto.store{
              mtl_out.push_str("       _vary.");
              mtl_out.push_str(&auto.name);
              mtl_out.push_str(" = _geom.");
              mtl_out.push_str(&auto.name);
              mtl_out.push_str(";\n");
            }
            else if let ShVarStore::Instance = auto.store{
              mtl_out.push_str("       _vary.");
              mtl_out.push_str(&auto.name);
              mtl_out.push_str(" = _inst.");
              mtl_out.push_str(&auto.name);
              mtl_out.push_str(";\n");
            }
        }

        mtl_out.push_str("       return _vary;");
        mtl_out.push_str("};\n");
        // then the fragment shader



        // alright lets make some metal source
        // we have to compute the 'Varying struct
        // we have to compute the uniform structs (cx, dl and dr)
        // also have to compute the geometry struct and the instance struct
        println!("---- Metal shader -----\n{}",mtl_out);
       
        // lets composite our ShAst structure into a set of methods
        Ok(AssembledMtlShader{
//            geometry_slots:geometry_slots,
//            instance_slots:instance_slots,
//            geometry_attribs:Self::ceil_div4(geometry_slots),
 //           instance_attribs:Self::ceil_div4(instance_slots),
            uniforms_dr:uniforms_dr,
            uniforms_dl:uniforms_dl,
            uniforms_cx:uniforms_cx,
            samplers_2d:samplers_2d,
            mtlsl:mtl_out
        })
    }

    pub fn compile_shader(sh:&Shader, device: &Device)->Result<CompiledShader, SlErr>{
        let ash = Self::assemble_shader(sh)?;

        let options = CompileOptions::new();
        //let library = device.new_library_with_source(&ash.mtlsl, &options);
        let library = device.new_library_with_source(&ash.mtlsl, &options);

        if let Err(err) = library{
            println!("{}", err);
        }

        Err(SlErr{msg:"NI".to_string()})
    }

    pub fn create_vao(shgl:&CompiledShader)->GLInstanceVAO{
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