use crate::shader::*;
use crate::cxtextures::*;
use crate::cxdrawing::*;
use crate::cxshaders_shared::*;

use cocoa::foundation::{NSRange, NSAutoreleasePool};
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
pub struct AssembledMtlShader{
 //   pub geometry_slots:usize,
    pub instance_slots:usize,
 //   pub geometry_attribs:usize,
 //   pub instance_attribs:usize,
    pub uniforms_dr: Vec<ShVar>,
    pub uniforms_dl: Vec<ShVar>,
    pub uniforms_cx: Vec<ShVar>,
    pub samplers_2d:Vec<ShVar>,

    pub mtlsl:String,
}

#[derive(Default,Clone)]
pub struct MetalBuffer{
    pub buffer:Option<metal::Buffer>,
    pub size:usize
}

impl MetalBuffer{
    pub fn update_with_f32_data(&mut self, device:&Device, data:&Vec<f32>){
        if self.size != data.len(){
            self.buffer = None;
        }
        if let None = &self.buffer{
            self.buffer = Some(
                device.new_buffer(
                    (data.len() * std::mem::size_of::<f32>()) as u64,
                    MTLResourceOptions::CPUCacheModeDefaultCache
                )
            );
            self.size = data.len()
        }
        if let Some(buffer) = &self.buffer{
            let p = buffer.contents(); 
            unsafe {
                std::ptr::copy(data.as_ptr(), p as *mut f32, data.len());
            }
            buffer.did_modify_range(NSRange::new(0 as u64, (data.len() * std::mem::size_of::<f32>()) as u64));
        }
    }

    pub fn update_with_u32_data(&mut self, device:&Device, data:&Vec<u32>){
        if self.size != data.len(){
            self.buffer = None;
        }
        if let None = &self.buffer{
            self.buffer = Some(
                device.new_buffer(
                    (data.len() * std::mem::size_of::<u32>()) as u64,
                    MTLResourceOptions::CPUCacheModeDefaultCache
                )
            );
            self.size = data.len()
        }
        if let Some(buffer) = &self.buffer{
            let p = buffer.contents(); 
            unsafe {
                std::ptr::copy(data.as_ptr(), p as *mut u32, data.len());
            }
            buffer.did_modify_range(NSRange::new(0 as u64, (data.len() * std::mem::size_of::<u32>()) as u64));
        }
    }
}

#[derive(Clone, Default)]
pub struct CxBuffers{
     pub uni_cx:MetalBuffer
}

#[derive(Clone, Default)]
pub struct DrawListBuffers{
     pub uni_dl:MetalBuffer
}


#[derive(Default,Clone)]
pub struct DrawBuffers{
    pub uni_dr:MetalBuffer,
    pub inst_vbuf:MetalBuffer
}

#[derive(Default,Clone)]
pub struct CompiledShader{
    pub library:Option<metal::Library>,
    pub pipeline_state:Option<metal::RenderPipelineState>,
    pub shader_id: usize,
    pub assembled_shader: AssembledMtlShader,
    pub geom_vbuf:MetalBuffer,
    pub geom_ibuf:MetalBuffer
}

#[derive(Default,Clone)]
pub struct GLTexture2D{
    pub texture_id: usize
}

#[derive(Clone, Default)]
pub struct CxShaders{
    pub compiled_shaders: Vec<CompiledShader>,
    pub shaders: Vec<Shader>,
}

impl CxShaders{

    pub fn get(&self, id:usize)->&CompiledShader{
        &self.compiled_shaders[id]
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
            if let Ok(mtlsh) = mtlsh{
                self.compiled_shaders.push(CompiledShader{
                    shader_id:self.compiled_shaders.len(),
                    ..mtlsh
                });
            }
            else if let Err(err) = mtlsh{
                println!("GOT ERROR: {}", err.msg);
                self.compiled_shaders.push(
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
        //let geometry_slots = sh.compute_slot_total(&geometries);
        let instance_slots = sh.compute_slot_total(&instances);
        //let varying_slots = sh.compute_slot_total(&varyings);

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
        mtl_out.push_str("vertex _Vary _vertex_shader(device _Geom *in_geometries [[buffer(0)]], device _Inst *in_instances [[buffer(1)]],\n");
        mtl_out.push_str("  device _UniCx &_uni_cx [[buffer(2)]], device _UniDl &_uni_dl [[buffer(3)]], device _UniDr &_uni_dr [[buffer(4)]],\n");
        mtl_out.push_str("  uint vtx_id [[vertex_id]], uint inst_id [[instance_id]]){\n");
        mtl_out.push_str("  _Loc _loc;\n");
        mtl_out.push_str("  _Vary _vary;\n");
        mtl_out.push_str("  _Geom _geom = in_geometries[vtx_id];\n");
        mtl_out.push_str("  _Inst _inst = in_instances[inst_id];\n");
        mtl_out.push_str("  _vary.mtl_position = _vertex(");
        mtl_out.push_str(&vtx_cx.defargs_call);
        mtl_out.push_str(");\n\n");

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

        mtl_out.push_str("       return _vary;\n");
        mtl_out.push_str("};\n");
        // then the fragment shader
        mtl_out.push_str("fragment float4 _fragment_shader(_Vary _vary[[stage_in]],\n");
        mtl_out.push_str("  device _UniCx &_uni_cx [[buffer(0)]], device _UniDl &_uni_dl [[buffer(1)]], device _UniDr &_uni_dr [[buffer(2)]]){\n");
        mtl_out.push_str("  _Loc _loc;\n");
        mtl_out.push_str("  return _pixel(");
        mtl_out.push_str(&pix_cx.defargs_call);
        mtl_out.push_str(");\n};\n");



        // alright lets make some metal source
        // we have to compute the 'Varying struct
        // we have to compute the uniform structs (cx, dl and dr)
        // also have to compute the geometry struct and the instance struct
        println!("---- Metal shader -----\n{}",mtl_out);
       
        // lets composite our ShAst structure into a set of methods
        Ok(AssembledMtlShader{
//            geometry_slots:geometry_slots,
            instance_slots:instance_slots,
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

        match library{
            Err(library)=>Err(SlErr{msg:library}),
            Ok(library)=>Ok(CompiledShader{
                shader_id:0,
                pipeline_state:{
                    let vert = library.get_function("_vertex_shader", None).unwrap();
                    let frag = library.get_function("_fragment_shader", None).unwrap();
                    let rpd = RenderPipelineDescriptor::new();
                    rpd.set_vertex_function(Some(&vert));
                    rpd.set_fragment_function(Some(&frag));
                    let color = rpd.color_attachments().object_at(0).unwrap();
                    color.set_pixel_format(MTLPixelFormat::BGRA8Unorm);
                    color.set_blending_enabled(true);
                    color.set_source_rgb_blend_factor(MTLBlendFactor::One);
                    color.set_destination_rgb_blend_factor(MTLBlendFactor::OneMinusSourceAlpha);
                    color.set_source_alpha_blend_factor(MTLBlendFactor::One);
                    color.set_destination_alpha_blend_factor(MTLBlendFactor::OneMinusSourceAlpha);
                    color.set_rgb_blend_operation(MTLBlendOperation::Add);
                    color.set_alpha_blend_operation(MTLBlendOperation::Add);
                    Some(device.new_render_pipeline_state(&rpd).unwrap())
                },
                library:Some(library),
                assembled_shader:ash,
                geom_ibuf:{
                    let mut geom_ibuf = MetalBuffer{..Default::default()};
                    geom_ibuf.update_with_u32_data(device, &sh.geometry_indices);
                    geom_ibuf
                },
                geom_vbuf:{
                    let mut geom_vbuf = MetalBuffer{..Default::default()};
                    geom_vbuf.update_with_f32_data(device, &sh.geometry_vertices);
                    geom_vbuf
                }
            })
        }
    }

    pub fn create_vao(_shgl:&CompiledShader)->GLInstanceVAO{
       
        // create the VAO
         /*unsafe{
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
        }*/
        GLInstanceVAO{
            vao:0,
            vb:0
        }
    }

    pub fn destroy_vao(glivao:&mut GLInstanceVAO){
    }
}