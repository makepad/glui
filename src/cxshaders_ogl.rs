use std::mem;
use std::ptr;
use std::collections::HashMap;

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

struct Glsl{
    sl:String,
    ty:String
}

struct GlslErr{
    msg:String
}

struct GlslDecl{
    name:String,
    ty:String
}

struct GlslCx<'a>{
    shader:&'a Shader,
    scope:Vec<GlslDecl>,
    fndeps:Vec<String>
}

impl<'a> GlslCx<'a>{
    fn scan_scope(&self, name:&str)->Option<String>{
        if let Some(decl) = self.scope.iter().find(|i| i.name == name){
            return Some(decl.ty);
        }
        None
    }
    fn get_type(&self, name:&str)->Result<&ShType, GlslErr>{
        if let Some(ty) = self.shader.find_type(name){
            return Ok(ty);
        }
        Err(GlslErr{msg:format!("Cannot find type {}", name)})
    }
}

impl ShExpr{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        match self{
            ShExpr::ShId(x)=>x.glsl(cx),
            ShExpr::ShLit(x)=>x.glsl(cx),
            ShExpr::ShAssign(x)=>x.glsl(cx),
            ShExpr::ShCall(x)=>x.glsl(cx),
            ShExpr::ShBinary(x)=>x.glsl(cx),
            ShExpr::ShUnary(x)=>x.glsl(cx),
            ShExpr::ShAssignOp(x)=>x.glsl(cx),
            ShExpr::ShIf(x)=>x.glsl(cx),
            ShExpr::ShWhile(x)=>x.glsl(cx),
            ShExpr::ShForLoop(x)=>x.glsl(cx),
            ShExpr::ShBlock(x)=>x.glsl(cx),
            ShExpr::ShField(x)=>x.glsl(cx),
            ShExpr::ShIndex(x)=>x.glsl(cx),
            ShExpr::ShParen(x)=>x.glsl(cx),
            ShExpr::ShReturn(x)=>x.glsl(cx),
            ShExpr::ShBreak(x)=>x.glsl(cx),
            ShExpr::ShContinue(x)=>x.glsl(cx),
        }
    }
}

impl ShId{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        // ok so. we have to find our id on
        if let Some(ty) = cx.scan_scope(&self.name){
            Ok(Glsl{sl:self.name, ty:ty})
        }
        else if let Some(cnst) = cx.shader.find_const(&self.name){
            Ok(Glsl{sl:self.name, ty:cnst.ty})
        } 
        else if let Some(var) = cx.shader.find_var(&self.name){
            Ok(Glsl{sl:self.name, ty:var.ty})
        } 
        else{ // id not found.. lets give an error
            Err(GlslErr{
                msg:format!("Id {} not resolved, is it declared?", self.name)
            })
        }
    }
}

impl ShLit{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        // we do a literal
        match self{
            ShLit::Int(val)=>{
                Ok(Glsl{sl:format!("{}", val), ty:"int".to_string()})
            }
            ShLit::Str(val)=>{
                Ok(Glsl{sl:format!("\"{}\"", val), ty:"string".to_string()})
            }
            ShLit::Float(val)=>{
                Ok(Glsl{sl:format!("{}", val), ty:"float".to_string()})
            }
            ShLit::Bool(val)=>{
                Ok(Glsl{sl:format!("{}", val), ty:"bool".to_string()})
            }
        }
    }
}

impl ShField{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let base = self.base.glsl(cx)?;
        // we now have to figure out the type of member
        let shty = cx.get_type(&base.ty)?;
        // lets get our member 
        if let Some(field) = shty.fields.iter().find(|i| i.name == self.member){
            Ok(Glsl{
                sl:format!("{}.{}", base.sl, self.member),
                ty:field.ty
            })
        }
        else{
            Err(GlslErr{
                msg:format!("member {} not cannot be found on type {}", self.member, base.ty)
            })
        }
    }
}

impl ShIndex{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let base = self.base.glsl(cx)?;
        let index = self.index.glsl(cx)?;
        // limit base type to vec2/3/4
        if base.ty != "vec2" && base.ty != "vec3" && base.ty != "vec4"{
             Err(GlslErr{
                msg:format!("index on unsupported type {}", base.ty)
            })
        }
        else {
            Ok(Glsl{
                sl:format!("{}[{}]", base.sl, index.sl),
                ty:"float".to_string()
            })
        }
    }
}

impl ShAssign{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let left = self.left.glsl(cx)?;
        let right = self.right.glsl(cx)?;
        if left.ty != right.ty{
            Err(GlslErr{
                msg:format!("Left type {} not the same as right {} in assign {}={}", left.ty, right.ty, left.sl, right.sl)
            })
        }
        else{
            Ok(Glsl{
                sl:format!("{} = {}", left.sl, right.sl),
                ty:left.ty
            })
        }
    }
}

impl ShAssignOp{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let left = self.left.glsl(cx)?;
        let right = self.right.glsl(cx)?;
        if left.ty != right.ty{
            Err(GlslErr{
                msg:format!("Left type {} not the same as right {} in assign {}={}", left.ty, right.ty, left.sl, right.sl)
            })
        }
        else{
            Ok(Glsl{
                sl:format!("{}{}{}", left.sl, self.op.to_string(), right.sl),
                ty:left.ty
            })
        }
    }
}

impl ShBinary{
        fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let left = self.left.glsl(cx)?;
        let right = self.right.glsl(cx)?;
        if left.ty != right.ty{
            Err(GlslErr{
                msg:format!("Left type {} not the same as right {} in assign {}={}", left.ty, right.ty, left.sl, right.sl)
            })
        }
        else{
            Ok(Glsl{
                sl:format!("{}{}{}", left.sl, self.op.to_string(), right.sl),
                ty:left.ty
            })
        }
    }
}

impl ShUnary{
        fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let expr = self.expr.glsl(cx)?;
        Ok(Glsl{
            sl:format!("{}{}", self.op.to_string(), expr.sl),
            ty:expr.ty
        })
    }
}

impl ShParen{
        fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let expr = self.expr.glsl(cx)?;
        Ok(Glsl{
            sl:format!("({})", expr.sl),
            ty:expr.ty
        })
    }
}

impl ShBlock{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let sl = String::new();
        sl.push_str("{\n");
        for stmt in self.stmts{
            match *stmt{
                ShStmt::ShLet(stmt) => {
                    let out = stmt.glsl(cx)?;
                    sl.push_str(&out.sl);
                },
                ShStmt::ShExpr(stmt) => {
                    let out = stmt.glsl(cx)?;
                    sl.push_str(&out.sl);
                }
                ShStmt::ShSemi(stmt) => {
                    let out = stmt.glsl(cx)?;
                    sl.push_str(&out.sl);
                }
            }
            sl.push_str(";\n");
        }
        sl.push_str("}");
        Ok(Glsl{
            sl:sl,
            ty:"void".to_string()
        })
    }
}

impl ShCall{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        // we have a call, look up the call type on cx
        if let Some(shfn) = cx.shader.find_fn(&self.call){
            if let Some(block) = shfn.block{ // not internal
                if cx.fndeps.iter().find(|i| **i == self.call).is_none(){
                    cx.fndeps.push(self.call);
                }
            }
            let out = String::new();
            out.push_str(&self.call);
            out.push_str("(");
            // lets check our args and compose return type
            let args_gl = Vec::new();
            let mut gen_t = "".to_string();
            // loop over args and typecheck / fill in generics
            for (i, arg) in self.args.iter().enumerate(){
                let arg_gl = arg.glsl(cx)?;
                let in_ty = arg_gl.ty;
                if i != 0{
                    out.push_str(", ");
                }
                out.push_str(&arg_gl.sl);
                // lets check the type against our shfn
                if i >= shfn.args.len(){
                    return Err(GlslErr{
                        msg:format!("Too many function arguments for call {} got:{} can use:{}", self.call, i+1, shfn.args.len())
                    })
                }
                // lets check our arg type
                let fnarg = &shfn.args[i];
                // lets see if ty is "T" or "O" or "F" or "B"
                if fnarg.ty == "T"{
                    // we already have a gen_t but its not the same
                    if gen_t != "" && gen_t != in_ty{
                        return Err(GlslErr{
                            msg:format!("Function type T incorrectly redefined for call {} type {} as {} for arg {}", self.call, gen_t, in_ty, i) 
                        })
                    }
                    gen_t = in_ty;
                }
                else if fnarg.ty == "F"{ // we have to be a float type
                    if in_ty != "float" && in_ty != "vec2" && in_ty != "vec3" && in_ty != "vec4"{
                        return Err(GlslErr{
                            msg:format!("Function type F is not a float-ty type for call {} for arg {} type {}", self.call, i, in_ty) 
                        })
                    }
                }
                else if fnarg.ty == "B"{ // have to be a boolvec
                    if in_ty != "bool" && in_ty != "bvec2" && in_ty != "bvec3" && in_ty != "bvec4"{
                        return Err(GlslErr{
                            msg:format!("Function arg is not a bool-ty type for call {} for arg {} type {}", self.call, i, in_ty) 
                        })
                    }
                    gen_t = in_ty;
                }
                else if fnarg.ty != in_ty{
                    return Err(GlslErr{
                        msg:format!("Arg wrong type for call {} for arg {} expected type {} got type {}", self.call, i, fnarg.ty, in_ty)
                    })
                }
            }
            // we have less args provided than the fn signature
            // check if they were optional
            if self.args.len() < shfn.args.len(){
                for i in self.args.len()..shfn.args.len(){
                    let fnarg = &shfn.args[i];
                    if fnarg.ty != "O"{
                        return Err(GlslErr{
                            msg:format!("Not enough args for call {} not enough args provided at {}, possible {}", self.call, i, shfn.args.len())
                        })
                    }
                }
            };
            let ret_ty = if shfn.ret == "T" || shfn.ret == "B"{
                gen_t
            }
            else{
                shfn.ret
            };
            out.push_str(")");
            // check our arg types
            // if our return type is T,
            // use one of the args marked T as its type
            // make sure all args are the same type T
            Ok(Glsl{
                sl:out,
                ty:ret_ty
            })
        }
        else{
            Err(GlslErr{
                msg:format!("Cannot find function {}", self.call)
            })
        }
        
    }
}

impl ShIf{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let out = "".to_string();
        out.push_str("if(");
        let cond = self.cond.glsl(cx)?;
        out.push_str(&cond.sl);
        out.push_str(")");

        let then = self.then_branch.glsl(cx)?;
        
        out.push_str(&then.sl);
        if let Some(else_branch) = self.else_branch{
            let else_gl = else_branch.glsl(cx)?;
            out.push_str("else ");
            out.push_str(&else_gl.sl);
        }
        
        Ok(Glsl{
            sl:out,
            ty:"void".to_string()
        })
    }
}

impl ShWhile{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let out = "".to_string();
        out.push_str("while(");
        let cond = self.cond.glsl(cx)?;
        out.push_str(&cond.sl);
        out.push_str(")");

        let body = self.body.glsl(cx)?;
        
        out.push_str(&body.sl);
        
        Ok(Glsl{
            sl:out,
            ty:"void".to_string()
        })
    }
}

impl ShForLoop{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let out = "".to_string();

        out.push_str("for(int ");
        out.push_str(&self.iter);
        out.push_str("=");
        
        let from = self.from.glsl(cx)?;
        out.push_str(&from.sl);

        out.push_str(";");
        out.push_str(&self.iter);
        out.push_str(" < ");

        let to = self.to.glsl(cx)?;
        out.push_str(&to.sl);

        out.push_str(";");
        out.push_str(&self.iter);
        out.push_str("++)");

        let body = self.body.glsl(cx)?;

        out.push_str(&body.sl);
                
        Ok(Glsl{
            sl:out,
            ty:"void".to_string()
        })
    }
}

impl ShReturn{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let out = "".to_string();
        if let Some(expr) = self.expr{
            let expr_gl = expr.glsl(cx)?;
            out.push_str("return ");
            out.push_str(&expr_gl.sl);
        }
        else{
            out.push_str("return;");
        }
        Ok(Glsl{
            sl:out,
            ty:"void".to_string()
        })
    }
}

impl ShBreak{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        Ok(Glsl{
            sl:"break".to_string(),
            ty:"void".to_string()
        })
    }
}

impl ShContinue{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        Ok(Glsl{
            sl:"continue".to_string(),
            ty:"void".to_string()
        })
    }
}

impl ShLet{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let out = "".to_string();
        out.push_str(&self.ty);
        out.push_str(" ");
        out.push_str(&self.name);
        out.push_str(" = ");
        let init = self.init.glsl(cx)?;
        out.push_str(&init.sl);
        Ok(Glsl{
            sl:out,
            ty:"void".to_string()
        })
    }
}

#[derive(Default,Clone)]
pub struct AssembledShader{

    pub functions:HashMap<String, ShFn>,
    pub uniforms_dr:Vec<ShVar>,
    pub uniforms_dl:Vec<ShVar>,
    pub uniforms_cx:Vec<ShVar>,
    
    pub geom_slots:usize,
    pub inst_slots:usize,
    pub geom_attribs:usize,
    pub inst_attribs:usize,

    //pub dr_uniforms: Vec<ShaderUniform>,
    //pub dl_uniforms: Vec<ShaderUniform>,
    //pub cx_uniforms: Vec<ShaderUniform>,
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
        
        let mut ash = AssembledShader{..Default::default()};
        
        // we can fetch functions on our shader

        // we can also flatten our uniform variable set
        
        // lets composite our ShAst structure into a set of methods
        
        return ash;
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
                //cx_uniforms:Self::compile_get_uniforms(program, &ash.cx_uniforms),
                //dl_uniforms:Self::compile_get_uniforms(program, &ash.dl_uniforms),
                //dr_uniforms:Self::compile_get_uniforms(program, &ash.dr_uniforms),
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