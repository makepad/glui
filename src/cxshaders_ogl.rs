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
    pub uniforms_dr: Vec<GLUniform>,
    pub uniforms_dl: Vec<GLUniform>,
    pub uniforms_cx: Vec<GLUniform>,
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

pub struct GlslErr{
    msg:String
}

struct GlslDecl{
    name:String,
    ty:String
}

struct GlslCx<'a>{
    depth:usize,
    shader:&'a Shader,
    scope:Vec<GlslDecl>,
    fndeps:Vec<String>
}

impl<'a> GlslCx<'a>{
    fn scan_scope(&self, name:&str)->Option<&str>{
        if let Some(decl) = self.scope.iter().find(|i| i.name == name){
            return Some(&decl.ty);
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
            Ok(Glsl{sl:self.name.to_string(), ty:ty.to_string()})
        }
        else if let Some(cnst) = cx.shader.find_const(&self.name){
            Ok(Glsl{sl:self.name.to_string(), ty:cnst.ty.to_string()})
        } 
        else if let Some(var) = cx.shader.find_var(&self.name){
            Ok(Glsl{sl:self.name.to_string(), ty:var.ty.to_string()})
        } 
        else{ // id not found.. lets give an error
            Err(GlslErr{
                msg:format!("Id {} not resolved, is it declared?", self.name)
            })
        }
    }
}

impl ShLit{
    fn glsl(&self, _cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        // we do a literal
        match self{
            ShLit::Int(val)=>{
                Ok(Glsl{sl:format!("{}", val), ty:"int".to_string()})
            }
            ShLit::Str(val)=>{
                Ok(Glsl{sl:format!("\"{}\"", val), ty:"string".to_string()})
            }
            ShLit::Float(val)=>{
                if val.ceil() == *val{
                    Ok(Glsl{sl:format!("{}.0", val), ty:"float".to_string()})
                }
                else{
                    Ok(Glsl{sl:format!("{}", val), ty:"float".to_string()})
                }
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
                ty:field.ty.to_string()
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
                msg:format!("Left type {} not the same as right {} in assign op {}{}{}", left.ty, self.op.to_string(), right.ty, left.sl, right.sl)
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
            if left.ty == "float" && (right.ty == "vec2" || right.ty == "vec3" || right.ty == "vec4"){
                Ok(Glsl{
                    sl:format!("{}{}{}", left.sl, self.op.to_string(), right.sl),
                    ty:right.ty
                })
            }
            else if right.ty == "float" && (left.ty == "vec2" || left.ty == "vec3" || left.ty == "vec4"){
                Ok(Glsl{
                    sl:format!("{}{}{}", left.sl, self.op.to_string(), right.sl),
                    ty:left.ty
                })
            }
            else{
                Err(GlslErr{
                    msg:format!("Left type {} not the same as right {} in binary op {}{}{}", left.ty, right.ty, left.sl, self.op.to_string(), right.sl)
                })
            }
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
        let mut sl = String::new();
        sl.push_str("{\n");
        cx.depth += 1;
        for stmt in &self.stmts{
            for _i in 0..cx.depth{
                sl.push_str("  ");
            }
            match &**stmt{
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
        cx.depth -= 1;
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
        let mut out = String::new();
        if let Some(shfn) = cx.shader.find_fn(&self.call){
            if let Some(_block) = &shfn.block{ // not internal, so its a dep
                if cx.fndeps.iter().find(|i| **i == self.call).is_none(){
                    cx.fndeps.push(self.call.clone());
                }
            };
            out.push_str(&self.call);
            out.push_str("(");
            // lets check our args and compose return type
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
                shfn.ret.clone()
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
            // its a constructor call
            if let Some(glty) = cx.shader.find_type(&self.call){
                out.push_str(&self.call);
                out.push_str("(");
                // TODO check args
                for (i, arg) in self.args.iter().enumerate(){
                    let arg_gl = arg.glsl(cx)?;
                    if i != 0{
                        out.push_str(", ");
                    }
                    out.push_str(&arg_gl.sl);
                }
                out.push_str(")");
                Ok(Glsl{
                    sl:out,
                    ty:glty.name.clone()
                })
            }
            else{
                Err(GlslErr{
                    msg:format!("Cannot find function {}", self.call)
                })
            }
        }
        
    }
}

impl ShIf{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let mut out = "".to_string();
        out.push_str("if(");
        let cond = self.cond.glsl(cx)?;
        out.push_str(&cond.sl);
        out.push_str(")");

        let then = self.then_branch.glsl(cx)?;
        
        out.push_str(&then.sl);
        if let Some(else_branch) = &self.else_branch{
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
        let mut out = "".to_string();
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
        let mut out = "".to_string();

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
        let mut out = "".to_string();
        if let Some(expr) = &self.expr{
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
    fn glsl(&self, _cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        Ok(Glsl{
            sl:"break".to_string(),
            ty:"void".to_string()
        })
    }
}

impl ShContinue{
    fn glsl(&self, _cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        Ok(Glsl{
            sl:"continue".to_string(),
            ty:"void".to_string()
        })
    }
}

impl ShLet{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let mut out = "".to_string();
        let init = self.init.glsl(cx)?;

        let ty = init.ty.clone();
        if self.ty != "" && self.ty != init.ty{
            return Err(GlslErr{
                msg:format!("Let definition {} type {} is different from initializer {}", self.name, self.ty, init.ty)
            })
        }

        out.push_str(&ty);
        out.push_str(" ");
        out.push_str(&self.name);
        out.push_str(" = ");
        
        // lets define our identifier on scope
        cx.scope.push(GlslDecl{
            name:self.name.clone(),
            ty:init.ty.clone()
        });

        out.push_str(&init.sl);
        Ok(Glsl{
            sl:out,
            ty:"void".to_string()
        })
    }
}

impl ShFn{
    fn glsl(&self, cx:&mut GlslCx)->Result<Glsl,GlslErr>{
        let mut out = "".to_string();
        out.push_str(&self.ret);
        out.push_str(" ");
        out.push_str(&self.name);
        out.push_str("(");
        for (i, arg) in self.args.iter().enumerate(){
            if i != 0{
                out.push_str(", ");
            }
            out.push_str(&arg.ty);
            out.push_str(" ");
            out.push_str(&arg.name);
            cx.scope.push(GlslDecl{
                name:arg.name.clone(),
                ty:arg.ty.clone()
            });
        };
        out.push_str(")");
        if let Some(block) = &self.block{
            let block = block.glsl(cx)?;
            out.push_str(&block.sl);
        };
        Ok(Glsl{
            sl:out,
            ty:self.name.clone()
        })
    }
}

#[derive(Default,Clone)]
pub struct AssembledFn{
    pub sl: String,
    pub fndeps: Vec<String>
}


#[derive(Default,Clone)]
pub struct AssembledShader{
    pub geometry_slots:usize,
    pub instance_slots:usize,
    pub geometry_attribs:usize,
    pub instance_attribs:usize,

    pub uniforms_dr: Vec<ShVar>,
    pub uniforms_dl: Vec<ShVar>,
    pub uniforms_cx: Vec<ShVar>,
    pub samplers_2d:Vec<ShVar>,

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
            if let Ok(glsh) = glsh{
                self.glshaders.push(GLShader{
                    shader_id:self.glshaders.len(),
                    ..glsh
                });
            }
            else if let Err(err) = glsh{
                println!("GOT ERROR: {}", err.msg);
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

    pub fn compile_get_uniforms(program:gl::types::GLuint, sh:&Shader, unis:&Vec<ShVar>)->Vec<GLUniform>{
        let mut gl_uni = Vec::new();
        for uni in unis{
            let mut name0 = "".to_string();
            name0.push_str(&uni.name);
            name0.push_str("\0");
            unsafe{
                gl_uni.push(GLUniform{
                    loc:gl::GetUniformLocation(program, name0.as_ptr() as *const _),
                    name:uni.name.clone(),
                    size:sh.get_type_slots(&uni.ty)
                })
            }
        }
        gl_uni
    }

    pub fn compile_get_samplers_2d(program:gl::types::GLuint, sams:&Vec<ShVar>)->Vec<GLSampler>{
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

    pub fn assemble_fn_and_deps(sh:&Shader, entry_name:&str)->Result<String, GlslErr>{

        let mut cx = GlslCx{
            depth:0,
            shader:sh,
            scope:Vec::new(),
            fndeps:Vec::new()
        };
        cx.fndeps.push(entry_name.to_string());

        let mut fn_done = Vec::<Glsl>::new();

        loop{

            // find what deps we haven't done yet
            let fn_not_done = cx.fndeps.iter().find(|cxfn|{
                if let Some(_done) = fn_done.iter().find(|i| i.ty == **cxfn){
                    false
                }
                else{
                    true
                }
            });
            // do that dep.
            if let Some(fn_not_done) = fn_not_done{
                let fn_to_do = sh.find_fn(fn_not_done);
                if let Some(fn_to_do) = fn_to_do{
                    cx.scope.clear();
                    let result = fn_to_do.glsl(&mut cx)?;
                    fn_done.push(result);
                }
                else{
                    return Err(GlslErr{msg:format!("Cannot find entry function {}", fn_not_done)})
                }
            }
            else{
                break;
            }
        }
        // ok lets reverse concatinate it
        let mut out = String::new();
        for fnd in fn_done.iter().rev(){
            out.push_str(&fnd.sl);
            out.push_str("\n");
        }

        Ok(out)
    }

    pub fn assemble_uniforms(unis:&Vec<ShVar>)->String{
        let mut out = String::new();
        for uni in unis{
            out.push_str("uniform ");
            out.push_str(&uni.ty);
            out.push_str(" ");
            out.push_str(&uni.name);
            out.push_str(";\n")
        };
        out
    }
    
    fn ceil_div4(base:usize)->usize{
        let r = base >> 2;
        if base&3 != 0{
            return r + 1
        }
        r
    }

    pub fn assemble_samplers_2d(unis:&Vec<ShVar>)->String{
        let mut out = String::new();
        for uni in unis{
            out.push_str("uniform sampler2D ");
            out.push_str(&uni.name);
            out.push_str(";\n")
        };
        out
    }

    fn assemble_vartype(i:usize, total:usize, left:usize)->String{
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
    

    fn assemble_varblock(thing: &str, base: &str, slots:usize)->String{
        // ok lets do a ceil
        let mut out = String::new();
        let total = Self::ceil_div4(slots);
        for i in 0..total{
            out.push_str(thing);
            out.push_str(" ");
            out.push_str(&Self::assemble_vartype(i, total, slots&3));
            out.push_str(" ");
            out.push_str(base);
            out.push_str(&i.to_string());
            out.push_str(";\n");
        }
        out
    }

    fn assemble_vardef(var:&ShVar)->String{
        // ok lets do a ceil
        let mut out = String::new();
        out.push_str(&var.ty);
        out.push_str(" ");
        out.push_str(&var.name);
        out.push_str(";\n");
        out
    }

    fn assemble_unpack(base: &str, slot:usize, total_slots:usize,sv:&ShVar)->String{
        let mut out = String::new();
        // ok we have the slot we start at
        out.push_str("    ");
        out.push_str(&sv.name);
        out.push_str("=");
        let id = (slot)>>2;

        // just splat directly
        if sv.ty == "vec2"{
            match slot&3{
                0=>{
                    out.push_str(base);
                    out.push_str(&id.to_string());
                    out.push_str(".xy;\r\n");
                    return out
                }
                1=>{
                    out.push_str(base);
                    out.push_str(&id.to_string());
                    out.push_str(".yz;\r\n");
                    return out
                }
                2=>{
                    out.push_str(base);
                    out.push_str(&id.to_string());
                    out.push_str(".zw;\r\n");
                    return out
                }
                _=>()            
            }
        }
        if sv.ty == "vec3"{
            match slot&3{
                0=>{
                    out.push_str(base);
                    out.push_str(&id.to_string());
                    out.push_str(".xyz;\r\n");
                    return out
                }
                1=>{
                    out.push_str(base);
                    out.push_str(&id.to_string());
                    out.push_str(".yzw;\r\n");
                    return out
                }            
                _=>()            
            }
        }        
        if sv.ty == "vec4"{
            if slot&3 == 0{
                out.push_str(base);
                out.push_str(&id.to_string());
                out.push_str(".xyzw;\r\n");
                return out
            }
        }          
        if sv.ty != "float"{
            out.push_str(&sv.ty);
            out.push_str("(");       
        }

        // splat via loose props
        let svslots = match sv.ty.as_ref(){
            "float"=>1,
            "vec2"=>2,
            "vec3"=>3,
            _=>4
        };

        for i in 0..svslots{
            if i != 0{
                out.push_str(", ");
            }
            out.push_str(base);

            let id = (slot+i)>>2;
            let ext = (slot+i)&3;

            out.push_str(&id.to_string());
            out.push_str(
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
        if sv.ty != "float"{
            out.push_str(")");
        }
        out.push_str(";\n");
        out
    }

    fn assemble_pack_chunk(base: &str, id:usize, chunk:&str, sv:&ShVar)->String{
        let mut out = String::new();
        out.push_str("    ");
        out.push_str(base);
        out.push_str(&id.to_string());
        out.push_str(chunk);
        out.push_str(&sv.name);
        out.push_str(";\n");
        out
    }

    fn assemble_pack(base: &str, slot:usize, total_slots:usize,sv:&ShVar)->String{
        // now we go the other way. we take slot and assign ShaderVar into it
        let mut out = String::new();
        let id = (slot)>>2;

        // just splat directly
        if sv.ty == "vec2"{
            match slot&3{
                0=>return Self::assemble_pack_chunk(base, id, ".xy =", &sv),
                1=>return Self::assemble_pack_chunk(base, id, ".yz =", &sv),
                2=>return Self::assemble_pack_chunk(base, id, ".zw =", &sv),
                _=>()            
            }
        }
        if sv.ty == "vec3"{
            match slot&3{
                0=>return Self::assemble_pack_chunk(base, id, ".xyz =", &sv),
                1=>return Self::assemble_pack_chunk(base, id, ".yzw =", &sv),
                _=>()            
            }
        }        
        if sv.ty == "vec4"{
            if slot&3 == 0{
                return Self::assemble_pack_chunk(base, id, ".xyzw =", &sv);
            }
        }          

       let svslots = match sv.ty.as_ref(){
            "float"=>1,
            "vec2"=>2,
            "vec3"=>3,
            _=>4
        };
        
        for i in 0..svslots{
            out.push_str("    ");
            out.push_str(base);
            let id = (slot+i)>>2;
            let ext = (slot+i)&3;
            out.push_str(&id.to_string());
            out.push_str(
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
            out.push_str(" = ");
            out.push_str(&sv.name);
            out.push_str(
                match i{
                    0=>{
                        if sv.ty == "float"{
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
            out.push_str(";\r\n");
        }
        out
    }

    pub fn assemble_shader(sh:&Shader)->Result<AssembledShader, GlslErr>{
        let mut vtx_out = "#version 100\nprecision highp float;\n".to_string();
        // #extension GL_OES_standard_derivatives : enable
        let mut pix_out = "#version 100\nprecision highp float;\n".to_string();

        // ok now define samplers from our sh. 
        let samplers_2d = sh.flat_vars(ShVarStore::Sampler2D);
        let geometries = sh.flat_vars(ShVarStore::Geometry);
        let instances = sh.flat_vars(ShVarStore::Instance);
        let varyings = sh.flat_vars(ShVarStore::Varying);
        let locals = sh.flat_vars(ShVarStore::Local);
        let uniforms_cx = sh.flat_vars(ShVarStore::UniformCx);
        let uniforms_dl = sh.flat_vars(ShVarStore::UniformDl);
        let uniforms_dr = sh.flat_vars(ShVarStore::Uniform);

        // lets count the slots
        let geometry_slots = sh.compute_slot_total(&geometries);
        let instance_slots = sh.compute_slot_total(&instances);
        let varying_slots = sh.compute_slot_total(&varyings);
        let mut shared = String::new();
        shared.push_str("//Context uniforms\n");
        shared.push_str(&Self::assemble_uniforms(&uniforms_cx));
        shared.push_str("//DrawList uniforms\n");
        shared.push_str(&Self::assemble_uniforms(&uniforms_dl));
        shared.push_str("//Draw uniforms\n");
        shared.push_str(&Self::assemble_uniforms(&uniforms_dr));
        shared.push_str("//Samplers2D\n");
        shared.push_str(&Self::assemble_samplers_2d(&samplers_2d));
        shared.push_str("// Varyings\n");
        shared.push_str(&Self::assemble_varblock("varying", "varying", varying_slots));

        for local in &locals{shared.push_str(&Self::assemble_vardef(&local));}

        pix_out.push_str(&shared);
        vtx_out.push_str(&shared);

        let mut vtx_main = "void main(){\n".to_string();
        let mut pix_main = "void main(){\n".to_string();

        vtx_out.push_str("// Geometry attributes\n");
        vtx_out.push_str(&Self::assemble_varblock("attribute", "geomattr", geometry_slots));
        let mut slot_id = 0;
        for geometry in &geometries{
            vtx_out.push_str(&Self::assemble_vardef(&geometry));
            vtx_main.push_str(&Self::assemble_unpack("geomattr", slot_id, geometry_slots, &geometry));
            slot_id += sh.get_type_slots(&geometry.ty);
        }

        vtx_out.push_str("// Instance attributes\n");
        vtx_out.push_str(&Self::assemble_varblock("attribute", "instattr", instance_slots));
        let mut slot_id = 0;
        for instance in &instances{
            vtx_out.push_str(&Self::assemble_vardef(&instance));
            vtx_main.push_str(&Self::assemble_unpack("instattr", slot_id, instance_slots, &instance));
            slot_id += sh.get_type_slots(&instance.ty);
        }

        vtx_main.push_str("\n    gl_Position = vertex();\n");
        vtx_main.push_str("\n    // Varying packing\n");

        pix_main.push_str("\n    // Varying unpacking\n");

        // alright lets pack/unpack varyings
        let mut slot_id = 0;
        for vary in &varyings{
            // only if we aren't already a geom/instance var
            if geometries.iter().find(|v|v.name == vary.name).is_none() &&
               instances.iter().find(|v|v.name == vary.name).is_none(){
                vtx_out.push_str(&Self::assemble_vardef(&vary));
            } 
            pix_out.push_str(&Self::assemble_vardef(&vary));
            // pack it in the vertexshader
            vtx_main.push_str( &Self::assemble_pack("varying", slot_id, varying_slots, &vary));
            // unpack it in the pixelshader
            pix_main.push_str( &Self::assemble_unpack("varying", slot_id, varying_slots, &vary));
            slot_id += sh.get_type_slots(&vary.ty);
        }

        pix_main.push_str("\n    gl_FragColor = pixel();\n");
        vtx_main.push_str("\n}\n\0");
        pix_main.push_str("\n}\n\0");

        pix_out.push_str("//Function defs\n");
        let pix_fns = Self::assemble_fn_and_deps(sh, "pixel")?;
        pix_out.push_str(&pix_fns);

        vtx_out.push_str("//Function defs\n");
        let vtx_fns = Self::assemble_fn_and_deps(sh, "vertex")?;
        vtx_out.push_str(&vtx_fns);

        vtx_out.push_str("//Main function\n");
        vtx_out.push_str(&vtx_main);

        pix_out.push_str("//Main function\n");
        pix_out.push_str(&pix_main);

        println!("---------- Pixelshader:  ---------\n{}", pix_out);
        println!("---------- Vertexshader:  ---------\n{}", vtx_out);

        // we can also flatten our uniform variable set
        
        // lets composite our ShAst structure into a set of methods
        Ok(AssembledShader{
            geometry_slots:geometry_slots,
            instance_slots:instance_slots,
            geometry_attribs:Self::ceil_div4(geometry_slots),
            instance_attribs:Self::ceil_div4(instance_slots),
            uniforms_dr:uniforms_dr,
            uniforms_dl:uniforms_dl,
            uniforms_cx:uniforms_cx,
            samplers_2d:samplers_2d,
            fragment:pix_out,
            vertex:vtx_out
        })
    }

    pub fn compile_shader(sh:&Shader)->Result<GLShader, GlslErr>{
        let ash = Self::assemble_shader(sh)?;
        // now we have a pixel and a vertex shader
        // so lets now pass it to GL
        unsafe{
            
            let vs = gl::CreateShader(gl::VERTEX_SHADER);
            gl::ShaderSource(vs, 1, [ash.vertex.as_ptr() as *const _].as_ptr(), ptr::null());
            gl::CompileShader(vs);
            if let Some(error) = Self::compile_has_shader_error(true, vs, &ash.vertex){
                return Err(GlslErr{
                    msg:format!("ERROR::SHADER::VERTEX::COMPILATION_FAILED\n{}",error)
                })
            }

            let fs = gl::CreateShader(gl::FRAGMENT_SHADER);
            gl::ShaderSource(fs, 1, [ash.fragment.as_ptr() as *const _].as_ptr(), ptr::null());
            gl::CompileShader(fs);
            if let Some(error) = Self::compile_has_shader_error(true, fs, &ash.fragment){
                return Err(GlslErr{
                    msg:format!("ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n{}",error)
                })
            }

            let program = gl::CreateProgram();
            gl::AttachShader(program, vs);
            gl::AttachShader(program, fs);
            gl::LinkProgram(program);
            if let Some(error) = Self::compile_has_shader_error(false, program, ""){
                return Err(GlslErr{
                    msg:format!("ERROR::SHADER::LINK::COMPILATION_FAILED\n{}",error)
                })
            }
            gl::DeleteShader(vs);
            gl::DeleteShader(fs);

            let geom_attribs = Self::compile_get_attributes(program, "geomattr", ash.geometry_slots, ash.geometry_attribs);
            let inst_attribs = Self::compile_get_attributes(program, "instattr", ash.instance_slots, ash.instance_attribs);

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
            return Ok(GLShader{
                program:program,
                geom_attribs:geom_attribs,
                inst_attribs:inst_attribs,
                geom_vb:geom_vb,
                geom_ib:geom_ib,
                uniforms_cx:Self::compile_get_uniforms(program, sh, &ash.uniforms_cx),
                uniforms_dl:Self::compile_get_uniforms(program, sh, &ash.uniforms_dl),
                uniforms_dr:Self::compile_get_uniforms(program, sh, &ash.uniforms_dr),
                samplers:Self::compile_get_samplers_2d(program, &ash.samplers_2d),
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