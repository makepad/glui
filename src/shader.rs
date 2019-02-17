
// Shader AST typedefs

pub use shader_ast::*;

// The AST block
#[derive(Clone)]
pub struct ShAst{
    pub types:Vec<ShType>,
    pub vars:Vec<ShVar>,
    pub consts:Vec<ShConst>,
    pub fns:Vec<ShFn>
}

#[derive(Clone)]
pub struct ShFnArg{
    pub name:String,
    pub ty:String
}

impl ShFnArg{
    pub fn new(name:&str, ty:&str)->Self{
        Self{
            name:name.to_string(),
            ty:ty.to_string()
        }
    }
}

#[derive(Clone)]
pub struct ShFn{
    pub name:String,
    pub args:Vec<ShFnArg>,
    pub ret:String,
    pub block:Option<ShBlock>,
}

#[derive(Clone, PartialEq)]
pub enum ShVarStore{
    Uniform,
    UniformDl,
    UniformCx,
    Instance,
    InstanceV,
    Geometry,
    GeometryV,
    Sampler2D,
    Local,
    Varying,
}

#[derive(Clone)]
pub struct ShVar{
    pub name:String,
    pub ty:String,
    pub store:ShVarStore
}

#[derive(Clone)]
pub struct ShConst{
    pub name:String,
    pub ty:String,
    pub value:ShExpr
}

#[derive(Clone)]
pub struct ShTypeField{
    pub name:String,
    pub ty:String,
}

impl ShTypeField{
    pub fn new(name:&str, ty:&str)->Self{
        Self{
            name:name.to_string(),
            ty:ty.to_string()
        }
    }
}

#[derive(Clone)]
pub struct ShType{
    pub name:String,
    pub slots:usize,
    pub prim:bool,
    pub fields:Vec<ShTypeField>
}

// AST tree nodes

#[derive(Clone)]
pub enum ShExpr{
    ShId(ShId),
    ShLit(ShLit),
    ShField(ShField),
    ShIndex(ShIndex),
    ShAssign(ShAssign),
    ShAssignOp(ShAssignOp),
    ShBinary(ShBinary),
    ShUnary(ShUnary),
    ShParen(ShParen),
    ShBlock(ShBlock),
    ShCall(ShCall),
    ShIf(ShIf),
    ShWhile(ShWhile),
    ShForLoop(ShForLoop),
    ShReturn(ShReturn),
    ShBreak(ShBreak),
    ShContinue(ShContinue)
}

#[derive(Clone)]
pub enum ShBinOp{
    Add,Sub,Mul,Div,
    Rem,
    And,Or,
    BitXor,BitAnd,BitOr,
    Shl,Shr,
    Eq, Lt, Le, Ne, Ge, Gt,
    AddEq,SubEq,MulEq,DivEq,RemEq,
    BitXorEq,BitAndEq,BitOrEq,ShlEq,ShrEq
}

impl ShBinOp{
    pub fn to_string(&self)->&str{
        match self{
            ShBinOp::Add=>"+",
            ShBinOp::Sub=>"-",
            ShBinOp::Mul=>"*",
            ShBinOp::Div=>"/",
            ShBinOp::Rem=>"%",
            ShBinOp::And=>"&&",
            ShBinOp::Or=>"||",
            ShBinOp::BitXor=>"^",
            ShBinOp::BitAnd=>"&",
            ShBinOp::BitOr=>"|",
            ShBinOp::Shl=>"<<",
            ShBinOp::Shr=>">>",
            ShBinOp::Eq=>"==",
            ShBinOp::Lt=>"<",
            ShBinOp::Le=>"<=",
            ShBinOp::Ne=>"!=",
            ShBinOp::Ge=>">=",
            ShBinOp::Gt=>">",
            ShBinOp::AddEq=>"+=",
            ShBinOp::SubEq=>"-=",
            ShBinOp::MulEq=>"*=",
            ShBinOp::DivEq=>"/=",
            ShBinOp::RemEq=>"%=",
            ShBinOp::BitXorEq=>"^=",
            ShBinOp::BitAndEq=>"&=",
            ShBinOp::BitOrEq=>"|=",
            ShBinOp::ShlEq=>"<<=",
            ShBinOp::ShrEq=>">>=",
        }
    }
}

#[derive(Clone)]
pub struct ShId{
    pub name:String
}

#[derive(Clone)]
pub enum ShLit{
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool)
}

#[derive(Clone)]
pub struct ShField{
    pub base:Box<ShExpr>,
    pub member:String
}

#[derive(Clone)]
pub struct ShIndex{
    pub base:Box<ShExpr>,
    pub index:Box<ShExpr>
}

#[derive(Clone)]
pub struct ShAssign{
    pub left:Box<ShExpr>,
    pub right:Box<ShExpr>
}

#[derive(Clone)]
pub struct ShAssignOp{
    pub left:Box<ShExpr>,
    pub right:Box<ShExpr>,
    pub op:ShBinOp
}

#[derive(Clone)]
pub struct ShBinary{
    pub left:Box<ShExpr>,
    pub right:Box<ShExpr>,
    pub op:ShBinOp
}

#[derive(Clone)]
pub enum ShUnaryOp{
    Not, Neg
}

impl ShUnaryOp{
    pub fn to_string(&self)->&str{
        match self{
            ShUnaryOp::Not=>"!",
            ShUnaryOp::Neg=>"-"
        }
    }
}

#[derive(Clone)]
pub struct ShUnary{
    pub expr:Box<ShExpr>,
    pub op:ShUnaryOp
}

#[derive(Clone)]
pub struct ShParen{
    pub expr:Box<ShExpr>,
}

#[derive(Clone)]
pub enum ShStmt{
    ShLet(ShLet),
    ShExpr(ShExpr),
    ShSemi(ShExpr)
}

#[derive(Clone)]
pub struct ShBlock{
    pub stmts:Vec<Box<ShStmt>>
}

#[derive(Clone)]
pub struct ShCall{
    pub call:String,
    pub args:Vec<Box<ShExpr>>
}

#[derive(Clone)]
pub struct ShIf{
    pub cond:Box<ShExpr>,
    pub then_branch:ShBlock,
    pub else_branch:Option<Box<ShExpr>>,
}

#[derive(Clone)]
pub struct ShWhile{
    pub cond:Box<ShExpr>,
    pub body:ShBlock,
}

#[derive(Clone)]
pub struct ShForLoop{
    pub iter:String,
    pub from:Box<ShExpr>,
    pub to:Box<ShExpr>,
    pub body:ShBlock
}

#[derive(Clone)]
pub struct ShReturn{
    pub expr:Option<Box<ShExpr>>
}

#[derive(Clone)]
pub struct ShBreak{
}

#[derive(Clone)]
pub struct ShContinue{
}

#[derive(Clone)]
pub struct ShLet{
    pub name:String,
    pub ty:String,
    pub init:Box<ShExpr>
}



#[derive(Default,Clone)]
pub struct Shader{
    pub log:i32,
    pub geometry_vertices:Vec<f32>,
    pub geometry_indices:Vec<u32>,
    pub asts:Vec<ShAst>
}

impl Shader{
    pub fn add_ast(&mut self, ast:ShAst){
        self.asts.push(ast);
    }

    pub fn def()->Shader{
        let sh = Shader{..Default::default()};
       
        // lets add the default library
        sh
    }

    // flatten our
    pub fn flat_vars(&self, store:ShVarStore)->Vec<ShVar>{
        let mut ret = Vec::new();
        for ast in self.asts.iter().rev(){
            for shvar in &ast.vars{
                // abusing an enum with flags complicates flattening a bit
                if shvar.store == store || 
                    store == ShVarStore::Varying && shvar.store == ShVarStore::GeometryV ||
                    store == ShVarStore::Varying && shvar.store == ShVarStore::InstanceV ||
                    store == ShVarStore::Geometry && shvar.store == ShVarStore::GeometryV ||
                    store == ShVarStore::Instance && shvar.store == ShVarStore::InstanceV{
                    ret.push(shvar.clone());
                }
            }
        }
        ret
    }

    // find a function
    pub fn find_fn(&self, name:&str)->Option<&ShFn>{
        for ast in self.asts.iter().rev(){
            for shfn in &ast.fns{
                if shfn.name == name{
                    return Some(&shfn)
                }
            }
        }
        None
    }

    pub fn find_var(&self, name:&str)->Option<&ShVar>{
        for ast in self.asts.iter().rev(){
            for shvar in &ast.vars{
                if shvar.name == name{
                    return Some(&shvar)
                }
            }
        }
        None
    }

    pub fn find_const(&self, name:&str)->Option<&ShConst>{
        for ast in self.asts.iter().rev(){
            for shconst in &ast.consts{
                if shconst.name == name{
                    return Some(&shconst)
                }
            }
        }
        None
    }

    pub fn find_type(&self, name:&str)->Option<&ShType>{
        for ast in self.asts.iter().rev(){
            for shtype in &ast.types{
                if shtype.name == name{
                    return Some(&shtype)
                }
            }
        }
        None
    }

    pub fn get_type_slots(&self, name:&str)->usize{
        if let Some(ty) = self.find_type(name){
            return ty.slots;
        }
        0
    }

    pub fn compute_slot_total(&self, vars:&Vec<ShVar>)->usize{
        let mut slots:usize = 0;
        for var in vars{
            slots += self.get_type_slots(&var.ty);
        }
        slots
    }

    pub fn def_builtins(&mut self){
        self.asts.push(
            ShAst{
                types:vec![
                    ShType{name:"float".to_string(), slots:1, prim:true, fields:Vec::new()},
                    ShType{name:"int".to_string(), slots:1, prim:true, fields:Vec::new()},
                    ShType{name:"bool".to_string(), slots:1, prim:true, fields:Vec::new()},
                    ShType{
                        name:"vec2".to_string(),
                        slots:2,
                        prim:true,
                        fields:vec![ShTypeField::new("x","float"),ShTypeField::new("y","float")]
                    },
                    ShType{
                        name:"vec3".to_string(),
                        slots:3,
                        prim:true,
                        fields:vec![ShTypeField::new("x","float"),ShTypeField::new("y","float"),ShTypeField::new("z","float")]
                    },
                    ShType{
                        name:"vec4".to_string(),
                        slots:4,
                        prim:true,
                        fields:vec![ShTypeField::new("x","float"),ShTypeField::new("y","float"),ShTypeField::new("z","float"),ShTypeField::new("w","float")]
                    },
                    ShType{
                        name:"mat2".to_string(),
                        slots:4,
                        prim:true,
                        fields:vec![
                            ShTypeField::new("a","float"),ShTypeField::new("b","float"),
                            ShTypeField::new("c","float"),ShTypeField::new("d","float")
                        ]
                    },
                    ShType{
                        name:"mat3".to_string(),
                        slots:9,
                        prim:true,
                        fields:vec![
                            ShTypeField::new("a","float"),ShTypeField::new("b","float"),ShTypeField::new("c","float"),
                            ShTypeField::new("d","float"),ShTypeField::new("e","float"),ShTypeField::new("f","float"),
                            ShTypeField::new("g","float"),ShTypeField::new("h","float"),ShTypeField::new("i","float")
                        ]
                    },
                    ShType{
                        name:"mat4".to_string(),
                        slots:16,
                        prim:true,
                        fields:vec![
                            ShTypeField::new("a","float"),ShTypeField::new("b","float"),ShTypeField::new("c","float"), ShTypeField::new("d","float"),
                            ShTypeField::new("e","float"),ShTypeField::new("f","float"),ShTypeField::new("g","float"),ShTypeField::new("h","float"),
                            ShTypeField::new("i","float"),ShTypeField::new("j","float"),ShTypeField::new("k","float"),ShTypeField::new("l","float"),
                            ShTypeField::new("m","float"),ShTypeField::new("n","float"),ShTypeField::new("o","float"),ShTypeField::new("p","float")
                        ]
                    },
                ],
                vars:Vec::new(),
                fns:vec![
                    // shorthand typed:
                    // T - generic
                    // O - optional
                    // F - float-like (float, vec2, vec3, etc)
                    // B - bool-vector (bvecn)

                    ShFn{name:"sizeof".to_string(), args:vec![ShFnArg::new("type","T")], ret:"int".to_string(), block:None},

                    ShFn{name:"radians".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"degrees".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},

                    ShFn{name:"sin".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"cos".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"tan".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"asin".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"acos".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"atan".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","O")], ret:"T".to_string(), block:None},

                    ShFn{name:"pow".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"exp".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"log".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"exp2".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"log2".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},

                    ShFn{name:"sqrt".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"inversesqrt".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},

                    ShFn{name:"abs".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"sign".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"floor".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"ceil".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"fract".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},

                    ShFn{name:"mod".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"min".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"max".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"clamp".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("mi","T"),ShFnArg::new("ma","T")], ret:"T".to_string(), block:None},

                    ShFn{name:"mix".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T"),ShFnArg::new("t","F")], ret:"T".to_string(), block:None},
                    ShFn{name:"step".to_string(), args:vec![ShFnArg::new("e","T"),ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"smoothstep".to_string(), args:vec![ShFnArg::new("e0","F"),ShFnArg::new("e1","F"),ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    
                    ShFn{name:"length".to_string(), args:vec![ShFnArg::new("x","T")], ret:"float".to_string(), block:None},
                    ShFn{name:"distance".to_string(), args:vec![ShFnArg::new("p0","T"),ShFnArg::new("p1","T")], ret:"float".to_string(), block:None},
                    ShFn{name:"dot".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"float".to_string(), block:None},
                    ShFn{name:"cross".to_string(), args:vec![ShFnArg::new("x","vec3"),ShFnArg::new("y","vec3")], ret:"vec3".to_string(), block:None},
                    ShFn{name:"normalize".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},

                    ShFn{name:"faceforward".to_string(), args:vec![ShFnArg::new("n","T"),ShFnArg::new("i","T"),ShFnArg::new("nref","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"reflect".to_string(), args:vec![ShFnArg::new("i","T"),ShFnArg::new("n","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"refract".to_string(), args:vec![ShFnArg::new("i","T"),ShFnArg::new("n","T"),ShFnArg::new("eta","T")], ret:"T".to_string(), block:None},

                    ShFn{name:"matrixCompMult".to_string(), args:vec![ShFnArg::new("a","mat4"),ShFnArg::new("b","mat4")], ret:"mat4".to_string(), block:None},

                    ShFn{name:"lessThan".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"B".to_string(), block:None},
                    ShFn{name:"lessThanEqual".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"B".to_string(), block:None},
                    ShFn{name:"greaterThan".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"B".to_string(), block:None},
                    ShFn{name:"greaterThanEqual".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"B".to_string(), block:None},
                    ShFn{name:"equal".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"B".to_string(), block:None},
                    ShFn{name:"notEqual".to_string(), args:vec![ShFnArg::new("x","T"),ShFnArg::new("y","T")], ret:"B".to_string(), block:None},

                    ShFn{name:"any".to_string(), args:vec![ShFnArg::new("x","B")], ret:"bool".to_string(), block:None},
                    ShFn{name:"all".to_string(), args:vec![ShFnArg::new("x","B")], ret:"bool".to_string(), block:None},
                    ShFn{name:"not".to_string(), args:vec![ShFnArg::new("x","B")], ret:"B".to_string(), block:None},

                    ShFn{name:"dFdx".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"dFdy".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},
                    ShFn{name:"fwidth".to_string(), args:vec![ShFnArg::new("x","T")], ret:"T".to_string(), block:None},

                    ShFn{name:"texture2DLod".to_string(), args:vec![ShFnArg::new("sampler","sampler2D"), ShFnArg::new("coord","vec2"), ShFnArg::new("lod","float")], ret:"vec4".to_string(), block:None},
                    ShFn{name:"texture2DProjLod".to_string(), args:vec![ShFnArg::new("sampler","sampler2D"), ShFnArg::new("coord","vec2"), ShFnArg::new("lod","float")], ret:"vec4".to_string(), block:None},
                    ShFn{name:"textureCubeLod".to_string(), args:vec![ShFnArg::new("sampler","samplerCube"), ShFnArg::new("coord","vec3"), ShFnArg::new("lod","float")], ret:"vec4".to_string(), block:None},

                    ShFn{name:"texture2D".to_string(), args:vec![ShFnArg::new("sampler","sampler2D"), ShFnArg::new("coord","vec2"), ShFnArg::new("bias","O")], ret:"vec4".to_string(), block:None},
                    ShFn{name:"texture2DProj".to_string(), args:vec![ShFnArg::new("sampler","sampler2D"), ShFnArg::new("coord","vec2"), ShFnArg::new("bias","O")], ret:"vec4".to_string(), block:None},
                    ShFn{name:"textureCube".to_string(), args:vec![ShFnArg::new("sampler","samplerCube"), ShFnArg::new("coord","vec3"), ShFnArg::new("bias","O")], ret:"vec4".to_string(), block:None},
                ],
                consts:Vec::new()
            }
        )
    }

    pub fn def_df(&mut self){
        /*
        self.define("PI","3.141592653589793");
		self.define("E","2.718281828459045");
		self.define("LN2","0.6931471805599453");
		self.define("LN10","2.302585092994046");
		self.define("LOG2E","1.4426950408889634");
		self.define("LOG10E","0.4342944819032518");
		self.define("SQRT1_2","0.70710678118654757");
		self.define("TORAD","0.017453292519943295");
		self.define("GOLDEN","1.618033988749895");
        */
        /*
        self.local("df_pos", Kind::Vec2);
        self.local("df_result", Kind::Vec4);
        self.local("df_last_pos", Kind::Vec2);
        self.local("df_start_pos", Kind::Vec2);
        self.local("df_shape", Kind::Float);
        self.local("df_old_shape", Kind::Float);
        self.local("df_blur", Kind::Float);
        self.local("df_aa", Kind::Float);
        self.local("df_scale", Kind::Float);
        self.local("df_field", Kind::Float);
        self.method("
            vec2 df_viewport(vec2 pos){
                df_pos = pos;
                df_result = vec4(0.);
                df_old_shape =
                df_shape = 1e+20;
                df_blur = 0.00001;
                df_aa = df_antialias(pos);
                df_scale = 1.0;
                df_field = 0.0;
                return df_pos;
            }
        ");
        self.method("
            float df_antialias(vec2 p){
                return 1.0 / length(vec2(length(dFdx(p)), length(dFdy(p))));
            }
        ");
        self.method("
            vec2 df_translate(float x, float y){
                df_pos -= vec2(x, y);
            }
        ");
        self.method("
            void df_rotate(float a, float x, float y) {$
                float ca = cos(-a);
                float sa = sin(-a);
                vec2 p = df_pos - vec2(x, y);
                df_pos = vec2(p.x * ca - p.y * sa, p.x * sa + p.y * ca) + vec2(x, y);
            }
        ");
        self.method("
            void df_scale(float f, float x, float y) {$
                df_scale *= f
                df_pos = (df_pos - vec2(x, y)) * f + vec2(x, y)
            }
        ");
        self.method("
            void df_clear(vec4 color){
                df_result = vec4(color.rgb * color.a + df_result.rgb * (1. - color.a), color.a);
            }
        ");
        self.method(
            "float df_calc_blur(float w) {
                float f = w - df_blur;
                float wa = clamp(-w * df_aa, 0., 1.);
                float wb = df_blur < 0.0001?1.0:clamp(-w / df_blur, 0., 1.);
                return wa * wb;
            }"
        );
        self.method("
            void df_fill_keep(vec4 color) {
                float f = df_calc_blur(df_shape);
                vec4 source = vec4(color.rgb * color.a, color.a);
                vec4 dest = df_result;
                df_result = source * f + dest * (1. - source.a * f);
            }
        ");
        self.method("
            vec4 df_fill(vec4 color) {
                df_fill_keep(color);
                df_old_shape = df_shape = 1e+20;
                return df_result;
            }
        ");
        self.method("
            void df_stroke_keep(vec4 color, float width) {
                float f = df_calc_blur(abs(df_shape) - width / df_scale);
                vec4 source = vec4(color.rgb * color.a, color.a);
                vec4 dest = df_result;
                df_result = source * f + dest * (1. - source.a * f);
            }
        ");
        self.method("
            vec4 df_stroke(vec4 color, float width) {
                df_stroke_keep(color, width);
                df_old_shape = df_shape = 1e+20;
                return df_result;
            }
        ");
        self.method("
            void df_glow_keep(vec4 color, float width) {
                float f = df_calc_blur(abs(df_shape) - width / df_scale);
                vec4 source = vec4(color.rgb * color.a, color.a);
                vec4 dest = df_result;
                df_result = vec4(source.rgb * f, 0.) + dest;
            }
        ");
        self.method("
            void df_glow(vec4 color, float width) {$
                df_glow_keep(color, width);
                df_old_shape = df_shape = 1e+20;
            }
        ");
        self.method("
            void df_union() {
                df_old_shape = df_shape = min(df_field, df_old_shape);
            }
        ");
        self.method("
            void df_intersect() {
                df_old_shape = df_shape = max(df_field, df_old_shape);
            }
        ");
        self.method("
            void df_subtract() {
                df_old_shape = df_shape = max(-df_field, df_old_shape);
            }
        ");
        self.method("
            void df_gloop(float k) {
                var h = clamp(.5 + .5 * (df_old_shape - df_field) / k, 0., 1.);
                df_old_shape = df_shape = mix(df_old_shape, df_field, h) - k * h * (1.0 - h);
            }
        ");
        self.method("
            void df_blend(float k){
                df_old_shape = df_shape = mix(df_old_shape, df_field, k);
            }
        ");
        self.method("
            void df_circle(float x, float y, float r) {
                vec2 c = df_pos - vec2(x, y);
                df_field = (length(c.xy) - r) / df_scale;
                df_old_shape = df_shape;
                df_shape = min(df_shape, df_field);
            }
        ");
        self.method("
            void df_box(float x, float y, float w, float h, float r) {
                vec2 p = df_pos - vec2(x, y);
                vec2 size = vec2(.5 * w, .5 * h);
                vec2 bp = max(abs(p - size.xy) - (size.xy - vec2(2. * r).xy), vec2(0.));
                df_field = (length(bp) - 2. * r) / df_scale;
                df_old_shape = df_shape;
                df_shape = min(df_shape, df_field);
            }
        ");
        self.method("
            void df_rect(float x, float y, float w, float h) {
                vec2 s = vec2(w, h) * .5;
                vec2 d = abs(vec2(x, y) - this.pos + s) - s;
                vec2 dm = min(d, vec2(0.));
                df_field = max(dm.x, dm.y) + length(max(d, vec2(0.)));
                df_old_shape = df_shape;
                df_shape = min(df_shape, df_field);
            }
        ");
        self.method("
            void df_move_to(float x, float y) {
                df_last_pos =
                df_start_pos = vec2(x, y);
            }
        ");
        self.method("
            void df_line_to(float x, float y) {
                vec2 p = vec2(x, y);

                vec2 pa = df_pos - df_last_pos;
                vec2 ba = p - df_last_pos;
                float h = clamp(dot(pa, ba) / dot(ba, ba), 0., 1.);
                df_field = length(pa - ba * h) / df_scale;
                df_old_shape = df_shape;
                df_shape = min(df_shape, df_field);
                df_last_pos = p;
            }
        ");
        self.method("
            void df_close_path() {
                df_line_to(df_start_pos.x, df_start_pos.y);
            }
        ");
        self.method("    
            vec4 df_hsv2rgb(vec4 c) { //http://gamedev.stackexchange.com/questions/59797/glsl-shader-change-hue-saturation-brightness
                vec4 K = vec4(1., 2. / 3., 1. / 3., 3.);
                vec4 p = abs(fract(c.xxx + K.xyz) * 6. - K.www);
                return vec4(c.z * mix(K.xxx, clamp(p - K.xxx, 0., 1.), c.y), c.w);
            }
        ");
        self.method("    
            vec4 df_rgb2hsv(vec4 c) {
                vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
                vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
                vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
                
                float d = q.x - min(q.w, q.y);
                float e = 1.0e-10;
                return vec4(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x, c.w);
            }
        ");*/
    }
}
