
#[derive(Clone,PartialEq, Debug)]
pub enum Kind{
    Float,
    Vec2,
    Vec3,
    Vec4,
    Mat4
}

impl Default for Kind{
    fn default()->Kind{
        Kind::Float
    }
}

impl Kind{
    pub fn slots(&self)->usize{
        match self {
            Kind::Float=>1,
            Kind::Vec2=>2,
            Kind::Vec3=>3,
            Kind::Vec4=>4,
            Kind::Mat4=>16
        }
    }
    pub fn name(&self)->&str{
        match self {
            Kind::Float=>"float",
            Kind::Vec2=>"vec2",
            Kind::Vec3=>"vec3",
            Kind::Vec4=>"vec4",
            Kind::Mat4=>"mat4"
        }
    }    
}

#[derive(Default, Clone, Debug)]
pub struct ShaderVar{
    pub name:String,
    pub kind:Kind
}





// Shader AST typedefs



pub use shader_ast::*;

// the shader AST types
#[derive(Clone)]
pub enum ShExpr{
    ShId(ShId),
    ShLit(ShLit),
    ShAssign(ShAssign),
    ShCall(ShCall),
    ShBinary(ShBinary),
    ShUnary(ShUnary),
    ShAssignOp(ShAssignOp),
    ShIf(ShIf),
    ShWhile(ShWhile),
    ShForLoop(ShForLoop),
    ShBlock(ShBlock),
    ShField(ShField),
    ShIndex(ShIndex),
    ShParen(ShParen)
}

#[derive(Clone)]
pub struct ShId{
    pub name:String
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
pub enum ShLit{
    ShLitInt(i64),
    ShLitFloat(f64),
    ShLitStr(String),
    ShLitBool(bool)
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
pub struct ShAssign{
    pub left:Box<ShExpr>,
    pub right:Box<ShExpr>
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

#[derive(Clone)]
pub struct ShAssignOp{
    pub left:Box<ShExpr>,
    pub right:Box<ShExpr>,
    pub op:ShBinOp
}

#[derive(Clone)]
pub struct ShUnary{
    pub expr:Box<ShExpr>,
    pub op:ShUnaryOp
}

#[derive(Clone)]
pub struct ShCall{
    pub call:String,
    pub args:Vec<Box<ShExpr>>
}

#[derive(Clone)]
pub struct ShLet{
    pub name:String,
    pub ty:String,
    pub init:Box<ShExpr>
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
pub struct ShFnArg{
    pub name:String,
    pub ty:String
}

#[derive(Clone)]
pub struct ShFn{
    pub name:String,
    pub args:Vec<ShFnArg>,
    pub block:ShBlock,
    pub ret:String
}

#[derive(Clone)]
pub enum ShVarStore{
    Uniform,
    UniformDl,
    UniformCx,
    Instance,
    Geometry,
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
pub struct ShStruct{
}

// the root
#[derive(Clone)]
pub struct ShAst{
    pub structs:Vec<ShStruct>,
    pub vars:Vec<ShVar>,
    pub consts:Vec<ShConst>,
    pub fns:Vec<ShFn>
}





impl PartialEq for ShaderVar{
    fn eq(&self, other: &ShaderVar) -> bool {
        self.name == other.name
    }
}

#[derive(Default, Clone)]
pub struct ShaderUniform{
    pub name:String,
    pub kind:Kind
}

#[derive(Clone)]
pub enum Sampler{
    Sampler2D,
    Sampler3D
}
impl Default for Sampler{
    fn default()->Self{
        Sampler::Sampler2D
    }
}

#[derive(Default, Clone)]
pub struct ShaderSampler{
    pub name:String,
    pub sampler:Sampler
}

#[derive(Default, Clone)]
pub struct ShaderDef{
    pub name:String,
    pub def:String
}

#[derive(Default, Clone)]
pub struct ShaderStruct{
    pub name:String,
    pub def:String
}


#[derive(Default,Clone)]
pub struct Shader{
    pub log:i32,
    pub geometry_vertices:Vec<f32>,
    pub geometry_indices:Vec<u32>,
    pub geometries:Vec<ShaderVar>,
    pub instancing:Vec<ShaderVar>,
    pub varyings:Vec<ShaderVar>,
    pub locals:Vec<ShaderVar>,
    pub defines:Vec<ShaderDef>,
    pub structs:Vec<ShaderStruct>,
    pub dr_uniforms:Vec<ShaderUniform>,
    pub dl_uniforms:Vec<ShaderUniform>,
    pub cx_uniforms:Vec<ShaderUniform>,
    pub samplers:Vec<ShaderSampler>,
    pub methods:Vec<String>,
    pub asts:Vec<ShAst>
}

impl Shader{
    pub fn geometry(&mut self, name:&str, kind:Kind){
        self.geometries.push(
            ShaderVar{
                name:name.to_string(),
                kind:kind
            }
        );
    }
    pub fn geometryv(&mut self, name:&str, kind:Kind){
        self.geometries.push(
            ShaderVar{
                name:name.to_string(),
                kind:kind.clone()
            }
        );
        self.varyings.push(
            ShaderVar{
                name:name.to_string(),
                kind:kind
            }
        );
    }

    pub fn sampler(&mut self, name:&str, sampler:Sampler){
        self.samplers.push(
            ShaderSampler{
                sampler:sampler,
                name:name.to_string()
            }
        );
    }

    pub fn instance(&mut self, name:&str, kind:Kind){
        self.instancing.push(
            ShaderVar{
                name:name.to_string(),
                kind:kind
            }
        );
    }

    pub fn local(&mut self, name:&str, kind:Kind){
        self.locals.push(
            ShaderVar{
                name:name.to_string(),
            kind:kind
            }
        );
    }

   pub fn uniform(&mut self, name:&str, kind:Kind){
        self.dr_uniforms.push(
            ShaderUniform{
                name:name.to_string(),
                kind:kind
            }
        );
    }

   pub fn dl_uniform(&mut self, name:&str, kind:Kind){
        self.dl_uniforms.push(
            ShaderUniform{
                name:name.to_string(),
                kind:kind
            }
        );
    }

   pub fn cx_uniform(&mut self, name:&str, kind:Kind){
        self.cx_uniforms.push(
            ShaderUniform{
                name:name.to_string(),
                kind:kind
            }
        );
    }

    pub fn define(&mut self, name: &str, def: &str){
        self.defines.push(
            ShaderDef{
                name:name.to_string(),
                def:def.to_string()
            }
        );
    }

    pub fn instancev(&mut self, name:&str, kind:Kind){
        self.instancing.push(
            ShaderVar{
                name:name.to_string(),
                kind:kind.clone()
            }
        );
        self.varyings.push(
            ShaderVar{
                name:name.to_string(),
                kind:kind
            }
        );
    }
    
    pub fn varying(&mut self, name:&str, kind:Kind){
        self.varyings.push(
            ShaderVar{
                name:name.to_string(),
                kind:kind
            }
        );
    }

    pub fn method(&mut self, body:&str){
        self.methods.push(body.to_string())
    }

    pub fn shader(&mut self, shader: ShAst){

    }

    pub fn add_ast(&mut self, _root:ShAst){

    }

    pub fn new()->Shader{
        Shader{..Default::default()}
    }

    pub fn def()->Shader{
        let sh = Shader{..Default::default()};
        // lets add the default library
        sh
    }

    pub fn def_constants(&mut self){

        self.define("PI","3.141592653589793");
		self.define("E","2.718281828459045");
		self.define("LN2","0.6931471805599453");
		self.define("LN10","2.302585092994046");
		self.define("LOG2E","1.4426950408889634");
		self.define("LOG10E","0.4342944819032518");
		self.define("SQRT1_2","0.70710678118654757");
		self.define("TORAD","0.017453292519943295");
		self.define("GOLDEN","1.618033988749895");
    }

    pub fn def_df(&mut self){

        self.add_ast(shader_ast!(||{
            let df_pos: vec2<Uniform>;
            let mine: vec2<UniformDl>;
            let x: float<Local>;
            let y: float<Varying>;

            const SQRT1_2:float = 0.070710678118654757;

            fn df_viewport(pos:vec2)->vec2{
                !df_pos;
                let x:int = 10;
                x.test = 10;
                x[10] = (10+10);
            }
        }));
 
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
        ");
    }
}
