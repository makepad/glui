#[derive(Clone,PartialEq)]
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
    fn slots(&self)->usize{
        match self {
            Kind::Float=>1,
            Kind::Vec2=>2,
            Kind::Vec3=>3,
            Kind::Vec4=>4,
            Kind::Mat4=>16
        }
    }
    fn name(&self)->&str{
        match self {
            Kind::Float=>"float",
            Kind::Vec2=>"vec2",
            Kind::Vec3=>"vec3",
            Kind::Vec4=>"vec4",
            Kind::Mat4=>"mat4"
        }
    }    
}

#[derive(Default, Clone)]
pub struct ShaderVar{
    pub name:String,
    pub kind:Kind
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
pub struct CompiledShader{
    pub geom_slots:usize,
    pub inst_slots:usize,
    pub geom_attribs:usize,
    pub inst_attribs:usize,
    pub fragment:String,
    pub vertex:String
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
    pub uniforms:Vec<ShaderVar>,
    pub methods:Vec<String>
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
        self.uniforms.push(
            ShaderVar{
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

    pub fn find_method(&self, name:&str)->Option<&str>{
        for method in &self.methods{
            if let Some(index) = method.find('('){
                if &method[(index - name.len())..index] == name{
                    return Some(method)
                }
            }
        }
        None
    }

    pub fn gather_deps(&self, body:&str, deps:&mut Vec<String>){
        let mut scan = Vec::<char>::new();
        for (i, c) in body.chars().enumerate() {
            if c == '('{
                let found = scan[0..(i-1)].iter().rposition(|v|{
                    !(*v>='A' && *v<='Z' || *v>='a' && *v<='z' || *v>='0' && *v<='9' || *v=='_')
                });
                if let Some(found) = found{
                    let st = scan[(found+1)..i].iter().map(|c| *c).collect::<String>();
                    
                    if !deps.contains(&st){
                        let sm = self.find_method(&st);
                        if let Some(sm) = sm{
                            deps.push(st);
                            self.gather_deps(&sm, deps);
                        }
                    }
                }
            }
            scan.push(c);
        }
    }

    fn combine_deps(&self, deps:&Vec<String>)->String{
        let mut r = "".to_string();
        for dep in deps.iter().rev(){
            let m = self.find_method(dep).unwrap();
            r.push_str(&m);
            r.push_str("\n");
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
        let total = Shader::ceil_div4(slots);
        for i in 0..total{
            r.push_str(thing);
            r.push_str(" ");
            r.push_str(&Shader::variable_type(i, total, slots&3));
            r.push_str(" ");
            r.push_str(base);
            r.push_str(&i.to_string());
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
                    Shader::variable_pack_chunk(&mut r, base, id, ".xy =", &sv);
                    return r
                }
                1=>{
                    Shader::variable_pack_chunk(&mut r, base, id, ".yz =", &sv);
                    return r
                }            
                2=>{
                    Shader::variable_pack_chunk(&mut r, base, id, ".zw =", &sv);
                    return r
                }
                _=>()            
            }
        }
        if sv.kind == Kind::Vec3{
            match slot&3{
                0=>{
                    Shader::variable_pack_chunk(&mut r, base, id, ".xyz =", &sv);
                    return r
                }
                1=>{
                    Shader::variable_pack_chunk(&mut r, base, id, ".yzw =", &sv);
                    return r
                }            
                _=>()            
            }
        }        
        if sv.kind == Kind::Vec4{
            if slot&3 == 0{
                Shader::variable_pack_chunk(&mut r, base, id, ".xyzw =", &sv);
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

    pub fn compile(&self)->CompiledShader{
        // lets take vertex and pixel
        let vtx_method = self.find_method("vertex").unwrap();
        let pix_method = self.find_method("pixel").unwrap();

        let mut vtx_deps = Vec::new();
        vtx_deps.push("vertex".to_string());
        self.gather_deps(&vtx_method, &mut vtx_deps);

        let mut pix_deps = Vec::new();
        pix_deps.push("pixel".to_string());
        self.gather_deps(&pix_method, &mut pix_deps);

        let mut vtx_final = "#version 100\nprecision highp float;\n".to_string();
        let mut pix_final = "#version 100\nprecision highp float;\n".to_string();

        // count slots
        let geom_slots:usize = self.geometries.iter().map(|v| v.kind.slots()).sum();
        let inst_slots:usize = self.instancing.iter().map(|v| v.kind.slots()).sum();
        let vary_slots:usize = self.varyings.iter().map(|v| v.kind.slots()).sum();
      
        vtx_final.push_str("\n// Geometry attributes\n");
        vtx_final.push_str(&Shader::variable_def("attribute", "geomattr", geom_slots));
  
        vtx_final.push_str("\n// Instance attributes\n");
        vtx_final.push_str(&Shader::variable_def("attribute", "instattr", inst_slots));
      
        pix_final.push_str("\n// Varyings\n");
        pix_final.push_str(&Shader::variable_def("varying", "varying", vary_slots));
        
        vtx_final.push_str("\n// Varyings\n");
        vtx_final.push_str(&Shader::variable_def("varying", "varying", vary_slots));
       
        let mut vtx_main = "void main(){\n".to_string();
        let mut pix_main = "void main(){\n".to_string();

        vtx_final.push_str("\n// Geometry attributes local names\n");
        vtx_main.push_str("\n    // Geometry unpacking\n");
        let mut slot_id = 0;
        for var in &self.geometries{
            vtx_final.push_str(&Shader::variable_gl_def(var));
            vtx_main.push_str( &Shader::variable_unpack("geomattr", slot_id, geom_slots, var));
            slot_id = slot_id + var.kind.slots();
        }
 
        vtx_final.push_str("\n// Instance attributes local names\n");
        vtx_main.push_str("\n    // Instance unpacking\n");
        let mut slot_id = 0;
        for var in &self.instancing{
            vtx_final.push_str(&Shader::variable_gl_def(var));
            vtx_main.push_str( &Shader::variable_unpack("instattr", slot_id, inst_slots, var));
            slot_id = slot_id + var.kind.slots();
        } 

        vtx_final.push_str("\n// Varyings local names\n");
        pix_final.push_str("\n// Varyings local names\n");
        pix_main.push_str("\n    // Varying unpacking\n");
        vtx_main.push_str("\n    gl_Position = vertex();\n");
        vtx_main.push_str("\n    // Varying packing\n");
        let mut slot_id = 0;
        for var in &self.varyings{
            // if it already exists, we'll use its local name 
            if !Shader::variable_exist_check(var, &self.geometries) &&
               !Shader::variable_exist_check(var, &self.instancing)  {
                vtx_final.push_str(&Shader::variable_gl_def(var));
            }  
            // pack it in the vertexshader
            vtx_main.push_str( &Shader::variable_pack("varying", slot_id, vary_slots, var));

            // define in pixelshader
            pix_final.push_str(&Shader::variable_gl_def(var));
            // unpack it
            pix_main.push_str( &Shader::variable_unpack("varying", slot_id, vary_slots, var));
            slot_id = slot_id + var.kind.slots();
        }
        pix_main.push_str("\n    gl_FragColor = pixel();\n");
        vtx_main.push_str("\n}\n");
        pix_main.push_str("\n}\n");

        vtx_final.push_str("\n// Methods\n");

        // push the method deps
        vtx_final.push_str(
            &self.combine_deps(&vtx_deps)
        );
        pix_final.push_str(
            &self.combine_deps(&pix_deps)
        );

        vtx_final.push_str(&vtx_main);

        pix_final.push_str(&pix_main);
        // push the main functions
        if self.log != 0{
            println!("---------- Vertex Shader ------- {}",  vtx_final);
            // push the main functions
            println!("---------- Pixel Shader --------- {}",  pix_final);
        }
        vtx_final.push_str("\0");
        pix_final.push_str("\0");
        CompiledShader{
            geom_slots:geom_slots,
            inst_slots:inst_slots,
            geom_attribs:Shader::ceil_div4(geom_slots),
            inst_attribs:Shader::ceil_div4(inst_slots),
            fragment:pix_final,
            vertex:vtx_final
        }
    }

    pub fn new()->Shader{
        Shader{..Default::default()}
    }

    pub fn def()->Shader{
        let sh = Shader{..Default::default()};
        // lets add the default library
        sh
    }

    pub fn draw_lib(sh:&mut Shader){
        
        sh.define("PI","3.141592653589793");
		sh.define("E","2.718281828459045");
		sh.define("LN2","0.6931471805599453");
		sh.define("LN10","2.302585092994046");
		sh.define("LOG2E","1.4426950408889634");
		sh.define("LOG10E","0.4342944819032518");
		sh.define("SQRT1_2","0.70710678118654757");
		sh.define("TORAD","0.017453292519943295");
		sh.define("GOLDEN","1.618033988749895");

        sh.local("draw_pos", Kind::Vec2);
        sh.local("draw_last_pos", Kind::Vec2);
        sh.local("draw_start_pos", Kind::Vec2);
        sh.local("draw_shape", Kind::Float);
        sh.local("draw_shape_old", Kind::Float);
        sh.local("draw_blur", Kind::Float);
        sh.local("draw_antialias", Kind::Float);
        sh.local("draw_scale", Kind::Float);
        sh.local("draw_field", Kind::Float);
        sh.method("
            vec2 draw_viewport(vec2 pos){
                draw_pos = pos;
                draw_result = vec4(0.);
                draw_shape_old =
                draw_shape = 1e+20;
                draw_blur = 0.00001;
                draw_aa = draw_antialias(pos);
                draw_scale = 1.0;
                draw_field = 0.0;
            }
        ");
        sh.method("
            float draw_antialias(vec2 p){
                return 1.0 / length(vec2(length(dFdx(p)), length(dFdy(p))));
            }
        ");
        sh.method("
            vec2 draw_translate(float x, float y){
                draw_pos -= vec2(x, y);
            }
        ");
        sh.method("
            void draw_rotate(float a, float x, float y) {$
                float ca = cos(-a);
                float sa = sin(-a);
                vec2 p = draw_pos - vec2(x, y);
                draw_pos = vec2(p.x * ca - p.y * sa, p.x * sa + p.y * ca) + vec2(x, y);
            }
        ");
        sh.method("
            void draw_scale(float f, float x, float y) {$
                draw_scale *= f
                draw_pos = (draw_pos - vec2(x, y)) * f + vec2(x, y)
            }
        ");
        sh.method("
            void draw_clear(vec4 color){
                draw_result = vec4(color.rgb * color.a + draw_result.rgb * (1. - color.a), color.a);
            }
        ");
        sh.method(
            "void draw_calc_blur(float w) {
                float f = w - draw_blur;
                float wa = clamp(-w * draw_aa, 0., 1.);
                float wb = draw_blur < 0.0001?1.0:clamp(-w / draw_blur, 0., 1.);
                return wa * wb;
            }"
        );
        sh.method("
            void draw_fill_keep(vec4 color) {
                float f = draw_calc_blur(draw_shape);
                float source = vec4(color.rgb * color.a, color.a);
                float dest = draw_result;
                draw_result = source * f + dest * (1. - source.a * f);
            }
        ");
        sh.method("
            void draw_fill(vec4 color) {
                draw_fill_keep(color);
                draw_old_shape = draw_shape = 1e+20;
            }
        ");
        sh.method("
            void draw_stroke_keep(vec4 color, float width) {
                float f = draw_calc_blur(abs(draw_shape) - width / draw_scale);
                vec4 source = vec4(color.rgb * color.a, color.a);
                vec4 dest = draw_result;
                draw_result = source * f + dest * (1. - source.a * f);
            }
        ");
        sh.method("
            void draw_stroke(vec4 color, float width) {
                draw_stroke_keep(color, width);
                draw_old_shape = draw_shape = 1e+20;
            }
        ");
        sh.method("
            void draw_glow_keep(vec4 color, float width) {
                float f = draw_calc_blur(abs(draw_shape) - width / draw_scale);
                vec4 source = vec4(color.rgb * color.a, color.a);
                vec4 dest = draw_result;
                draw_result = vec4(source.rgb * f, 0.) + dest;
            }
        ");
        sh.method("
            void draw_glow(vec4 color, float width) {$
                draw_glow_keep(color, width);
                draw_old_shape = draw_shape = 1e+20;
            }
        ");
        sh.method("
            void draw_union() {
                draw_old_shape = draw_shape = min(draw_field, draw_old_shape);
            }
        ");
        sh.method("
            void draw_intersect() {
                draw_old_shape = draw_shape = max(draw_field, draw_old_shape);
            }
        ");
        sh.method("
            void subtract() {
                draw_old_shape = draw_shape = max(-draw_field, draw_old_shape);
            }
        ");
        sh.method("
            void gloop(float k) {
                var h = clamp(.5 + .5 * (draw_old_shape - draw_field) / k, 0., 1.);
                draw_old_shape = draw_shape = mix(draw_old_shape, draw_field, h) - k * h * (1.0 - h);
            }
        ");
        sh.method("
            void blend(float k){
                draw_old_shape = draw_shape = mix(draw_old_shape, draw_field, k);
            }
        ");
        sh.method("
            void circle(float x, float y, float r) {
                vec2 c = draw_pos - vec2(x, y);
                draw_field = (length(c.xy) - r) / draw_scale;
                draw_old_shape = draw_shape;
                draw_shape = min(draw_shape, draw_field);
            }
        ");
        sh.method("
            void box(float x, float y, float w, float h, float r) {
                vec2 p = draw_pos - vec2(x, y);
                vec2 size = vec2(.5 * w, .5 * h);
                vec2 bp = max(abs(p - size.xy) - (size.xy - vec2(2. * r).xy), vec2(0.));
                draw_field = (length(bp) - 2. * r) / draw_scale;
                draw_old_shape = draw_shape;
                draw_shape = min(draw_shape, draw_field);
            }
        ");
        sh.method("
            void rect(float x, float y, float w, float h) {
                vec2 s = vec2(w, h) * .5;
                vec2 d = abs(vec2(x, y) - this.pos + s) - s;
                vec2 dm = min(d, vec2(0.));
                draw_field = max(dm.x, dm.y) + length(max(d, vec2(0.)));
                draw_old_shape = draw_shape;
                draw_shape = min(draw_shape, draw_field);
            }
        ");
        sh.method("
            void draw_move_to(float x, float y) {
                draw_last_pos =
                draw_start_pos = vec2(x, y);
            }
        ");
        sh.method("
            void draw_line_to(float x, float y) {
                vec2 p = vec2(x, y);

                vec2 pa = draw_pos - draw_last_pos;
                vec2 ba = p - draw_last_pos;
                float h = clamp(dot(pa, ba) / dot(ba, ba), 0., 1.);
                draw_field = length(pa - ba * h) / draw_scale;
                draw_old_shape = draw_shape;
                draw_shape = min(draw_shape, draw_field);
                draw_last_pos = p;
            }
        ");
        sh.method("
            void draw_close_path() {
                draw_line_to(draw_start_pos.x, draw_start_pos.y);
            }
        ");
        sh.method("    
            vec4 draw_hsv2rgb(vec4 c) { //http://gamedev.stackexchange.com/questions/59797/glsl-shader-change-hue-saturation-brightness
                vec4 K = vec4(1., 2. / 3., 1. / 3., 3.);
                vec4 p = abs(fract(c.xxx + K.xyz) * 6. - K.www);
                return vec4(c.z * mix(K.xxx, clamp(p - K.xxx, 0., 1.), c.y), c.w);
            }
        ");
        sh.method("    
            vec4 draw_rgb2hsv(vec4 c) {
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
