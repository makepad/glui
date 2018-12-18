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

trait Slots{
    fn slots(&self)->usize;
    fn name(&self)->&str;
}

impl Slots for Kind{
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

impl ShaderVar{
    fn gl_def(&self)->String{
        let mut s = "".to_string();
        s.push_str(self.kind.name());
        s.push_str(" ");
        s.push_str(&self.name);
        s.push_str(";\n");
        s
    }
}

#[derive(Default,Clone)]
pub struct CompiledShader{
    pub fragment:String,
    pub vertex:String
}

#[derive(Default,Clone)]
pub struct Shader{
    pub geometries:Vec<ShaderVar>,
    pub instancing:Vec<ShaderVar>,
    pub varyings:Vec<ShaderVar>,
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
    
    pub fn instance(&mut self, name:&str, kind:Kind){
        self.instancing.push(
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

    pub fn find_method(&self, name:&str)->Option<&String>{
        for method in &self.methods{
            if let Some(index) = method.find(' '){
                if &method[(index + 1)..=(index+name.len())] == name{
                    return Some(method)
                }
            }
        }
        None
    }

    pub fn gather_deps(&self, body:&String, deps:&mut Vec<String>){
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

    fn variable_type(i:usize, total:usize, left:usize)->String{
        if i == total - 1{
            match left{
                0...1=>"float",
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
        for svl in list{
            if svl.name == sv.name{
                if svl.kind != sv.kind{
                    panic!()
                }
                return true
            }
        }
        false
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

        let mut vtx_final = "".to_string();
        let mut pix_final = "".to_string();

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
       
        let mut vtx_main = "vec4 main(){\n".to_string();
        let mut pix_main = "vec4 main(){\n".to_string();

        vtx_final.push_str("\n// Geometry attributes local names\n");
        vtx_main.push_str("\n    // Geometry unpacking\n");
        let mut slot_id = 0;
        for var in &self.geometries{
            vtx_final.push_str(&var.gl_def());
            vtx_main.push_str( &Shader::variable_unpack("geomattr", slot_id, geom_slots, var));
            slot_id = slot_id + var.kind.slots();
        }

        vtx_final.push_str("\n// Instance attributes local names\n");
        vtx_main.push_str("\n    // Instance unpacking\n");
        let mut slot_id = 0;
        for var in &self.instancing{
            vtx_final.push_str(&var.gl_def());
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
                vtx_final.push_str(&var.gl_def());
            }  
            // pack it in the vertexshader
            vtx_main.push_str( &Shader::variable_pack("varying", slot_id, vary_slots, var));

            // define in pixelshader
            pix_final.push_str(&var.gl_def());
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
        println!("---------- Vertex Shader ------- {}",  vtx_final);
        // push the main functions
        println!("---------- Pixel Shader --------- {}",  pix_final);
        
        CompiledShader{..Default::default()}
    }

    pub fn new()->Shader{
        Shader{..Default::default()}
    }

    pub fn def()->Shader{
        let sh = Shader{..Default::default()};
        // lets add the default library
        sh
    }
}
