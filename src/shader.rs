#[derive(Clone)]
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
    pub base_attr:Vec<ShaderVar>,
    pub inst_attr:Vec<ShaderVar>,
    pub varyings:Vec<ShaderVar>,
    pub uniforms:Vec<ShaderVar>,
    pub methods:Vec<String>
}

impl Shader{
    pub fn base(&mut self, name:&str, kind:Kind){
        self.base_attr.push(
            ShaderVar{
                name:name.to_string(),
                kind:kind
            }
        );
    }
    
    pub fn inst(&mut self, name:&str, kind:Kind){
        self.inst_attr.push(
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

    fn attrib_type(i:usize, total:usize, left:usize)->String{
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

    fn attrib_def(base: &str, slots:usize)->String{
        // ok lets do a ceil
        let mut r = "".to_string();
        let total = Shader::ceil_div4(slots);
        for i in 0..total{
            r.push_str("attribute ");
            r.push_str(&Shader::attrib_type(i, total, slots&3));
            r.push_str(" ");
            r.push_str(base);
            r.push_str(&i.to_string());
            r.push_str(";\n");
        }
        r
    }

    fn attrib_unpack(base: &str, slot:usize, sv:&ShaderVar)->String{
        let mut r = "".to_string();
        // ok we have the slot we start at
        r.push_str("    ");
        r.push_str(&sv.name);
        r.push_str("=");
        r.push_str(&sv.kind.name());
        r.push_str("(");        
        // now we need to grab it from the slots
        for i in 0..sv.kind.slots(){
            // now we need to read .x/y/z/w
            r.push_str(base)
            
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
        let base_slots:usize = self.base_attr.iter().map(|v| v.kind.slots()).sum();
        let inst_slots:usize = self.inst_attr.iter().map(|v| v.kind.slots()).sum();
      
        vtx_final.push_str("\n// Base attributes\n");
        vtx_final.push_str(&Shader::attrib_def("baseattr", base_slots));
  
        vtx_final.push_str("\n// Instance attributes\n");
        vtx_final.push_str(&Shader::attrib_def("instattr", inst_slots));
       
        let mut vtx_main = "vec4 main(){\n".to_string();

        vtx_final.push_str("\n// Base attributes local names\n");
        let mut slot_id = 0;
        for attr in &self.base_attr{
            vtx_final.push_str(&attr.gl_def());
            vtx_main.push_str( &Shader::attrib_unpack("baseattr", slot_id, attr));
            slot_id = slot_id + attr.kind.slots();
        }

        vtx_final.push_str("\n// Instance attributes local names\n");
        let mut slot_id = 0;
        for attr in &self.inst_attr{
            vtx_final.push_str(&attr.gl_def());
            vtx_main.push_str( &Shader::attrib_unpack("instattr", slot_id, attr));
            slot_id = slot_id + attr.kind.slots();
        }

        vtx_final.push_str("\n// Methods\n");

        // push the method deps
        vtx_final.push_str(
            &self.combine_deps(&vtx_deps)
        );
        pix_final.push_str(
            &self.combine_deps(&pix_deps)
        );

        // push the main functions
        println!("Vertex {}",  vtx_final);
        
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
