
#[derive(Default,Clone)]
pub struct ShaderVar{
    pub name:String,
    pub slots:u32
}

#[derive(Default,Clone)]
pub struct ShaderMethod{
    pub ret:String,
    pub name:String,
    pub args:String,
    pub body:String
}

#[derive(Default,Clone)]
pub struct Shader{
    pub base_attr:Vec<ShaderVar>,
    pub inst_attr:Vec<ShaderVar>,
    pub base_slots:u32,
    pub inst_slots:u32,
    pub vary:Vec<ShaderVar>,
    pub uniforms:Vec<ShaderVar>,
    pub methods:Vec<ShaderMethod>
}

impl Shader{
    pub fn base(&mut self, name:&str, slots:u32){
        self.base_attr.push(
            ShaderVar{
                name:name.to_string(),
                slots:slots
            }
        );
        self.base_slots = self.inst_slots + slots
    }
    
    pub fn inst(&mut self, name:&str, slots:u32){
        self.inst_attr.push(
            ShaderVar{
                name:name.to_string(),
                slots:slots
            }
        );
        self.inst_slots = self.inst_slots + slots
    }

    pub fn method(&mut self, ret:&str, name:&str, args:&str, body:&str){
        self.methods.push(
            ShaderMethod{
                ret:ret.to_string(),
                name:name.to_string(),
                args:args.to_string(),
                body:body.to_string()
            }
        )
    }

    pub fn compile(&mut self){

    }

    pub fn new()->Shader{
        Shader{..Default::default()}
    }
}
