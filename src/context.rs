use crate::shader::*;

#[derive(Default,Clone)]
pub struct Cx{
    pub shader_table: Vec<Shader>
}

impl Cx{
    pub fn add_shader(&mut self, sh:&Shader)->usize{
        let id = self.shader_table.len();
        self.shader_table.push(sh.clone());
        id
    }

    pub fn new()->Cx{
        return Cx{..Default::default()}
    }
}

pub struct Ev{
}

pub trait Style{
    fn style(cx:&mut Cx) -> Self;
}

#[derive(Default,Clone)]
pub struct Draw{ // draw info per UI element
    pub id:usize
}

impl Draw{
    pub fn begin(&mut self, cx:&mut Cx){
    }

    pub fn end(&mut self, cx:&mut Cx){
    }
}
