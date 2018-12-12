use crate::math::*;
use crate::shader::*;
use crate::context::*;

pub struct Rect{
    pub shader:usize,
    pub id:u32,
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
    pub color: Vec4
}

impl Style for Rect{
    fn style(cx:&mut Cx)->Self{
        let mut sh = Shader::new(); 
        Self::def_shader(&mut sh);
        Self{
            shader:cx.add_shader(&sh),
            id:0,
            x:0.0,
            y:0.0,
            w:0.0,
            h:0.0,
            color:Vec4{x:1.0,y:0.0,z:0.0,w:0.0}
        }
    }
}

struct RectShaderInst{
    x:f32,
    y:f32,
    w:f32,
    h:f32,
    color:Vec4
}

impl Rect{
    pub fn def_shader(sh: &mut Shader){
        sh.inst("x",1);
        sh.inst("y",1);
        sh.inst("z",1);
        sh.inst("w",1);
        sh.inst("color",4);
        sh.method(
            "vec4","pixel","()","{
                return color;
            }"
        );
    }

    pub fn draw_at(&mut self, cx:&mut Cx, x:f32, y:f32){
        // lets get a RectShaderInst slot in our attribute array
        // and fill it

    }
}