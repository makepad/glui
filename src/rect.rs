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
        let mut sh = Shader::def(); 
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

impl Rect{
    pub fn def_shader(sh: &mut Shader){
        sh.geometry("pos", Kind::Vec2);
        sh.instance("x", Kind::Float);
        sh.instance("y", Kind::Float);
        sh.instance("w", Kind::Float);
        sh.instance("h", Kind::Float);
        sh.instance("color", Kind::Vec4);
        sh.varying("x", Kind::Float);
        sh.varying("color", Kind::Vec4);
        sh.method(
            "vec4 pixel(){
                color2 = color;
                return color;
            }"
        );
        sh.method(
            "vec4 vertex(){
                call1();
                call2();
                return pos;
            }"
        );
        sh.method(
            "vec4 call1(){
                call3();
                return pos;
            }"
        );
    }

    pub fn draw_at(&mut self, cx:&mut Cx, x:f32, y:f32)->InstanceWriter{
        let mut wr = cx.instance(self.shader);
        wr.float(cx, x);
        wr.float(cx, y);
        wr.float(cx, self.w);
        wr.float(cx, self.h);
        wr
    }
}