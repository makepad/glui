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
        Self::def_shader(cx, &mut sh);
        Self{
            shader:cx.add_shader(&sh),
            id:0,
            x:0.0,
            y:0.0,
            w:1.0,
            h:1.0,
            color:Vec4{x:1.0,y:0.0,z:0.0,w:0.0}
        }
    }
}

impl Rect{
    pub fn def_shader(cx: &mut Cx, sh: &mut Shader){
        // lets add the draw shader lib
        Shader::default_defs(cx, sh);

        sh.geometry_vertices = vec![
            0.0,0.0,
            1.0,0.0,
            1.0,1.0,
            0.0,1.0
        ];
        sh.geometry_indices = vec![
            0,1,2,
            2,3,0
        ];
        sh.geometry("pos", Kind::Vec2);
        sh.instance("x", Kind::Float);
        sh.instance("y", Kind::Float);
        sh.instance("w", Kind::Float);
        sh.instance("h", Kind::Float);
        sh.uniform("fac", Kind::Float, UniBlock::Call);
        
        sh.instancev("color", Kind::Vec4);
        sh.method("
            vec4 pixel(){
                return color;
            }
        ");
        sh.method("
            vec4 vertex(){
                return vec4(pos*vec2(w, h)+vec2(x, y),0.,1.);
            }
        ");
        sh.log =1;
    }

    pub fn draw_at(&mut self, cx:&mut Cx, x:f32, y:f32, w:f32, h:f32)->InstanceWriter{
        let mut wr = cx.instance(self.shader);
        if wr.uniforms{
            wr.ufloat(cx, "fac", 1.0);
        }

        wr.float(cx, "x", x);
        wr.float(cx, "y", y);
        wr.float(cx, "w", w);
        wr.float(cx, "h", h);
        wr.vec4(cx, "color", &self.color);
        wr
    }
}