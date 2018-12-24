use crate::math::*;
use crate::shader::*;
use crate::context::*;

pub struct Rect{
    pub shader_id:usize,
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
            shader_id:cx.shaders.add(sh),
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
        CxShaders::def(sh);

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
        // this allocates a uniform slot ID in the call buffer
        sh.uniform("fac", Kind::Float);
        
        sh.instancev("color", Kind::Vec4);
        sh.method("
            vec4 pixel(){
                return color*fac;
            }
        ");
        sh.method("
            vec4 vertex(){
                return vec4(pos*vec2(w, h)+vec2(x, y),0.,1.);
            }
        ");
        sh.log =1;
    }

    pub fn draw_at<'a>(&mut self, cx:&'a mut Cx, x:f32, y:f32, w:f32, h:f32)->&'a mut Draw{

        // how do we write uniforms?
        //let mut wr = cx.instance(self.shader_id);
        let dr = cx.drawing.instance(cx.shaders.get(self.shader_id));

        if dr.first{
            dr.ufloat("fac", 0.1);
        }

        dr.float("x", x);
        dr.float("y", y);
        dr.float("w", w);
        dr.float("h", h);
        dr.vec4("color", &self.color);
        dr
    }
}