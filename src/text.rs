use crate::math::*;
use crate::shader::*;
use crate::context::*;

pub struct Text{
    pub font_id:usize,
    pub shader_id:usize,
    pub text:String,
    pub color: Vec4,
    pub x: f32,
    pub y: f32,
    pub size: f32
}

impl Style for Text{
    fn style(cx:&mut Cx)->Self{
        let mut sh = Shader::def(); 
        Self::def_shader(cx, &mut sh);
        Self{
            shader_id:0,//cx.shaders.add(sh),
            font_id:cx.fonts.load("ubuntu_regular_256.font"),
            text:"".to_string(),
            x:0.0,
            y:0.0,
            size:10.0,
            color:Vec4{x:1.0,y:1.0,z:1.0,w:1.0}
        }
    }
}

impl Text{
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
        
        sh.sampler2D("font_sampler");
        sh.uniform("font_texture_size", Kind::Vec2);

        sh.instance("x1", Kind::Float);
        sh.instance("y1", Kind::Float);
        sh.instance("x2", Kind::Float);
        sh.instance("y2", Kind::Float);
        sh.instance("tx1", Kind::Float);
        sh.instance("ty1", Kind::Float);
        sh.instance("tx2", Kind::Float);
        sh.instance("ty2", Kind::Float);

        // this allocates a uniform slot ID in the call buffer
        
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

   // pub fn draw_text(&mut self, cx:&mut Cx, text:&str)->InstanceWriter{
   // }
}