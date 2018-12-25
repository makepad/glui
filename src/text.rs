use crate::math::*;
use crate::shader::*;
use crate::cx::*;

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
        Self::def_shader(&mut sh);
        Self{
            shader_id:cx.shaders.add(sh),
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
    pub fn def_shader(sh: &mut Shader){
        // lets add the draw shader lib
        Cx::def_shader(sh);
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
        
        sh.sampler("font_sampler", Sampler::Sampler2D);
        sh.uniform("font_size", Kind::Vec2);

        sh.instance("x1", Kind::Float);
        sh.instance("y1", Kind::Float);
        sh.instance("x2", Kind::Float);
        sh.instance("y2", Kind::Float);
        sh.instance("tx1", Kind::Float);
        sh.instance("ty1", Kind::Float);
        sh.instance("tx2", Kind::Float);
        sh.instance("ty2", Kind::Float);
        sh.instancev("color", Kind::Vec4);
        
        sh.varying("tex_coord", Kind::Vec2);

        sh.method("
            vec4 pixel(){
               // return vec4(1.0,1.0,0.0,1.0);
                return texture2D(font_sampler, tex_coord);
            }
        ");
        sh.method("
            vec4 vertex(){
                tex_coord = pos;
                return vec4(pos,0.,1.);
            }
        ");
        sh.log =1;
    }

    pub fn draw_text(&mut self, cx:&mut Cx, text:&str){
        let dr = cx.drawing.instance(cx.shaders.get(self.shader_id));
        let font = cx.fonts.get(self.font_id, &mut cx.textures);

        if dr.first{
            dr.usampler("font_sampler", font.texture_id);
            dr.uvec2f("font_size", font.width as f32, font.height as f32);
        }

        dr.float("x1",0.);
        dr.float("y1",0.);
        dr.float("x2",0.);
        dr.float("y2",0.);
        dr.float("tx1",0.);
        dr.float("ty1",0.);
        dr.float("tx2",0.);
        dr.float("ty2",0.);
        dr.vec4f("color",1.0,1.0,1.0,1.0);

    }
}