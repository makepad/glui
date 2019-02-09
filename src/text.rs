use crate::math::*;
use crate::shader::*;
use crate::cx::*;
use crate::cxdrawing::*;

pub struct Text{
    pub font_id:usize,
    pub shader_id:usize,
    pub text:String,
    pub color: Vec4,
    pub font_size:f32,
    pub x: f32,
    pub y: f32
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
            font_size:10.0,
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

        sh.geometry("geom", Kind::Vec2);
        
        sh.sampler("tex_sampler", Sampler::Sampler2D);
        sh.uniform("tex_size", Kind::Vec2);
        sh.uniform("list_clip", Kind::Vec4);

        sh.instance("draw_clip", Kind::Vec4);

        sh.instance("font_g", Kind::Vec4);
        sh.instance("font_t", Kind::Vec4);
        sh.instancev("color", Kind::Vec4);
        sh.instance("x", Kind::Float);
        sh.instance("y", Kind::Float);

        sh.instance("font_size", Kind::Float);
        sh.instance("font_base", Kind::Float);
        
        sh.varying("tex_coord", Kind::Vec2);

        sh.method("
            vec4 pixel(){
                vec4 s = texture2D(tex_sampler, tex_coord);
                float sig_dist =  max(min(s.r, s.g), min(max(s.r, s.g), s.b)) - .5;

                df_viewport(tex_coord * tex_size * 0.07);
                df_shape = -sig_dist - 0.5 / df_aa;

                return df_fill(color);
            }   
        ");
        sh.method("
            vec4 vertex(){
                vec2 shift = vec2(0.0,0.0);
                vec2 min_pos = vec2(
                    x + font_size * font_g.x,
                    y - font_size * font_g.y + font_size * font_base
                );
                vec2 max_pos = vec2(
                    x + font_size * font_g.z,
                    y - font_size * font_g.w + font_size * font_base
                );
                
                vec2 clipped = clamp(
                    mix(min_pos, max_pos, geom) + shift,
                    max(draw_clip.xy, list_clip.xy),
                    min(draw_clip.zw, list_clip.zw)
                );
                vec2 normalized = (clipped - min_pos - shift) / (max_pos - min_pos);

                tex_coord = mix(
                    font_t.xy,
                    font_t.zw,
                    normalized.xy
                );
                return vec4(clipped*vec2(0.01,0.01),0.,1.);
            }
        ");
        sh.log =1;
    }

    pub fn draw_text(&mut self, cx:&mut Cx, text:&str){
        let dr = cx.drawing.instance(cx.shaders.get(self.shader_id));
        let font = cx.fonts.get(self.font_id, &mut cx.textures);
        let turtle = &mut cx.turtle;
        if dr.first{
            dr.usampler("tex_sampler", font.texture_id);
            dr.uvec2f("tex_size", font.width as f32, font.height as f32);
            dr.uvec4f("list_clip", -50000.0,-50000.0,50000.0,50000.0);
        }
        // lets draw 'str' from char a to z
        for c in text.chars(){
            
            if c >= '\u{10000}'{
                continue
            }
            // lets look up the glyph
            let slot = font.unicodes[c as usize];
            if slot == 0 {
                continue
            }
           
            let glyph = &font.glyphs[slot];
            dr.vec4f("draw_clip", -50000.0,-50000.0,50000.0,50000.0);
            dr.vec4f("font_g",glyph.x1 ,glyph.y1 ,glyph.x2 ,glyph.y2);
            dr.vec4f("font_t",glyph.tx1 ,glyph.ty1 ,glyph.tx2 ,glyph.ty2);
            dr.vec4f("color",1.0,1.0,1.0,1.0);
            dr.float("x", turtle.x);
            dr.float("y", turtle.y);
            dr.float("font_size", self.font_size);
            dr.float("font_base", 0.0);
             
            turtle.x += glyph.advance * self.font_size;
        }
     
    }
}