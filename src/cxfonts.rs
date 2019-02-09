
use crate::font::*;
use crate::cxtextures::*;

#[derive(Clone, Default)]
pub struct CxFonts{
    pub fonts:Vec<Font>
}

impl CxFonts{
    pub fn get(&mut self, id:usize,tex:&mut CxTextures)->&Font{
        let font = &mut self.fonts[id];
        if font.texture_id == 0{
            font.texture_id = tex.add_2d_static(Filter::Linear, &font.texture,font.width, font.height);
        }
        font
    }

    pub fn load(&mut self, file_name: &str)->usize{
        let id = self.fonts.len();
        self.fonts.push(
            Font{
                ..Font::read(file_name).unwrap()
            }
        );
        id
    }

}
