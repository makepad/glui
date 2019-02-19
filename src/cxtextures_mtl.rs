use std::mem;

#[derive(Default,Clone)]
pub struct GLTexture{
    pub texture_id: usize,
    pub gl_texture: gl::types::GLuint
}

#[derive(Clone, Default)]
pub struct CxTextures{
    pub textures:Vec<GLTexture>
}

pub enum Filter{
    Nearest,
    Linear,
    Mipmap
}

impl CxTextures{
    pub fn get(&self, id:usize)->&GLTexture{
        &self.textures[id]
    }

    pub fn add_2d_static(&mut self, filter:Filter, v:&Vec<u32>, width:usize, height:usize)->usize{
        //let id = self.textures.len();
        let id = self.textures.len();
        self.textures.push(
            GLTexture{
                texture_id:id,
                gl_texture:0
            }
        );
        id
    }
}
