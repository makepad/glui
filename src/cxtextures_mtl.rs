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
        let mut tex_handle;
        unsafe{
            tex_handle = mem::uninitialized();
            gl::GenTextures(1, &mut tex_handle);
            gl::BindTexture(gl::TEXTURE_2D, tex_handle);
            if let Filter::Nearest = filter{
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::NEAREST as i32);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::NEAREST as i32);
            }
            else if let Filter::Linear = filter{
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
            }
            else{
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR_MIPMAP_LINEAR as i32);
                gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR_MIPMAP_LINEAR as i32);
            }
            gl::TexImage2D(gl::TEXTURE_2D, 0, gl::RGBA as i32, width as i32, height as i32, 0, gl::RGBA, gl::UNSIGNED_BYTE, v.as_ptr() as *const _);
            gl::BindTexture(gl::TEXTURE_2D, 0);
        }
        let id = self.textures.len();
        self.textures.push(
            GLTexture{
                texture_id:id,
                gl_texture:tex_handle
            }
        );
        id
    }
}
