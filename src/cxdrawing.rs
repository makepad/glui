use crate::math::*;
use crate::shader::*;
use crate::cx::*;
use crate::cxshaders::*;

#[derive(Clone, Default)]
pub struct CxDrawing{
    pub draw_lists: Vec<DrawList>,
    pub draw_lists_free: Vec<usize>,
    pub draw_node_stack: Vec<DrawNode>,
    pub draw_list_id: usize,
    pub frame_id: usize
}

impl CxDrawing{
    pub fn draw_list(&mut self)->&mut DrawList{
        &mut self.draw_lists[self.draw_list_id]
    }

    pub fn instance(&mut self, sh:&GLShader)->&mut Draw{
        let draw_list = &mut self.draw_lists[self.draw_list_id];
        
        // find our drawcall in the filled draws
        for i in (0..draw_list.draws_len).rev(){
            if draw_list.draws[i].shader_id == sh.shader_id{
                // reuse this drawcmd.
                let dc = &mut draw_list.draws[i];
                dc.first = false;
                return dc
            }
        }  

        // we need a new draw
        let id = draw_list.draws_len;
        draw_list.draws_len = draw_list.draws_len + 1;
        
        // see if we need to add a new one
        if id >= draw_list.draws.len(){
            draw_list.draws.push(Draw{
                sub_list_id:0,
                shader_id:sh.shader_id,
                instance:Vec::new(),
                uniforms:Vec::new(),
                samplers:Vec::new(),
                first:true,
                update_frame_id:self.frame_id,
                vao:CxShaders::create_vao(sh)
            });
            return &mut draw_list.draws[id];
        }

        // reuse a sub list node
        let draw = &mut draw_list.draws[id];
        // we used to be a sublist, construct vao
        if draw.sub_list_id != 0{
            draw.shader_id = sh.shader_id;
            draw.vao = CxShaders::create_vao(sh);
        }
        // used to be another shader, destroy/construct vao
        else if draw.shader_id != sh.shader_id{
            CxShaders::destroy_vao(&mut draw.vao);
            draw.vao = CxShaders::create_vao(sh);
            draw.shader_id = sh.shader_id;
        }
        // truncate buffers and set update frame
        draw.instance.truncate(0);
        draw.uniforms.truncate(0);
        draw.samplers.truncate(0);
        draw.update_frame_id = self.frame_id;
        draw.first = true;
        draw
    }
}

#[derive(Default,Clone)]
pub struct GLInstanceVAO{
    pub vao:gl::types::GLuint,
    pub vb:gl::types::GLuint
}

#[derive(Default,Clone)]
pub struct Draw{
    pub sub_list_id:usize, // if not 0, its a subnode
    pub shader_id:usize, // if shader_id changed, delete gl vao
    pub instance:Vec<f32>,
    pub uniforms:Vec<f32>,  // draw uniforms
    pub samplers:Vec<usize>,
    pub update_frame_id: usize,
    pub vao:GLInstanceVAO,
    pub first:bool
}

impl Draw{
    pub fn float(&mut self, _name: &str, v:f32){
        self.instance.push(v);
    }

    pub fn vec2f(&mut self, _name: &str, x:f32, y:f32){
        self.instance.push(x);
        self.instance.push(y);
    }

    pub fn vec3f(&mut self, _name: &str, x:f32, y:f32, z:f32){
        self.instance.push(x);
        self.instance.push(y);
        self.instance.push(z);
    }

    pub fn vec4f(&mut self, _name: &str, x:f32, y:f32, z:f32, w:f32){
        self.instance.push(x);
        self.instance.push(y);
        self.instance.push(z);
        self.instance.push(w);
    }

    pub fn vec2(&mut self, _name: &str, v:&Vec2){
        self.instance.push(v.x);
        self.instance.push(v.y);
    }

    pub fn vec3(&mut self, _name: &str, v:&Vec3){
        self.instance.push(v.x);
        self.instance.push(v.y);
        self.instance.push(v.z);
    }

    pub fn vec4(&mut self, _name: &str, v:&Vec4){
        self.instance.push(v.x);
        self.instance.push(v.y);
        self.instance.push(v.z);
        self.instance.push(v.w);
    }

    pub fn usampler(&mut self, _name: &str, texture_id: usize){
        // how do we store these?
        self.samplers.push(texture_id);
    }

    pub fn ufloat(&mut self, _name: &str, v:f32){
        self.uniforms.push(v);
    }

    pub fn uvec2f(&mut self, _name: &str, x:f32, y:f32){
        self.uniforms.push(x);
        self.uniforms.push(y);
    }

    pub fn uvec3f(&mut self, _name: &str, x:f32, y:f32, z:f32){
        self.uniforms.push(x);
        self.uniforms.push(y);
        self.uniforms.push(z);
    }

    pub fn uvec4f(&mut self, _name: &str, x:f32, y:f32, z:f32, w:f32){
        self.uniforms.push(x);
        self.uniforms.push(y);
        self.uniforms.push(z);
        self.uniforms.push(w);
    }

    pub fn uvec2(&mut self, _name: &str, v:&Vec2){
        self.uniforms.push(v.x);
        self.uniforms.push(v.y);
    }

    pub fn uvec3(&mut self, _name: &str, v:&Vec3){
        self.uniforms.push(v.x);
        self.uniforms.push(v.y);
        self.uniforms.push(v.z);
    }

    pub fn uvec4(&mut self, _name: &str, v:&Vec4){
        self.uniforms.push(v.x);
        self.uniforms.push(v.y);
        self.uniforms.push(v.z);
        self.uniforms.push(v.w);
    }

    pub fn umat4(&mut self, _name: &str, v:&Mat4){
        for i in 0..16{
            self.uniforms.push(v.v[i]);
        }
    }
}

// CX and DL uniforms
const DL_UNI_PROP2:usize = 0;
const DL_UNI_SIZE:usize = 1;

#[derive(Default,Clone)]
pub struct DrawList{
    pub draws:Vec<Draw>,
    pub draws_len: usize,
    pub uniforms:Vec<f32> // cmdlist uniforms
}

impl DrawList{
    pub fn initialize(&mut self){
        self.uniforms.resize(DL_UNI_SIZE, 0.0);
    }
    
    pub fn def_uniforms(sh:&mut Shader){
        sh.dl_uniform("prop2", Kind::Float);
    }

    pub fn uniform_prop2(&mut self, v:f32){
        self.uniforms[DL_UNI_PROP2] = v;
    }
}
