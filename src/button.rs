//use crate::math::*;
//use crate::shader::*;
use crate::context::*;
use crate::rect::*;

pub struct Button{
    pub draw:Draw,
    pub time:f32,
    pub bg: Rect,
    pub label:String,
    pub did_click: bool
}

impl Style for Button{
    fn style(cx:&mut Cx)->Self{
        Self{
            time:0.0,
            draw:Draw{..Default::default()},
            label:"OK".to_string(),
            did_click:false,
            bg:Rect{..Style::style(cx)}
        }
    }
}

impl Button{
    pub fn handle(&mut self, _cx:&mut Cx, _ev:&Ev){
        // handle event and figure out if we got clicked
    }

    pub fn handle_click(&mut self, cx:&mut Cx, ev:&Ev)->bool{
        self.handle(cx,ev);
        self.did_click()
    }

    pub fn did_click(&self)->bool{
        self.did_click
    }

    pub fn draw_with_label(&mut self, cx:&mut Cx, _label: &str){
        self.draw.begin(cx);
        self.time = self.time + 0.001;
        for i in 0..1000{
            self.bg.color.x = 0.5+0.5*f32::sin(i as f32 / 10.0+self.time);
            self.bg.color.y = 0.5+0.5*f32::cos(i as f32 / 10.0+self.time);
            self.bg.color.z = 0.5+0.5*f32::cos(i as f32 / 10.0+1.5+self.time);
            self.bg.draw_at(cx, 
                f32::sin(i as f32 / 10.0+self.time), 
                f32::cos(i as f32 / 60.0+self.time),
                0.1, 0.1);
        }

        self.draw.end(cx);
    }
}
