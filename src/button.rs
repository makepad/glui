//use crate::math::*;
//use crate::shader::*;
use crate::context::*;
use crate::rect::*;

pub struct Button{
    pub draw:Draw,
    pub bg: Rect,
    pub label:String,
    pub did_click: bool
}

impl Style for Button{
    fn style(cx:&mut Cx)->Self{
        Self{
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

        self.bg.draw_at(cx, 0.0, 0.0);

        self.draw.end(cx);
    }
}
