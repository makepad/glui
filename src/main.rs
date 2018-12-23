#![allow(dead_code)]
extern crate gl;
extern crate glutin;

mod math;
mod shader;
mod context;
mod rect;
mod button;
use crate::context::*;
use crate::button::*;

struct App{
    draw_cx:DrawCx,
    ok_button:Button
}

impl Style for App{
    fn style(cx:&mut Cx)->Self{
        Self{
            draw:DrawCx{..Default::default()},
            ok_button:Button{
                ..Style::style(cx)
            }
        }
    }
}

impl App{
    fn handle(&mut self, cx:&mut Cx, ev:&Ev){
        if self.ok_button.handle_click(cx, ev){
            // do something!
        }
    }

    fn draw(&mut self, cx:&mut Cx){
        self.draw_cx.begin(cx);

        self.ok_button.draw_with_label(cx, "OK");

        self.draw_cx.end(cx);
    }
}

fn main() {
    let mut cx = Cx{
        running:true,
        title:"Hello World".to_string(),
        ..Default::default()
    };

    let mut app = App{
        ..Style::style(&mut cx)
    };

    cx.event_loop(|cx, ev|{
        if let Ev::Redraw = ev{
            return app.draw(cx);
        }
        app.handle(cx, &ev);
    });
}