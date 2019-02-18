#![allow(dead_code)]

// renderer specific modules
#[cfg(feature = "ogl")]
#[path="cx_ogl.rs"]
mod cx; 
#[cfg(feature = "ogl")]
#[path="cxshaders_ogl.rs"]
mod cxshaders; 
#[cfg(feature = "ogl")]
#[path="cxtextures_ogl.rs"]
mod cxtextures;

#[cfg(feature = "mtl")]
#[path="cx_mtl.rs"]
mod cx; 
#[cfg(feature = "mtl")]
#[path="cxshaders_mtl.rs"]
mod cxshaders; 
#[cfg(feature = "mtl")]
#[path="cxtextures_mtl.rs"]
mod cxtextures;

mod cxshaders_shared;

// shared modules
mod cxdrawing;
mod cxfonts;
mod cxturtle;

mod math;
mod shader;

mod font;
mod rect;
mod text;
mod button;

use crate::cx::*;
use crate::cxdrawing::*;
use crate::button::*;

struct App{
    dn:DrawNode,
    ok:Button
}

impl Style for App{
    fn style(cx:&mut Cx)->Self{
        Self{
            dn:DrawNode{..Default::default()},
            ok:Button{
                ..Style::style(cx)
            }
        }
    }
}

impl App{
    fn handle(&mut self, cx:&mut Cx, ev:&Ev){
        if self.ok.handle_click(cx, ev){
            // do something!
        }
    }

    fn draw(&mut self, cx:&mut Cx){
        self.dn.begin(cx);

        self.ok.draw_with_label(cx, "OK");

        self.dn.end(cx);
    }
}

fn main() {
    let mut cx = Cx{
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