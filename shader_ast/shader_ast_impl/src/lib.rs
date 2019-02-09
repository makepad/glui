#![feature(proc_macro_hygiene)]

extern crate proc_macro;

use proc_macro_hack::proc_macro_hack;
use proc_macro::*;
use syn::{
    Expr, FnArg, Type, Pat
};
use quote::quote;

fn match_expr(expr:Expr)->TokenStream{
    let out;
    match expr {
        Expr::Closure(expr)=>{
            let mut arg_toks = Vec::new();
            
            for arg in expr.inputs.iter(){
                let mut id = "".to_string();
                let mut ty = "".to_string();
                if let FnArg::Captured(arg) = arg{
                    if let Pat::Ident(pat) = &arg.pat{
                        id = pat.ident.to_string();
                    }
                    if let Type::Path(typath) = &arg.ty{
                        ty = typath.path.segments[0].ident.to_string();
                    }
                }
                arg_toks.push(quote!{
                    FnArg{
                        id:#id.to_string(),
                        ty:#ty.to_string()
                    }
                })
            };
            out = quote!{
                Function{
                    args:vec![#(#arg_toks),*]
                }
            };
        }
        _=>{
            out = quote!{
                unknown
            }
        }
    };
    proc_macro::TokenStream::from(out)
}

#[proc_macro_hack]
pub fn shader_ast(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as syn::Expr);

    let ts = match_expr(parsed);
    
    println!("Macro result: {}", ts.to_string());
    ts
}
