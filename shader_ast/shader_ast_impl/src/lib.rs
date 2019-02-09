#![feature(proc_macro_hygiene)]

extern crate proc_macro;

use proc_macro_hack::proc_macro_hack;
use proc_macro::*;
use syn::{
    Expr, FnArg, Type
};

// lets make a tokentree builder API
struct TokenTreeBuilder{
    stack: Vec<Vec<TokenTree>>
}

impl TokenTreeBuilder{
    fn new()->Self{
        TokenTreeBuilder{
            stack:Vec::<Vec<TokenTree>>::new()
        }
    }
    fn push(&mut self){
        self.stack.push(Vec::<TokenTree>::new());
    }

    fn bracket_pop(&mut self){
        let tree = self.stack.pop().unwrap();
        self.stack.last_mut().unwrap().push(
            TokenTree::Group(
                Group::new(
                    Delimiter::Bracket,
                    tree.into_iter().collect::<TokenStream>()
                )
            )
        )
    }
    
    fn paren_pop(&mut self){
        let tree = self.stack.pop().unwrap();
        self.stack.last_mut().unwrap().push(
            TokenTree::Group(
                Group::new(
                    Delimiter::Parenthesis,
                    tree.into_iter().collect::<TokenStream>()
                )
            )
        )
    }

    fn brace_pop(&mut self){
        let tree = self.stack.pop().unwrap();
        self.stack.last_mut().unwrap().push(
            TokenTree::Group(
                Group::new(
                    Delimiter::Brace,
                    tree.into_iter().collect::<TokenStream>()
                )
            )
        )        
    }

    fn punct(&mut self, c:char){
        self.stack.last_mut().unwrap().push(
            TokenTree::Punct(Punct::new(c, Spacing::Joint))
        )
    }

    fn id(&mut self, name: &str){
        self.stack.last_mut().unwrap().push(
            TokenTree::Ident(Ident::new(name,Span::call_site()))
        )
    }
    
    fn str(&mut self, value: &str){
        self.stack.last_mut().unwrap().push(
            TokenTree::Literal(Literal::string(value))//, Span::call_site())
        )
    }

    fn string(&mut self, value: &str){
        let mut last = self.stack.last_mut().unwrap();
        last.push(
            TokenTree::Literal(Literal::string(value))//, Span::call_site())
        );
        last.push(
            TokenTree::Punct(Punct::new('.', Spacing::Joint))//, Span::call_site())
        );
        last.push(
            TokenTree::Ident(Ident::new("to_string",Span::call_site()))
        );
        last.push(
            TokenTree::Group(
                Group::new(
                    Delimiter::Parenthesis,
                    TokenStream::new()
                )
            )
        );
    }

    fn key(&mut self, name: &str){
        self.id(name);
        self.punct(':');
    }

    fn macr(&mut self, name: &str){
        self.id(name);
        self.punct('!');
    }
}

fn debug_type(ty:Type){
    match ty{
        Type::Slice(x)=>{
            println!("Slice");
        }
        Type::Array(x)=>{
            println!("Array");
        }
        Type::Ptr(x)=>{
            println!("Ptr");
        }        
        Type::Reference(x)=>{
            println!("Reference");
        }        
        Type::BareFn(x)=>{
            println!("BareFn");
        }   
        Type::Never(x)=>{
            println!("Never");
        }   
        Type::Tuple(x)=>{
            println!("Tuple");
        }   
        Type::Path(x)=>{
            println!("Path");
        }   
        Type::TraitObject(x)=>{
            println!("TraitObject");
        }   
        Type::ImplTrait(x)=>{
            println!("ImplTrait");
        }   
        Type::Paren(x)=>{
            println!("Paren");
        }   
        Type::Group(x)=>{
            println!("Group");
        }   
        Type::Infer(x)=>{
            println!("Infer");
        }   
        Type::Macro(x)=>{
            println!("Macro");
        }   
        Type::Verbatim(x)=>{
            println!("Verbatim");
        }   

        _=>{
           
        }
    }
}

fn match_expr(expr:Expr, tb:&mut TokenTreeBuilder){
    match(expr){
        Expr::Closure(expr)=>{
            tb.id("Function");
            tb.push();
            tb.key("args");
            tb.macr("vec");
            tb.push();

            for arg in expr.inputs.iter(){
                tb.id("FnArg");
                tb.push();
                if let syn::FnArg::Captured(arg) = arg{
                    if let syn::Pat::Ident(id) = &arg.pat{
                        tb.key("id");
                        tb.string(&id.ident.to_string());
                        tb.punct(',');
                        tb.key("ty");
                        //println!("{:?}", arg.ty);
                        debug_type(arg.ty.clone());
                        if let Type::Path(ty) = &arg.ty{
                            tb.string(&ty.path.segments[0].ident.to_string());
                        }
                        else{ // throw error

                        }
                    }
                }
                tb.brace_pop();
            }
            tb.bracket_pop();
            tb.brace_pop();
        }
        _=>{
        }
    }
}

#[proc_macro_hack]
pub fn shader_ast(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as syn::Expr);

    let mut tb = TokenTreeBuilder::new();
    tb.stack.push(Vec::<TokenTree>::new());

    let out = match_expr(parsed, &mut tb);
    
    let ts = tb.stack.pop().unwrap().into_iter().collect::<TokenStream>();
    println!("{}", ts.to_string());
    ts
}
