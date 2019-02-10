#![feature(proc_macro_hygiene)]

extern crate proc_macro;
extern crate proc_macro2;
use proc_macro_hack::proc_macro_hack;
use proc_macro2::TokenStream;
use proc_macro2::Span;
use syn::{
    Expr, Type, Pat, Stmt, PathArguments, GenericArgument, 
    Item, Local, ItemFn, ItemConst, ItemStruct,
    Lit
};
use quote::quote;
use quote::quote_spanned;
use syn::spanned::Spanned;

fn error(span:Span, msg: &str)->TokenStream{
    let fmsg = format!("shader_ast: {}", msg);
    quote_spanned!(span=>compile_error!(#fmsg))
}

fn process_var_def(stmt:Local)->TokenStream{
    // lets define a local with storage specified
    if let Pat::Ident(pat) = &stmt.pats[0]{
        let name =  pat.ident.to_string();
        let tyc;
        let store;
        if let Some((_tok, ty)) = stmt.ty.clone(){
            if let Type::Path(typath) = *ty{
                if typath.path.segments.len() != 1{
                    return error(typath.span(), "type not simple");
                }
                let seg = &typath.path.segments[0];
                tyc = seg.ident.to_string();
                // lets read the path args
                if let PathArguments::AngleBracketed(angle) = &seg.arguments{
                    if angle.args.len() != 1{
                        return error(angle.span(), "storage arg incorrect");
                    }
                    let arg = &angle.args[0];
                    if let GenericArgument::Type(ty) = arg{
                        if let Type::Path(typath) = ty{
                            if typath.path.segments.len() != 1{
                                return error(typath.span(), "type not simple");
                            }
                            let seg = &typath.path.segments[0];
                            store = seg.ident.clone();
                        }
                        else{
                            return error(arg.span(), "type store arg not a basic identifier");
                        }
                    }
                    else{
                        return error(arg.span(), "type store arg not a type");
                    }
                }
                else{
                    return error(seg.ident.span(), "type should have storage specifier <>");
                }
            }
            else{
                return error(stmt.span(), "type missing or malformed");
            }
        }
        else{
            return error(stmt.span(), "let pattern misses type info");
        }
        return quote!{
            ShVar{
                name:#name.to_string(),
                ty:#tyc.to_string(),
                store:ShVarStore::#store
            }
        }
    }
    else{
        return error(stmt.span(), "let pattern not simple identifier")
    }
}

fn process_fn_def(_item:ItemFn)->TokenStream{
    // alright lets do a function
    // and then incrementally add all supported ast nodes
    
}

fn process_expr(expr:Expr)->TokenStream{
    match expr{
        Expr::Call(_expr)=>{
        },
        Expr::Binary(_expr)=>{
        },
        Expr::Unary(_expr)=>{
        },
        Expr::Lit(expr)=>{
            match expr.lit{
                Lit::Str(lit)=>{
                    let value = lit.value();
                    return quote!{ShExpr::ShLit(ShLit::ShLitStr(#value.to_string()))}
                }
                Lit::Int(lit)=>{
                    let value = lit.value();
                    return quote!{ShExpr::ShLit(ShLit::ShLitInt(#value))}
                }
                Lit::Float(lit)=>{
                    let value = lit.value();
                    return quote!{ShExpr::ShLit(ShLit::ShLitFloat(#value))}
                }
                Lit::Bool(lit)=>{
                    let value = lit.value;
                    return quote!{ShExpr::ShLit(ShLit::ShLitBool(#value))}
                }
                _=>{
                    return error(expr.span(), "Unsupported literal for shader")
                }
            }
        },
        Expr::Let(_expr)=>{
        },
        Expr::If(_expr)=>{
        },
        Expr::While(_expr)=>{
        },
        Expr::ForLoop(_expr)=>{
        },
        Expr::Assign(_expr)=>{
        },
        Expr::AssignOp(_expr)=>{
        },
        Expr::Field(_expr)=>{
        },
        Expr::Index(_exprx)=>{
        },
        Expr::Path(_expr)=>{
        },
        Expr::Paren(_expr)=>{
        },
        _=>{
            return error(expr.span(), "Unsupported syntax for shader")
        }
    }
    TokenStream::new()
}


fn process_const_def(item:ItemConst)->TokenStream{
    let name = item.ident.to_string();
    let ty;

    if let Type::Path(typath) = *item.ty{
        if typath.path.segments.len() != 1{
            return error(typath.span(), "const type not a basic identifie");
        }
        let seg = &typath.path.segments[0];
        ty = seg.ident.to_string();
    }
    else{
        return error(item.ty.span(), "const type not a basic identifier");
    }

    let expr = process_expr(*item.expr);
    quote!{
        ShConst{
            name:#name.to_string(),
            ty:#ty.to_string(),
            value:#expr
        }
    }
}

fn process_struct_def(_item:ItemStruct)->TokenStream{
    TokenStream::new()
}

fn match_root(expr:Expr)->TokenStream{
    let mut vars = Vec::new();
    let mut fns = Vec::new();
    let mut consts = Vec::new();
    let mut structs = Vec::new();
    match expr {
        Expr::Closure(expr)=>{
            if let Expr::Block(expr) = *expr.body{
                for stmt in expr.block.stmts{
                    match stmt{
                        Stmt::Local(stmt)=>{
                            vars.push(process_var_def(stmt));
                        }
                        Stmt::Item(stmt)=>{
                            match stmt{
                                Item::Struct(item)=>{
                                    structs.push(process_struct_def(item));
                                }
                                Item::Const(item)=>{
                                    consts.push(process_const_def(item));
                                }
                                Item::Fn(item)=>{
                                    fns.push(process_fn_def(item));
                                }
                                _=>{
                                    return error(stmt.span(), "Unexpected statement")
                                }
                            }
                        }
                        Stmt::Expr(stmt)=>{
                             return error(stmt.span(), "Expression not expected here")
                        }
                        Stmt::Semi(stmt, _tok)=>{
                             return error(stmt.span(), "Statement not expected here")
                        }
                    }
                }
            }
            else{
               return error(expr.span(),"Expecting closure with block")
            }

            quote!{ 
                ShAst{
                    structs:vec![],
                    vars:vec![#(#vars),*],
                    consts:vec![#(#consts),*],
                    fns:vec![#(#fns),*]
                }
            }
        }
        _=>{
            error(expr.span(), "Expecting closure")
        }
    }
}

#[proc_macro_hack]
pub fn shader_ast(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    
    let parsed = syn::parse_macro_input!(input as syn::Expr);

    let ts = match_root(parsed);
    println!("----- GENERATED FROM MACRO ---- {}", ts.to_string());
    proc_macro::TokenStream::from(ts)
}

fn _debug_expr(x:&Expr)->&str{
    match x{
        Expr::Box(_x)=>"Expr::Box",
        Expr::InPlace(_x)=>"Expr::InPlace",
        Expr::Array(_x)=>"Expr::Array",
        Expr::Call(_x)=>"Expr::Call",
        Expr::MethodCall(_x)=>"Expr::MethodCall",
        Expr::Tuple(_x)=>"Expr::Tuple",
        Expr::Binary(_x)=>"Expr::Binary",
        Expr::Unary(_x)=>"Expr::Unary",
        Expr::Lit(_x)=>"Expr::Lit",
        Expr::Cast(_x)=>"Expr::Cast",
        Expr::Type(_x)=>"Expr::Type",
        Expr::Let(_x)=>"Expr::Let",
        Expr::If(_x)=>"Expr::If",
        Expr::While(_x)=>"Expr::While",
        Expr::ForLoop(_x)=>"Expr::ForLoop",
        Expr::Loop(_x)=>"Expr::Loop",
        Expr::Match(_x)=>"Expr::Match",
        Expr::Closure(_x)=>"Expr::Closure",
        Expr::Unsafe(_x)=>"Expr::Unsafe",
        Expr::Block(_x)=>"Expr::Block",
        Expr::Assign(_x)=>"Expr::Assign",
        Expr::AssignOp(_x)=>"Expr::AssignOp",
        Expr::Field(_x)=>"Expr::Field",
        Expr::Index(_x)=>"Expr::Index",
        Expr::Range(_x)=>"Expr::Range",
        Expr::Path(_x)=>"Expr::Path",
        Expr::Reference(_x)=>"Expr::Reference",
        Expr::Break(_x)=>"Expr::Break",
        Expr::Continue(_x)=>"Expr::Continue",
        Expr::Return(_x)=>"Expr::Return",
        Expr::Macro(_x)=>"Expr::Macro",
        Expr::Struct(_x)=>"Expr::Struct",
        Expr::Repeat(_x)=>"Expr::Repeat",
        Expr::Paren(_x)=>"Expr::Paren",
        Expr::Group(_x)=>"Expr::Group",
        Expr::Try(_x)=>"Expr::Try",
        Expr::Async(_x)=>"Expr::Async",
        Expr::TryBlock(_x)=>"Expr::TryBlock",
        Expr::Yield(_x)=>"Expr::Yield",
        Expr::Verbatim(_x)=>"Expr::Verbatim"
    }
}

fn _debug_type(x:&Type)->&str{
    match x{
        Type::Slice(_x)=>"Type::Slice",
        Type::Array(_x)=>"Type::Array",
        Type::Ptr(_x)=>"Type::Ptr",
        Type::Reference(_x)=>"Type::Reference",
        Type::BareFn(_x)=>"Type::BareFn",
        Type::Never(_x)=>"Type::Never",
        Type::Tuple(_x)=>"Type::Tuple",
        Type::Path(_x)=>"Type::Path",
        Type::TraitObject(_x)=>"Type::TraitObject",
        Type::ImplTrait(_x)=>"Type::ImplTrait",
        Type::Paren(_x)=>"Type::Paren",
        Type::Group(_x)=>"Type::Group",
        Type::Infer(_x)=>"Type::Infer",
        Type::Macro(_x)=>"Type::Macro",
        Type::Verbatim(_x)=>"Type::Verbatim",
    }
}

fn _debug_item(x:&Item)->&str{
    match x{
        Item::ExternCrate(_x)=>"Item::ExternCrate",
        Item::Use(_x)=>"Item::Use",
        Item::Static(_x)=>"Item::Static",
        Item::Const(_x)=>"Item::Const",
        Item::Fn(_x)=>"Item::Fn",
        Item::Mod(_x)=>"Item::Mod",
        Item::ForeignMod(_x)=>"Item::ForeignMod",
        Item::Type(_x)=>"Item::Type",
        Item::Existential(_x)=>"Item::Existential",
        Item::Struct(_x)=>"Item::Struct",
        Item::Enum(_x)=>"Item::Enum",
        Item::Union(_x)=>"Item::Union",
        Item::Trait(_x)=>"Item::Trait",
        Item::TraitAlias(_x)=>"Item::TraitAlias",
        Item::Impl(_x)=>"Item::Impl",
        Item::Macro(_x)=>"Item::Macro",
        Item::Macro2(_x)=>"Item::Macro2",
        Item::Verbatim(_x)=>"Item::Verbatim",
    }
}