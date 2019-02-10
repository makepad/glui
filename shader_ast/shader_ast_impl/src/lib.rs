#![feature(proc_macro_hygiene)]

extern crate proc_macro;
extern crate proc_macro2;
use proc_macro_hack::proc_macro_hack;
use proc_macro2::TokenStream;
use proc_macro2::Span;
use syn::{
    Expr, Type, Pat, Stmt, PathArguments, GenericArgument, 
    Item, Local, ItemFn, ItemConst, ItemStruct,
    Lit, Block, FnArg, BinOp, UnOp, Ident, ReturnType, Member
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

fn process_fn_def(item:ItemFn)->TokenStream{
    // alright lets do a function
    // and then incrementally add all supported ast nodes
    let name = item.ident.to_string();
       let mut args = Vec::new();
    // lets process the fnargs
    for arg in &item.decl.inputs{
        if let FnArg::Captured(arg) = arg{
            // lets look at pat and ty
            if let Pat::Ident(pat) = &arg.pat{
                let name =  pat.ident.to_string();
                let tyc;
                if let Type::Path(typath) = &arg.ty{
                    if typath.path.segments.len() != 1{
                        return error(typath.span(), "arg type not simple");
                    }
                    let seg = &typath.path.segments[0];
                    tyc = seg.ident.to_string();
                }
                else{
                    return error(arg.span(), "arg type not simple");
                }
                args.push(quote!{
                    ShFnArg{
                        name:#name.to_string(),
                        ty:#tyc.to_string()
                    }
                })
            }
            else{
                return error(arg.span(), "arg pattern not simple identifier")
            }
        }
        else{
             return error(arg.span(), "arg pattern not simple identifier")
        }
    }
    let rtype;
    if let ReturnType::Type(_, ty) = item.decl.output{
        if let Type::Path(typath) = *ty{
            if typath.path.segments.len() != 1{
                return error(typath.span(), "return type not simple");
            }
            let seg = &typath.path.segments[0];
            rtype = seg.ident.to_string();
        }
        else{
            return error(ty.span(), "return type not simple");
        }
    }   
    else{
        rtype = "void".to_string();
        //return error(item.span(), "function needs to specify return type")
    }
    let block = process_block(*item.block);
    quote!{
        ShFn{
            name:#name.to_string(),
            args:vec![#(#args),*],
            ret:#rtype.to_string(),
            block:#block
        }
    }
}

fn process_let(local:Local)->TokenStream{
    // lets define a local with storage specified
    if let Pat::Ident(pat) = &local.pats[0]{
        let name =  pat.ident.to_string();
        let tyc;
        if let Some((_tok, ty)) = local.ty.clone(){
            if let Type::Path(typath) = *ty{
                if typath.path.segments.len() != 1{
                    return error(typath.span(), "type not simple");
                }
                let seg = &typath.path.segments[0];
                tyc = seg.ident.to_string();
            }
            else{
                return error(local.span(), "type missing or malformed");
            }
        }
        else{
            return error(local.span(), "let pattern misses type info");
        }
        let init;
        if let Some((_,local_init)) = local.init{
            init = process_expr(*local_init);
        }
        else{
            return error(local.span(), "let pattern misses initializer");
        }
        return quote!{
            ShLet{
                name:#name.to_string(),
                ty:#tyc.to_string(),
                init:Box::new(#init)
            }
        }
    }
    else{
        return error(local.span(), "let pattern not simple identifier")
    }
}

fn process_block(block:Block)->TokenStream{
    let mut stmts = Vec::new();
    for stmt in block.stmts{
        match stmt{
            Stmt::Local(stmt)=>{
                let letstmt = process_let(stmt);
                stmts.push(quote!{
                    ShStmt::ShLet(#letstmt)
                })
            }
            Stmt::Item(stmt)=>{
                return error(stmt.span(), "Shader functions don't support items");
            }
            Stmt::Expr(stmt)=>{
                let expr = process_expr(stmt);
                stmts.push(quote!{
                    ShStmt::ShExpr(#expr)
                })
            }
            Stmt::Semi(stmt, _tok)=>{
                let expr = process_expr(stmt);
                stmts.push(quote!{
                    ShStmt::ShSemi(#expr)
                })
            }
        }
    }
    return quote!{
        ShBlock{
            stmts:vec![#(Box::new(#stmts)),*]
        }
    }
}

fn get_binop(op:BinOp)->&'static str{
    match op{
        BinOp::Add(_)=>"Add",
        BinOp::Sub(_)=>"Sub",
        BinOp::Mul(_)=>"Mul",
        BinOp::Div(_)=>"Div",
        BinOp::Rem(_)=>"Rem",
        BinOp::And(_)=>"And",
        BinOp::Or(_)=>"Or",
        BinOp::BitXor(_)=>"BitXor",
        BinOp::BitAnd(_)=>"BitAnd",
        BinOp::BitOr(_)=>"BitOr",
        BinOp::Shl(_)=>"Shl",
        BinOp::Shr(_)=>"Shr",
        BinOp::Eq(_)=>"Eq",
        BinOp::Lt(_)=>"Lt",
        BinOp::Le(_)=>"Le",
        BinOp::Ne(_)=>"Ne",
        BinOp::Ge(_)=>"Ge",
        BinOp::Gt(_)=>"Gt",
        BinOp::AddEq(_)=>"AddEq",
        BinOp::SubEq(_)=>"SubEq",
        BinOp::MulEq(_)=>"MulEq",
        BinOp::DivEq(_)=>"DivEq",
        BinOp::RemEq(_)=>"RemEq",
        BinOp::BitXorEq(_)=>"BitXorEq",
        BinOp::BitAndEq(_)=>"BitAndEq",
        BinOp::BitOrEq(_)=>"BitOrEq",
        BinOp::ShlEq(_)=>"ShlEq",
        BinOp::ShrEq(_)=>"ShrEq",
    }
}

fn process_expr(expr:Expr)->TokenStream{
    match expr{
        Expr::Call(expr)=>{
            if let Expr::Path(func) = *expr.func{
                if func.path.segments.len() != 1{
                    return error(func.span(), "call identifier not simple");
                }
                let seg = &func.path.segments[0].ident.to_string();
                // lets get all fn args
                let mut args = Vec::new();
                for arg in expr.args{
                    args.push(process_expr(arg));
                }
                return quote!{ShExpr::ShCall(ShCall{call:#seg.to_string(), args:vec![#(Box::new(#args)),*]})}
            }
            else{
                 return error(expr.span(), "call identifier not simple");
            }
        }
        Expr::Binary(expr)=>{
            let left = process_expr(*expr.left);
            let right = process_expr(*expr.right);
            let op = Ident::new(get_binop(expr.op), Span::call_site());
            return quote!{ShExpr::ShBinary(ShBinary{left:Box::new(#left),op:ShBinOp::#op,right:Box::new(#right)})}
        }
        Expr::Unary(expr)=>{
            let op;
            if let UnOp::Not(_) = &expr.op{
                op = Ident::new("Not", Span::call_site());
            }
            else if let UnOp::Neg(_) = &expr.op{
                op = Ident::new("Neg", Span::call_site());
            }
            else {
                return error(expr.span(), "Deref not implemented");
            }
            let right = process_expr(*expr.expr);
            return quote!{ShExpr::ShUnary(ShUnary{op:ShUnaryOp::#op,expr:Box::new(#right)})}
        }
        Expr::Lit(expr)=>{
            match expr.lit{
                Lit::Str(lit)=>{
                    let value = lit.value();
                    return quote!{ShExpr::ShLit(ShLit::ShLitStr(#value.to_string()))}
                }
                Lit::Int(lit)=>{
                    let value = lit.value() as i64;
                    return quote!{ShExpr::ShLit(ShLit::ShLitInt(#value))}
                }
                Lit::Float(lit)=>{
                    let value = lit.value() as f64;
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
        }
        Expr::Let(expr)=>{
            return error(expr.span(), "Not implemented Expr::Let")
        }
        Expr::If(expr)=>{
            let cond = process_expr(*expr.cond);
            let then_branch = process_block(expr.then_branch);

            if let Some((_,else_branch)) = expr.else_branch{
                let else_branch = process_expr(*else_branch);
                return quote!{
                    ShExpr::ShIf(ShIf{
                        cond:Box::new(#cond),
                        then_branch:#then_branch,
                        else_branch:Some(Box::new(#else_branch))
                    })
                }
            }
            return quote!{
               ShExpr::ShIf(ShIf{
                   cond:Box::new(#cond),
                   then_branch:Box::new(#then_branch),
                   else_branch:None
                })
            }
        }
        Expr::While(expr)=>{
            let cond = process_expr(*expr.cond);
            let block = process_block(expr.body);
            return quote!{
               ShExpr::ShWhile(ShWhile{
                   cond:Box::new(#cond),
                   body:#block
                })
            }
        }
        Expr::ForLoop(expr)=>{
              // lets define a local with storage specified
            let span = expr.span();
            if let Pat::Ident(pat) = *expr.pat{
                let name =  pat.ident.to_string();
                let body = process_block(expr.body);
                let from_ts;
                let to_ts;
                if let Expr::Range(range) = *expr.expr{
                    if let Some(from) = range.from {
                        from_ts = process_expr(*from);
                    }
                    else{
                        return error(span, "Must provide from range expression")
                    }
                    if let Some(to) = range.to {
                        to_ts = process_expr(*to);
                    }
                    else{
                        return error(span, "Must provide to range expression")
                    }
                }
                else{
                    return error(span, "Must provide range expression")
                }
                return quote!{
                    ShExpr::ShForLoop(ShForLoop{
                        iter:#name.to_string(),
                        from:Box::new(#from_ts),
                        to:Box::new(#to_ts),
                        body:#body
                    })
                }
            }
            else{
                return error(expr.span(), "Use simple identifier for for loop")
            }
        }
        Expr::Assign(expr)=>{
            let left = process_expr(*expr.left);
            let right = process_expr(*expr.right);
            return quote!{ShExpr::ShAssign(ShAssign{left:Box::new(#left),right:Box::new(#right)})}
        }
        Expr::AssignOp(expr)=>{
            let left = process_expr(*expr.left);
            let right = process_expr(*expr.right);
            let op = Ident::new(get_binop(expr.op), Span::call_site());
            return quote!{ShExpr::ShAssignOp(ShAssignOp{left:Box::new(#left),op:ShBinOp::#op,right:Box::new(#right)})}
        }
        Expr::Field(expr)=>{
            let member;
            if let Member::Named(ident) = expr.member{
                member = ident.to_string();
            }
            else{
                return error(expr.span(), "No unnamed members supported")
            }
            let base = process_expr(*expr.base);
            return quote!{ShExpr::ShField(ShField{base:Box::new(#base),member:#member.to_string()})}
        }
        Expr::Index(expr)=>{
            let base = process_expr(*expr.expr);
            let index = process_expr(*expr.index);
            return quote!{ShExpr::ShIndex(ShIndex{base:Box::new(#base),index:Box::new(#index)})}
        }
        Expr::Path(expr)=>{
            if expr.path.segments.len() != 1{
                return error(expr.span(), "type not simple");
            }
            let seg = &expr.path.segments[0].ident.to_string();
            return quote!{ShExpr::ShId(ShId{name:#seg.to_string()})}
        }
        Expr::Paren(expr)=>{
            let expr = process_expr(*expr.expr);
            return quote!{ShExpr::ShParen(ShParen{expr:Box::new(#expr)})}
        }
        Expr::Block(expr)=>{ // process a block expression
            let block = process_block(expr.block); 
            return quote!{ShExpr::ShBlock(#block)}
        }
        Expr::Return(expr)=>{
            if let Some(expr) = expr.expr{
                let expr = process_expr(*expr);
                return quote!{ShExpr::ShReturn(ShReturn{expr:Some(Box::new(#expr))})}
            }
            return quote!{ShExpr::ShReturn(ShReturn{expr:None})}
        }
        Expr::Break(_)=>{
            return quote!{ShExpr::ShBreak(ShBreak{})}

        }
        Expr::Continue(_)=>{
            return quote!{ShExpr::ShContinue(ShContinue{})}
        }
        _=>{
            return error(expr.span(), "Unsupported syntax for shader")
        }
    }
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
    //println!("----- GENERATED FROM MACRO ---- {}", ts.to_string());
    proc_macro::TokenStream::from(ts)
}