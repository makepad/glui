use proc_macro_hack::proc_macro_hack;

#[proc_macro_hack]
pub use shader_ast_impl::shader_ast;

pub mod ast_types;
pub use crate::ast_types::*;