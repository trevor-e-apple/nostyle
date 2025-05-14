pub mod error;

use error::TypeError;

use crate::parse::ast::Ast;

pub fn type_check(ast: &Ast) -> Result<(), Vec<TypeError>> {
    todo!()
}

fn function_level_type_check() -> Result<(), TypeError> {
    todo!()
}
