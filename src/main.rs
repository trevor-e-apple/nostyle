mod parse;
mod tokenize;
mod type_check;

use std::todo;

use crate::parse::parse;
use crate::tokenize::tokenize;
use crate::type_check::type_check;

fn main() {
    todo!("CLI");
    let s = "";
    let tokens = match tokenize(s) {
        Ok(tokens) => tokens,
        Err(_) => {
            todo!();
            return;
        }
    };

    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(_) => todo!(),
    };

    let type_check_result = match type_check(&ast) {
        Ok(type_check_result) => type_check_result,
        Err(_) => todo!(),
    };
}
