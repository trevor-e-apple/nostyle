use std::todo;

use crate::tokenize::tokenize;

mod tokenize;
mod parse;

fn main() {
    todo!("CLI");
    let s = "";
    match tokenize(s) {
        Ok(_) => todo!(),
        Err(_) => todo!(),
    }
}
