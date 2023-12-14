mod parse;
mod tokenize;

use std::todo;

use crate::parse::parse;
use crate::tokenize::tokenize;

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

    parse(&tokens);
}
