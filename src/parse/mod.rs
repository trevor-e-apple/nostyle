/*
Grammar

this grammar (will) expands in a way that matches operator precedence

expression -> literal | unary | binary | grouping | SYMBOL;
literal -> NUMBER | STRING | "true" | "false";
unary -> ("-" | "!") expression;
binary -> expression operator expression;
operator -> "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">"" | ">=";
grouping -> "(" expression ")"

expression -> brace_expression | if_else | for_loop | equality;
brace_expression -> "{" brace_contents "}";
brace_contents -> statement brace_contents | expression;
statement -> SYMBOL "=" expression ";" | expression ";";
if_else -> "if" expression brace_expression "else" expression brace_expression;
for_loop -> "for" "(" statement statement statement ")" brace_expression;
equality -> (equality ("==" | "!=") comparison) | comparison;
comparison -> (comparison (">" | ">=" | "<" | "<=") plus_minus) | plus_minus;
plus_minus -> (plus_minus ("+" | "-") mult_div) | mult_div;
mult_div -> (expression ("*" | "/") expression) | boolean;
unary -> (("!" | "-") unary) | primary;
primary -> TRUE | FALSE | SYMBOL | NUMBER | STRING | "(" expression ")";
*/

/* NOTES:
TODO: delete me

what is it we actually need from the parser?
The abstract syntax tree shows the order of operations, and a tree is a natural
way to express that b/c it's a grammar...
but what about flow control? it's just another type of node i suppose
break? early return?
named loops?
function calls?
*/

use crate::tokenize::tokens::{Tokens, Token};

/// finds the indices of the matching rtoken for the first ltoken found at 
/// starts_at
fn find_matching_group_indices(
    tokens: &Tokens, ltoken: &Token, rtoken: &Token, starts_at: usize
) -> usize {
    let mut ltokens_found = 1;
    let mut rtokens_found = 0;

    let mut index = starts_at + 1;
    while let Some(token) = tokens.get(index) {
        if *token == *ltoken {
            ltokens_found += 1;
        } else if *token == *rtoken {
            rtokens_found += 1;
        }

        if ltokens_found == rtokens_found {
            break;
        }
        else {
            index += 1;
        }
    }

    index
}