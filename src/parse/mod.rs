/*
Grammar

this grammar (will) expands in a way that matches operator precedence

expression -> literal | unary | binary | grouping | SYMBOL;
literal -> NUMBER | STRING | "true" | "false";
unary -> ("-" | "!") expression;
binary -> expression operator expression;
operator -> "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">"" | ">=";
grouping -> "(" expression ")"

expression -> brace_expression | if | if_else | for_loop | equality;
brace_expression -> "{" brace_statements expression "}";
brace_statements -> brace_statements (brace_expression | statement | if_else | for_loop);
statement -> SYMBOL "=" expression ";" | expression ";";
if -> "if" expression brace_expression;
if_else -> "if" expression brace_expression "else" expression;
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

use std::todo;

use crate::tokenize::tokens::{Token, Tokens};

pub struct Ast {}

pub enum Rule {
    Expression,
    BraceExpression,
    BraceStatements,
    Statement,
    IfElse,
    ForLoop,
    Equality,
    Comparison,
    PlusMinus,
    MultDiv,
    Unary,
    Primary,
}

struct SearchData {
    start: usize,
    end: usize,
    rule: Rule,
}

/// parses tokens and returns an abstract syntax tree
pub fn parse(tokens: &Tokens) -> Ast {
    let mut stack: Vec<SearchData> = vec![SearchData {
        start: 0,
        end: tokens.len(),
        rule: Rule::Expression,
    }];
    while let Some(search_data) = stack.pop() {
        match search_data.rule {
            Rule::Expression => {
                parse_expression_rule(tokens, &search_data, &mut stack);
            }
            Rule::BraceExpression => todo!(),
            Rule::BraceStatements => todo!(),
            Rule::Statement => todo!(),
            Rule::IfElse => todo!(),
            Rule::ForLoop => todo!(),
            Rule::Equality => todo!(),
            Rule::Comparison => todo!(),
            Rule::PlusMinus => todo!(),
            Rule::MultDiv => todo!(),
            Rule::Unary => todo!(),
            Rule::Primary => todo!(),
        }
    }

    todo!()
}

fn parse_expression_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    stack: &mut Vec<SearchData>,
) {
    let start_token = match tokens.get(search_data.start) {
        Some(token) => token,
        None => todo!("Parse error (panic?)"),
    };
    let end_token = match tokens.get(search_data.end) {
        Some(token) => token,
        None => todo!("Parse error (panic?)"),
    };
    if *start_token == Token::LBrace {
        if *end_token != Token::RBrace {
            todo!("Syntax error");
        }

        // search for last endstatement before the rbrace
        match find_final_token(
            tokens,
            &Token::EndStatement,
            search_data.start,
            search_data.end,
        ) {
            Some(expression_start_index) => {
                // expression at end of braces
                stack.push(SearchData {
                    start: expression_start_index,
                    end: search_data.end - 1,
                    rule: Rule::Expression,
                });
                // statements preceding expression
                stack.push(SearchData {
                    start: search_data.start + 1,
                    end: expression_start_index,
                    rule: Rule::BraceStatements,
                })
            }
            None => {
                // no leading statements
            }
        }
    } else if *start_token == Token::If {
        if *end_token != Token::RBrace {
            todo!("Syntax error");
        }

        let else_index: Option<usize> = {
            let mut else_index = None;
            for search_index in search_data.start..search_data.end {
                match tokens.get(search_index) {
                    Some(token) => {
                        if *token == Token::Else {
                            else_index = Some(search_index);
                            break;
                        }
                    }
                    None => todo!("Panic?"),
                }
            }

            else_index
        };

        match else_index {
            Some(else_index) => {
                // verify that else index is preceded by closing bracket
                match tokens.get(else_index - 1) {
                    Some(check_token) => {
                        if *check_token != Token::RBrace {
                            todo!("Syntax error");
                        }
                    }
                    None => todo!("Panic?"),
                };

                // if
                stack.push(SearchData {
                    start: search_data.start + 1,
                    end: else_index - 1,
                    rule: Rule::BraceExpression,
                });

                // else
                stack.push(SearchData {
                    start: else_index + 1,
                    end: search_data.end,
                    rule: Rule::BraceExpression,
                });
            }
            None => stack.push(SearchData {
                start: search_data.start + 1,
                end: search_data.end - 1,
                rule: Rule::BraceExpression,
            }),
        }
    } else if *start_token == Token::For {
        todo!()
    } else {
        // try to parse as expression
    }
    todo!()
}

/// finds the indices of the matching rtoken for the first ltoken found at
/// starts_at
fn find_matching_group_indices(
    tokens: &Tokens,
    ltoken: &Token,
    rtoken: &Token,
    starts_at: usize,
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
        } else {
            index += 1;
        }
    }

    index
}

/// finds the index of the final token between starts_at and ends_at
/// (starts_at <= index < ends_at). Returns None if not found
/// searches in reverse
fn find_final_token(
    tokens: &Tokens,
    token: &Token,
    starts_at: usize,
    ends_at: usize,
) -> Option<usize> {
    for index in (starts_at..ends_at).rev() {
        if let Some(check_token) = tokens.get(index) {
            if *check_token == *token {
                return Some(index);
            }
        } else {
            return None;
        }
    }

    None
}
