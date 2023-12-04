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
brace_expression -> "{" brace_statements? expression "}";
brace_statements -> brace_statements? (brace_expression | statement | if_else);
statement -> SYMBOL "=" expression ";" | expression ";";
if -> "if" expression brace_expression;
if_else -> "if" expression brace_expression "else" expression;
for_loop -> "for" "(" expression ";" expression ";" expression ";" ")" brace_expression;
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

use core::panic;
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
    end: usize, // end is one-past the final included element in the search data
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
            Rule::BraceExpression => {
                parse_brace_expression_rule(tokens, &search_data, &mut stack);
            }
            Rule::BraceStatements => {
                parse_brace_statements_rule(tokens, &search_data, &mut stack);
            }
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
        expand_for_expression_rule(stack, tokens, search_data, end_token);
    } else {
        // try to parse as expression
    }
    todo!()
}

/// helper function for expanding an expression rule that seems to match a for
/// loop
fn expand_for_expression_rule(
    stack: &mut Vec<SearchData>,
    tokens: &Tokens,
    search_data: &SearchData,
    end_token: &Token,
) {
    if *end_token != Token::RBrace {
        todo!("Syntax error");
    }

    let lparen_index = search_data.start + 1;

    // check for leading lparen
    match tokens.get(lparen_index) {
        Some(expected_lparen) => {
            if *expected_lparen != Token::LParen {
                todo!("Syntax error")
            }
        }
        None => todo!("Syntax error"),
    }

    let rparen_index = match find_matching_group_indices(
        tokens,
        &Token::LParen,
        &Token::RParen,
        lparen_index,
    ) {
        Some(rparen_index) => rparen_index,
        None => todo!("Syntax error"),
    };

    // set up brace expression
    {
        let lbrace_index = rparen_index + 1;
        match tokens.get(lbrace_index) {
            Some(expected_lbrace) => {
                if *expected_lbrace != Token::LBrace {
                    todo!("Syntax error");
                }
            }
            None => todo!("Syntax error"),
        }

        let rbrace_index = match find_matching_group_indices(
            tokens,
            &Token::LBrace,
            &Token::RBrace,
            lbrace_index,
        ) {
            Some(rbrace_index) => rbrace_index,
            None => todo!("Syntax error"),
        };

        stack.push(SearchData {
            start: lbrace_index,
            end: rbrace_index,
            rule: Rule::BraceExpression,
        });
    }

    // set up init, condition, increment statements
    {
        let init_semicolon_index = match find_next_token(
            tokens,
            &Token::EndStatement,
            lparen_index,
            rparen_index,
        ) {
            Some(index) => index,
            None => todo!("Syntax error"),
        };
        let condition_semicolon_index = match find_next_token(
            tokens,
            &Token::EndStatement,
            init_semicolon_index,
            rparen_index,
        ) {
            Some(index) => index,
            None => todo!("Syntax error"),
        };
        let increment_semicolon_index = match find_next_token(
            tokens,
            &Token::EndStatement,
            condition_semicolon_index,
            rparen_index,
        ) {
            Some(index) => index,
            None => todo!("Syntax error"),
        };

        stack.push(SearchData {
            start: lparen_index,
            end: init_semicolon_index,
            rule: Rule::Expression,
        });
        stack.push(SearchData {
            start: init_semicolon_index + 1,
            end: condition_semicolon_index,
            rule: Rule::Expression,
        });
        stack.push(SearchData {
            start: condition_semicolon_index + 1,
            end: increment_semicolon_index,
            rule: Rule::Expression,
        });
    }
}

/// parses the brace expression rule
fn parse_brace_expression_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    stack: &mut Vec<SearchData>,
) {
    let first_token = match tokens.get(search_data.start) {
        Some(token) => token,
        None => todo!("Syntax error"),
    };

    if *first_token != Token::LBrace {
        todo!("Syntax error");
    }

    let final_token = match tokens.get(search_data.end) {
        Some(token) => token,
        None => todo!("Syntax error"),
    };

    if *final_token != Token::RBrace {
        todo!("Syntax error");
    }

    let brace_contents_start = search_data.start + 1;
    let brace_contents_end = search_data.end - 1;

    // there are two ways for a group of brace statements to terminate, either
    // a semicolon or an rbrace
    let end_brace_statements_index: Option<usize> = {
        match find_final_matching_level_token(
            tokens,
            &[Token::EndStatement],
            brace_contents_start,
            brace_contents_end,
            &Token::LBrace,
            &Token::RBrace,
        ) {
            Some(final_semicolon_index) => Some(final_semicolon_index),
            None => {
                // find final rbrace
                match find_final_token(
                    tokens,
                    &Token::RBrace,
                    brace_contents_start,
                    brace_contents_end,
                ) {
                    Some(final_rbrace_index) => {
                        if final_rbrace_index == search_data.end - 1 {
                            // this rbrace is for the trailing expression
                            find_final_token(
                                tokens,
                                &Token::RBrace,
                                brace_contents_start + 1,
                                brace_contents_end - 1,
                            )
                        } else {
                            // this rbrace is for the brace statements
                            Some(final_rbrace_index)
                        }
                    }
                    None => None, // no semicolon or rbrace
                }
            }
        }
    };

    match end_brace_statements_index {
        Some(end_brace_statements_index) => {
            // brace statements followed by expression
            stack.push(SearchData {
                start: brace_contents_start,
                end: end_brace_statements_index,
                rule: Rule::BraceStatements,
            });
            stack.push(SearchData {
                start: end_brace_statements_index + 1,
                end: brace_contents_end,
                rule: Rule::Expression,
            });
        }
        None => {
            // expression only
            stack.push(SearchData {
                start: brace_contents_start,
                end: brace_contents_end,
                rule: Rule::Expression,
            });
        }
    }
}

/// parse brace_statements rule
fn parse_brace_statements_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    stack: &mut Vec<SearchData>,
) {
    // brace_statements? (brace_expression | statement | if_else)

    // find brace_statement terminal
    let (non_recursive_start_index, non_recursive_end_index): (usize, usize) = {
        let mut non_recursive_end_index: Option<usize> = None;
        for index in (search_data.start..search_data.end).rev() {
            match tokens.get(index) {
                Some(check_token) => {
                    if *check_token == Token::RBrace
                        || *check_token == Token::EndStatement
                    {
                        non_recursive_end_index = Some(index);
                        break;
                    }
                }
                None => todo!("Syntax error"),
            }
        }

        let non_recursive_end_index = match non_recursive_end_index {
            Some(non_recursive_end_index) => non_recursive_end_index,
            None => todo!("Syntax error"),
        };

        // find the previous RBrace or EndStatement at the same level as the
        // end of the terminal
        match find_prev_matching_level_token(
            tokens,
            &[Token::RBrace, Token::EndStatement],
            search_data.start,
            non_recursive_end_index,
            &Token::LBrace,
            &Token::RBrace,
        ) {
            Some(non_recursive_start_index) => {
                (non_recursive_start_index + 1, non_recursive_end_index)
            }
            None => todo!("Syntax error"),
        }
    };

    // push the preceding statements for a recursive expansion
    stack.push(SearchData {
        start: search_data.start,
        end: non_recursive_start_index - 1,
        rule: Rule::BraceStatements,
    });

    let non_recursive_start_token = match tokens.get(non_recursive_start_index)
    {
        Some(non_recursive_start_token) => non_recursive_start_token,
        None => todo!("Syntax error"),
    };
    let non_recursive_end_token = match tokens.get(non_recursive_end_index) {
        Some(non_recursive_end_token) => non_recursive_end_token,
        None => todo!("Syntax error"),
    };

    let non_recursive_rule = if *non_recursive_start_token == Token::If {
        Rule::IfElse
    } else if *non_recursive_end_token == Token::EndStatement {
        Rule::Statement
    } else if *non_recursive_end_token == Token::RBrace {
        Rule::BraceExpression
    } else {
        todo!("Syntax error")
    };

    stack.push(SearchData {
        start: non_recursive_start_index,
        end: non_recursive_end_index,
        rule: non_recursive_rule,
    })
}

/// finds the indices of the matching rtoken for the first ltoken found at
/// starts_at
fn find_matching_group_indices(
    tokens: &Tokens,
    ltoken: &Token,
    rtoken: &Token,
    starts_at: usize,
) -> Option<usize> {
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
            return Some(index);
        } else {
            index += 1;
        }
    }

    None
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

/// finds the index of the final token between starts_at and ends_at
/// (starts_at <= index < ends_at) that is at the same grouping level as
/// starts_at
///
/// Returns None if not found
fn find_final_matching_level_token(
    tokens: &Tokens,
    matching_tokens: &[Token],
    starts_at: usize,
    ends_at: usize,
    group_start_token: &Token,
    group_end_token: &Token,
) -> Option<usize> {
    let mut result: Option<usize> = None;

    let mut current_level = 0;
    for index in starts_at..ends_at {
        if let Some(check_token) = tokens.get(index) {
            if *check_token == *group_start_token {
                current_level += 1;
            } else if *check_token == *group_end_token {
                current_level -= 1;
            } else if current_level == 0
                && matching_tokens.contains(check_token)
            {
                result = Some(index);
            }

            if current_level < 0 {
                // we have moved outside of the grouping that starts_at was in
                return None;
            }
        } else {
            return None;
        }
    }

    result
}

/// finds the index of the token previous token between starts_at and ends_at
/// (starts_at <= index < ends_at) that is at the same grouping level as ends_at
///
/// returns None if not found
fn find_prev_matching_level_token(
    tokens: &Tokens,
    matching_tokens: &[Token],
    starts_at: usize,
    ends_at: usize,
    group_start_token: &Token,
    group_end_token: &Token,
) -> Option<usize> {
    let mut current_level = 0;

    for index in (starts_at..ends_at).rev() {
        if let Some(check_token) = tokens.get(index) {
            if current_level == 0 && matching_tokens.contains(check_token) {
                return Some(index);
            } else if *check_token == *group_start_token {
                current_level -= 1;
            } else if *check_token == *group_end_token {
                current_level += 1;
            }
        } else {
            return None;
        }
    }

    None
}

/// finds the index of the next token of a certain type
fn find_next_token(
    tokens: &Tokens,
    token: &Token,
    starts_at: usize,
    ends_at: usize,
) -> Option<usize> {
    for index in starts_at..ends_at {
        if let Some(check_token) = tokens.get(index) {
            if *check_token == *token {
                return Some(index);
            }
        }
    }

    None
}
