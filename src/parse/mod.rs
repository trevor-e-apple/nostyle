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
brace_expression -> "{" brace_statements? expression "}";
brace_statements -> brace_statements? (brace_expression | statement | if_else);
statement -> SYMBOL "=" expression ";" | expression ";";
if_else -> "if" expression brace_expression ("else" expression)?;
for_loop -> "for" "(" expression ";" expression ";" expression ";" ")" brace_expression;
equality -> (equality ("==" | "!=") comparison) | comparison;
comparison -> (comparison (">" | ">=" | "<" | "<=") plus_minus) | plus_minus;
plus_minus -> (plus_minus ("+" | "-") mult_div) | mult_div;
mult_div -> (mult_div ("*" | "/") unary) | unary;
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
data structures
*/

pub mod ast;
pub mod rule;
mod token_search;

use std::todo;

use crate::tokenize::tokens::{Token, Tokens};

use self::{
    ast::{Ast, AstNodeHandle},
    rule::Rule,
    token_search::{
        find_final_matching_level_token, find_final_token,
        find_matching_group_indices, find_matching_group_indices_end,
        find_next_matching_level_token, find_next_token,
        find_prev_matching_level_token,
    },
};

struct SearchData {
    start: usize,
    end: usize, // end is one-past the final included element in the search data
    node_handle: AstNodeHandle,
}

/// parses tokens and returns an abstract syntax tree
pub fn parse(tokens: &Tokens) -> Ast {
    let mut result = Ast::new();

    let root_handle = result.add_root(Rule::Expression);

    let mut stack: Vec<SearchData> = vec![SearchData {
        start: 0,
        end: tokens.len(),
        node_handle: root_handle,
    }];
    while let Some(search_data) = stack.pop() {
        let rule = match result.get_node(search_data.node_handle) {
            Some(node) => node.rule,
            None => todo!("Panic?"),
        };

        match rule {
            Rule::Expression => {
                parse_expression_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::BraceExpression => {
                parse_brace_expression_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::BraceStatements => {
                parse_brace_statements_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::Statement => {
                parse_statement_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::IfElse => {
                parse_if_else_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::ForLoop => {
                parse_for_rule(tokens, &search_data, &mut result, &mut stack);
            }
            Rule::Equality => {
                parse_equality_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::Comparison => {
                parse_comparison_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::PlusMinus => {
                parse_plus_minus_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::MultDiv => {
                parse_mult_div_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::Unary => {
                parse_unary_rule(tokens, &search_data, &mut result, &mut stack);
            }
            Rule::Primary => {
                parse_primary_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
        }
    }

    todo!()
}

fn parse_expression_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
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
    let rule = if *start_token == Token::LBrace {
        Rule::BraceExpression
    } else if *start_token == Token::If {
        Rule::IfElse
    } else if *start_token == Token::For {
        Rule::ForLoop
    } else {
        Rule::Equality
    };

    let child_handle = ast.add_child(search_data.node_handle, rule);

    stack.push(SearchData {
        start: search_data.start,
        end: search_data.end,
        node_handle: child_handle,
    });
}

/// parses the for rule
fn parse_for_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match tokens.get(search_data.end) {
        Some(end_token) => {
            if *end_token != Token::RBrace {
                todo!("Syntax error")
            }
        }
        None => todo!("Syntax error"),
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
        search_data.end,
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
            search_data.end,
        ) {
            Some(rbrace_index) => rbrace_index,
            None => todo!("Syntax error"),
        };

        let child_handle =
            ast.add_child(search_data.node_handle, Rule::BraceExpression);
        stack.push(SearchData {
            start: lbrace_index,
            end: rbrace_index,
            node_handle: child_handle,
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

        let init_expression_handle =
            ast.add_child(search_data.node_handle, Rule::Expression);
        stack.push(SearchData {
            start: lparen_index,
            end: init_semicolon_index,
            node_handle: init_expression_handle,
        });

        let condition_expression_handle =
            ast.add_child(search_data.node_handle, Rule::Expression);
        stack.push(SearchData {
            start: init_semicolon_index + 1,
            end: condition_semicolon_index,
            node_handle: condition_expression_handle,
        });

        let increment_expression_handle =
            ast.add_child(search_data.node_handle, Rule::Expression);
        stack.push(SearchData {
            start: condition_semicolon_index + 1,
            end: increment_semicolon_index,
            node_handle: increment_expression_handle,
        });
    }
}

/// parses the brace expression rule
fn parse_brace_expression_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
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
            let brace_statements_handle =
                ast.add_child(search_data.node_handle, Rule::BraceStatements);
            stack.push(SearchData {
                start: brace_contents_start,
                end: end_brace_statements_index,
                node_handle: brace_statements_handle,
            });
            let expression_handle =
                ast.add_child(search_data.node_handle, Rule::Expression);
            stack.push(SearchData {
                start: end_brace_statements_index + 1,
                end: brace_contents_end,
                node_handle: expression_handle,
            });
        }
        None => {
            let child_handle =
                ast.add_child(search_data.node_handle, Rule::Expression);
            // expression only
            stack.push(SearchData {
                start: brace_contents_start,
                end: brace_contents_end,
                node_handle: child_handle,
            });
        }
    }
}

/// parse brace_statements rule
fn parse_brace_statements_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
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
    let recursive_handle =
        ast.add_child(search_data.node_handle, Rule::BraceStatements);
    stack.push(SearchData {
        start: search_data.start,
        end: non_recursive_start_index - 1,
        node_handle: recursive_handle,
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

    let non_recursive_handle =
        ast.add_child(search_data.node_handle, non_recursive_rule);
    stack.push(SearchData {
        start: non_recursive_start_index,
        end: non_recursive_end_index,
        node_handle: non_recursive_handle,
    })
}

/// parse statement rule
fn parse_statement_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match find_next_token(
        tokens,
        &Token::Assign,
        search_data.start,
        search_data.end,
    ) {
        Some(assign_index) => match tokens.get(search_data.end - 1) {
            Some(expected_end_statement) => {
                if *expected_end_statement == Token::EndStatement {
                    let child_node = ast
                        .add_child(search_data.node_handle, Rule::Expression);
                    stack.push(SearchData {
                        start: assign_index + 1,
                        end: search_data.end - 1,
                        node_handle: child_node,
                    });
                    todo!("Modify AST for assigning to the symbol at search_data.start -> assign_index");
                } else {
                    todo!("Syntax error");
                }
            }
            None => todo!(),
        },
        None => match tokens.get(search_data.end - 1) {
            Some(expected_end_statement) => {
                if *expected_end_statement != Token::EndStatement {
                    let child_node = ast
                        .add_child(search_data.node_handle, Rule::Expression);
                    stack.push(SearchData {
                        start: search_data.start,
                        end: search_data.end - 1,
                        node_handle: child_node,
                    });
                } else {
                    todo!("Syntax error");
                }
            }
            None => todo!("Syntax error"),
        },
    }
}

/// parse the if_else rule
fn parse_if_else_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match tokens.get(search_data.start) {
        Some(expected_if) => {
            if *expected_if != Token::If {
                todo!("Syntax error");
            }
        }
        None => todo!("Syntax error"),
    }

    // check if we have an if-else or just an if
    match find_next_matching_level_token(
        tokens,
        &[Token::Else],
        search_data.start + 1,
        search_data.end,
        &Token::LBrace,
        &Token::RBrace,
    ) {
        Some(else_index) => {
            let else_brace_expression =
                ast.add_child(search_data.node_handle, Rule::BraceExpression);
            stack.push(SearchData {
                start: else_index + 1,
                end: search_data.end,
                node_handle: else_brace_expression,
            });

            let if_lbrace_index = match find_matching_group_indices_end(
                tokens,
                &Token::LBrace,
                &Token::RBrace,
                search_data.start,
                else_index - 1,
            ) {
                Some(lbrace_index) => lbrace_index,
                None => todo!("Syntax error"),
            };
            let if_brace_expression =
                ast.add_child(search_data.node_handle, Rule::BraceExpression);
            stack.push(SearchData {
                start: if_lbrace_index,
                end: else_index - 1,
                node_handle: if_brace_expression,
            });
            let if_condition_expression =
                ast.add_child(search_data.node_handle, Rule::Expression);
            stack.push(SearchData {
                start: search_data.start + 1,
                end: if_lbrace_index - 1,
                node_handle: if_condition_expression,
            });
        }
        None => {
            // find the final rbrace to find the end of the if brace_expression
            let rbrace_index = match find_final_token(
                tokens,
                &Token::RBrace,
                search_data.start + 1,
                search_data.end,
            ) {
                Some(rbrace_index) => rbrace_index,
                None => todo!("Syntax error"),
            };

            // find the matching lbrace for this rbrace
            let lbrace_index = match find_matching_group_indices_end(
                tokens,
                &Token::LBrace,
                &Token::RBrace,
                search_data.start,
                rbrace_index,
            ) {
                Some(lbrace_index) => lbrace_index,
                None => todo!("Syntax error"),
            };

            let if_condition_expression =
                ast.add_child(search_data.node_handle, Rule::Expression);
            stack.push(SearchData {
                start: search_data.start + 1,
                end: lbrace_index - 1,
                node_handle: if_condition_expression,
            });
            let if_brace_expression =
                ast.add_child(search_data.node_handle, Rule::BraceExpression);
            stack.push(SearchData {
                start: lbrace_index + 1,
                end: rbrace_index - 1,
                node_handle: if_brace_expression,
            });
        }
    }
}

/// for parsing binary operations in an expression
fn parse_binary_op_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    matching_tokens: &[Token],
    recursive_rule: Rule,
    next_rule: Rule,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match find_final_matching_level_token(
        tokens,
        matching_tokens,
        search_data.start,
        search_data.end,
        &Token::LParen,
        &Token::RParen,
    ) {
        Some(split_index) => {
            let recursive_node =
                ast.add_child(search_data.node_handle, recursive_rule);
            stack.push(SearchData {
                start: search_data.start,
                end: split_index,
                node_handle: recursive_node,
            });
            let comparison_node =
                ast.add_child(search_data.node_handle, next_rule);
            stack.push(SearchData {
                start: split_index + 1,
                end: search_data.end,
                node_handle: comparison_node,
            });
        }
        None => {
            let comparison_node =
                ast.add_child(search_data.node_handle, next_rule);
            stack.push(SearchData {
                start: search_data.start,
                end: search_data.end,
                node_handle: comparison_node,
            });
        }
    }
}

// parses the equality rule
fn parse_equality_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    parse_binary_op_rule(
        tokens,
        search_data,
        &[Token::BoolEquals, Token::NotEquals],
        Rule::Equality,
        Rule::Comparison,
        ast,
        stack,
    )
}

/// parse comparison rule
fn parse_comparison_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    parse_binary_op_rule(
        tokens,
        search_data,
        &[
            Token::GreaterThan,
            Token::GreaterThanOrEqual,
            Token::LessThan,
            Token::LessThanOrEqual,
        ],
        Rule::Comparison,
        Rule::PlusMinus,
        ast,
        stack,
    )
}

/// parse plus_minus rule
fn parse_plus_minus_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    parse_binary_op_rule(
        tokens,
        search_data,
        &[Token::Plus, Token::Minus],
        Rule::PlusMinus,
        Rule::MultDiv,
        ast,
        stack,
    )
}

/// parse mult div
fn parse_mult_div_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    parse_binary_op_rule(
        tokens,
        search_data,
        &[Token::Times, Token::Divide],
        Rule::MultDiv,
        Rule::Unary,
        ast,
        stack,
    )
}

fn parse_unary_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match tokens.get(search_data.start) {
        Some(first_token) => {
            if *first_token == Token::Not || *first_token == Token::Minus {
                let child_node =
                    ast.add_child(search_data.node_handle, Rule::Unary);
                stack.push(SearchData {
                    start: search_data.start + 1,
                    end: search_data.end,
                    node_handle: child_node,
                });
            } else {
                let child_node =
                    ast.add_child(search_data.node_handle, Rule::Primary);
                stack.push(SearchData {
                    start: search_data.start,
                    end: search_data.end,
                    node_handle: child_node,
                });
            }
        }
        None => todo!("Syntax error"),
    }
}

fn parse_primary_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match tokens.get(search_data.start) {
        Some(token) => match token {
            Token::Symbol(_)
            | Token::IntLiteral(_)
            | Token::FloatLiteral(_)
            | Token::StringLiteral(_) => {
                ast.add_literal_child(
                    search_data.node_handle,
                    Rule::Terminal,
                    *token,
                );
            }
            _ => todo!("Syntax error"),
        },
        None => todo!("Syntax error"),
    }
}
