/*
Grammar

this grammar expands in a way that matches operator precedence

expression -> brace_expression | if_else | for_loop | equality;
brace_expression -> "{" brace_statements? expression "}";
brace_statements -> brace_statements? (brace_expression | statement | if_else);
statement -> (SYMBOL "=" expression ";") | (expression ";");
if_else -> "if" expression brace_expression ("else" expression)?;
for_loop -> "for" "(" expression ";" expression ";" expression ";" ")" brace_expression;
equality -> (equality ("==" | "!=") comparison) | comparison;
comparison -> (comparison (">" | ">=" | "<" | "<=") plus_minus) | plus_minus;
plus_minus -> (plus_minus ("+" | "-") mult_div) | mult_div;
mult_div -> (mult_div ("*" | "/") unary) | unary;
unary -> (("!" | "-") unary) | primary;
primary -> TRUE | FALSE | SYMBOL | NUMBER | STRING | NONE | "(" expression ")";
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
            Rule::Terminal => {}
        }
    }

    result
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

    let final_token = match tokens.get(search_data.end - 1) {
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
    let end_brace_statements: Option<usize> = {
        match find_final_matching_level_token(
            tokens,
            &[Token::EndStatement],
            brace_contents_start,
            brace_contents_end,
            &Token::LBrace,
            &Token::RBrace,
        ) {
            Some(final_semicolon_index) => Some(final_semicolon_index + 1),
            None => {
                // find final rbrace
                match find_final_token(
                    tokens,
                    &Token::RBrace,
                    brace_contents_start,
                    brace_contents_end,
                ) {
                    Some(final_rbrace_index) => {
                        if final_rbrace_index == brace_contents_end - 1 {
                            // this rbrace is for the trailing expression
                            // find the preceding LBrace and end brace contents there
                            match find_prev_matching_level_token(
                                tokens,
                                &[Token::LBrace],
                                brace_contents_start,
                                final_rbrace_index,
                                &Token::LBrace,
                                &Token::RBrace,
                            ) {
                                Some(lbrace_index) => Some(lbrace_index),
                                None => todo!("Syntax error"),
                            }
                        } else {
                            // this rbrace is for the brace statements
                            Some(final_rbrace_index + 1)
                        }
                    }
                    None => None, // no semicolon or rbrace
                }
            }
        }
    };

    match end_brace_statements {
        Some(end_brace_statements) => {
            // some braces don't have brace statements
            if brace_contents_start < end_brace_statements {
                // brace statements followed by expression
                let brace_statements_handle = ast
                    .add_child(search_data.node_handle, Rule::BraceStatements);
                stack.push(SearchData {
                    start: brace_contents_start,
                    end: end_brace_statements,
                    node_handle: brace_statements_handle,
                });
            }

            let expression_handle =
                ast.add_child(search_data.node_handle, Rule::Expression);
            stack.push(SearchData {
                start: end_brace_statements,
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
        let (end_token, non_recursive_end_index): (Token, usize) = {
            let mut non_recursive_end_data: Option<(Token, usize)> = None;
            for index in (search_data.start..search_data.end).rev() {
                match tokens.get(index) {
                    Some(check_token) => {
                        if *check_token == Token::RBrace
                            || *check_token == Token::EndStatement
                        {
                            non_recursive_end_data =
                                Some((check_token.clone(), index + 1));
                            break;
                        }
                    }
                    None => todo!("Syntax error"),
                }
            }

            match non_recursive_end_data {
                Some(result) => result,
                None => todo!("Syntax error"),
            }
        };

        // find the previous RBrace or EndStatement at the same level as the
        // end of the terminal
        let non_recursive_start_index = if end_token == Token::RBrace {
            // find matching lbrace
            match find_prev_matching_level_token(
                tokens,
                &[Token::LBrace],
                search_data.start,
                non_recursive_end_index - 1,
                &Token::LBrace,
                &Token::RBrace,
            ) {
                Some(lbrace_index) => lbrace_index,
                None => todo!("Syntax error"),
            }
        } else if end_token == Token::EndStatement {
            // find end of previous statement, if it exists
            match find_prev_matching_level_token(
                tokens,
                &[Token::EndStatement],
                search_data.start,
                non_recursive_end_index - 1,
                &Token::LBrace,
                &Token::RBrace,
            ) {
                Some(prev_end_index) => prev_end_index + 1,
                None => search_data.start,
            }
        } else {
            todo!("This should never happen? panic?");
        };

        (non_recursive_start_index, non_recursive_end_index)
    };

    // push the preceding statements for a recursive expansion
    if search_data.start < non_recursive_start_index {
        let recursive_handle =
            ast.add_child(search_data.node_handle, Rule::BraceStatements);
        stack.push(SearchData {
            start: search_data.start,
            end: non_recursive_start_index - 1,
            node_handle: recursive_handle,
        });
    }

    let non_recursive_start_token = match tokens.get(non_recursive_start_index)
    {
        Some(non_recursive_start_token) => non_recursive_start_token,
        None => todo!("Syntax error"),
    };
    let non_recursive_end_token = match tokens.get(non_recursive_end_index - 1)
    {
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
                    // RHS: expand expression
                    let child_node = ast
                        .add_child(search_data.node_handle, Rule::Expression);
                    stack.push(SearchData {
                        start: assign_index + 1,  // move past assignment
                        end: search_data.end - 1, // don't include endstatement token
                        node_handle: child_node,
                    });

                    // LHS: get the symbol for assignment
                    match tokens.get(assign_index - 1) {
                        Some(symbol_token) => match symbol_token {
                            Token::Symbol(_) => ast.add_terminal_child(
                                search_data.node_handle,
                                Some(symbol_token.clone()),
                            ),
                            _ => todo!("syntax error: not a symbol"),
                        },
                        None => todo!("syntax error"),
                    };
                } else {
                    todo!("Syntax error");
                }
            }
            None => todo!("Syntax error?"),
        },
        None => match tokens.get(search_data.end - 2) {
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
    if search_data.start == search_data.end {
        // handle empty expression case
        ast.add_terminal_child(search_data.node_handle, None);
    } else {
        match tokens.get(search_data.start) {
            Some(token) => match token {
                Token::Symbol(_)
                | Token::IntLiteral(_)
                | Token::FloatLiteral(_)
                | Token::StringLiteral(_) => {
                    ast.add_terminal_child(
                        search_data.node_handle,
                        Some(token.clone()),
                    );
                }
                Token::LParen => {
                    let child_handle = ast
                        .add_child(search_data.node_handle, Rule::Expression);

                    let expected_rparen_index = search_data.end - 1;
                    match tokens.get(expected_rparen_index) {
                        Some(expected_rparen) => {
                            // check whether we have mismatched parens
                            if *expected_rparen == Token::RParen {
                                stack.push(SearchData {
                                    start: search_data.start + 1,
                                    end: expected_rparen_index,
                                    node_handle: child_handle,
                                });
                            } else {
                                todo!("Syntax error (mismatched parens)")
                            }
                        }
                        None => todo!("panic?"),
                    }
                }
                _ => todo!("Syntax error"),
            },
            None => todo!("Syntax error"),
        }
        use std::unimplemented;
    }
}

#[cfg(test)]
mod tests {
    use std::println;

    use super::*;

    use crate::tokenize::tokenize;

    /// helper function for adding an expression with nothing but a terminal
    /// to an ast
    fn add_terminal_expression(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
        terminal_value: Option<Token>,
    ) {
        let expression_handle = ast.add_child(parent_handle, Rule::Expression);
        let equality_handle = ast.add_child(expression_handle, Rule::Equality);
        let comparison_handle =
            ast.add_child(equality_handle, Rule::Comparison);
        let plus_minus_handle =
            ast.add_child(comparison_handle, Rule::PlusMinus);
        let mult_div_handle = ast.add_child(plus_minus_handle, Rule::MultDiv);
        let unary_handle = ast.add_child(mult_div_handle, Rule::Unary);
        let primary_child = ast.add_child(unary_handle, Rule::Primary);
        ast.add_terminal_child(primary_child, terminal_value);
    }

    /// test empty parse
    #[test]
    fn empty_parse() {
        let tokens = tokenize("");
        unimplemented!();
    }

    #[test]
    fn empty_braces() {
        let tokens = tokenize("{}");
        unimplemented!();
    }

    #[test]
    fn empty_parens() {
        let tokens = tokenize("()");
        unimplemented!();
    }

    #[test]
    fn empty_statement() {
        let tokens = tokenize(";");
        unimplemented!();
    }

    /// test for mismatched parens
    #[test]
    fn paren_mismatch() {
        unimplemented!();
    }

    #[test]
    fn brace_mismatch() {
        unimplemented!();
    }

    /// test the parse of a single basic token
    #[test]
    fn single_token() {
        let tokens = tokenize("0").expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let equality_handle =
                expected_ast.add_child(root_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);
            let mult_div_handle =
                expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
            let unary_handle =
                expected_ast.add_child(mult_div_handle, Rule::Unary);
            let primary_child =
                expected_ast.add_child(unary_handle, Rule::Primary);
            expected_ast
                .add_terminal_child(primary_child, Some(Token::IntLiteral(0)));
            expected_ast
        };

        ast.print();
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn single_token_in_braces() {
        let tokens = tokenize("{0}").expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);
            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            let equality_handle =
                expected_ast.add_child(expression_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);
            let mult_div_handle =
                expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
            let unary_handle =
                expected_ast.add_child(mult_div_handle, Rule::Unary);
            let primary_child =
                expected_ast.add_child(unary_handle, Rule::Primary);
            expected_ast
                .add_terminal_child(primary_child, Some(Token::IntLiteral(0)));
            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn single_token_nested_braces() {
        let tokens = tokenize("{{0}}").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let outer_brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);
            let expression_handle = expected_ast
                .add_child(outer_brace_expression_handle, Rule::Expression);

            let brace_expression_handle = expected_ast
                .add_child(expression_handle, Rule::BraceExpression);
            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            let equality_handle =
                expected_ast.add_child(expression_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);
            let mult_div_handle =
                expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
            let unary_handle =
                expected_ast.add_child(mult_div_handle, Rule::Unary);
            let primary_child =
                expected_ast.add_child(unary_handle, Rule::Primary);
            expected_ast
                .add_terminal_child(primary_child, Some(Token::IntLiteral(0)));
            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    /// test a simple arithmetic expression
    #[test]
    fn arithmetic_expression() {
        let tokens = tokenize("1 + 2").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let equality_handle =
                expected_ast.add_child(root_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);

            // 1 (recursive)
            {
                let recursive_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::PlusMinus);
                let mult_div_handle =
                    expected_ast.add_child(recursive_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::IntLiteral(1)),
                );
            }
            // 2
            {
                let mult_div_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::IntLiteral(2)),
                );
            }
            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    /// test a simple multiplication expression
    #[test]
    fn mult_expression() {
        let tokens = tokenize("1 * 2").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let equality_handle =
                expected_ast.add_child(root_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);
            let mult_div_handle =
                expected_ast.add_child(plus_minus_handle, Rule::MultDiv);

            // 1 (recursive)
            {
                let mult_div_handle =
                    expected_ast.add_child(mult_div_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::IntLiteral(1)),
                );
            }
            // 2
            {
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::IntLiteral(2)),
                );
            }
            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    /// test for group on right
    #[test]
    fn expression_with_grouping_right() {
        let tokens =
            tokenize("1 + (2 + 3)").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let equality_handle =
                expected_ast.add_child(root_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);

            // LHS: 1
            {
                let recursive_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::Expression);
                let mult_div_handle =
                    expected_ast.add_child(recursive_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::IntLiteral(1)),
                );
            }

            // RHS: (2 + 3)
            {
                let mult_div_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);

                // 2 + 3
                {
                    let expression_handle =
                        expected_ast.add_child(primary_child, Rule::Expression);
                    let equality_handle = expected_ast
                        .add_child(expression_handle, Rule::Equality);
                    let comparison_handle = expected_ast
                        .add_child(equality_handle, Rule::Comparison);
                    let plus_minus_handle = expected_ast
                        .add_child(comparison_handle, Rule::PlusMinus);

                    // 2 (recursive)
                    {
                        let recursive_handle = expected_ast
                            .add_child(plus_minus_handle, Rule::PlusMinus);
                        let mult_div_handle = expected_ast
                            .add_child(recursive_handle, Rule::MultDiv);
                        let unary_handle = expected_ast
                            .add_child(mult_div_handle, Rule::Unary);
                        let primary_child =
                            expected_ast.add_child(unary_handle, Rule::Primary);
                        expected_ast.add_terminal_child(
                            primary_child,
                            Some(Token::IntLiteral(2)),
                        );
                    }
                    // 3
                    {
                        let mult_div_handle = expected_ast
                            .add_child(plus_minus_handle, Rule::MultDiv);
                        let unary_handle = expected_ast
                            .add_child(mult_div_handle, Rule::Unary);
                        let primary_child =
                            expected_ast.add_child(unary_handle, Rule::Primary);
                        expected_ast.add_terminal_child(
                            primary_child,
                            Some(Token::IntLiteral(3)),
                        );
                    }
                }
            }

            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
    }

    /// test for group on left
    #[test]
    fn expression_with_grouping_left() {
        let tokens =
            tokenize("(1 + 2) * 3").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let equality_handle =
                expected_ast.add_child(root_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);
            let mult_div_handle =
                expected_ast.add_child(plus_minus_handle, Rule::MultDiv);

            // LHS: (1 + 2)
            {
                let mult_div_handle =
                    expected_ast.add_child(mult_div_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);

                // 1 + 2
                {
                    let expression_handle =
                        expected_ast.add_child(primary_child, Rule::Expression);
                    let equality_handle = expected_ast
                        .add_child(expression_handle, Rule::Equality);
                    let comparison_handle = expected_ast
                        .add_child(equality_handle, Rule::Comparison);
                    let plus_minus_handle = expected_ast
                        .add_child(comparison_handle, Rule::PlusMinus);

                    // 1 (recursive)
                    {
                        let recursive_handle = expected_ast
                            .add_child(plus_minus_handle, Rule::PlusMinus);
                        let mult_div_handle = expected_ast
                            .add_child(recursive_handle, Rule::MultDiv);
                        let unary_handle = expected_ast
                            .add_child(mult_div_handle, Rule::Unary);
                        let primary_child =
                            expected_ast.add_child(unary_handle, Rule::Primary);
                        expected_ast.add_terminal_child(
                            primary_child,
                            Some(Token::IntLiteral(1)),
                        );
                    }
                    // 2
                    {
                        let mult_div_handle = expected_ast
                            .add_child(plus_minus_handle, Rule::MultDiv);
                        let unary_handle = expected_ast
                            .add_child(mult_div_handle, Rule::Unary);
                        let primary_child =
                            expected_ast.add_child(unary_handle, Rule::Primary);
                        expected_ast.add_terminal_child(
                            primary_child,
                            Some(Token::IntLiteral(2)),
                        );
                    }
                }
            }

            // RHS: 3
            {
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::IntLiteral(3)),
                );
            }

            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn expression_with_brace_grouping() {
        let tokens =
            tokenize("1 + {2 + 3}").expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    /// test an expression including symbols as opposed to literals
    #[test]
    fn expression_with_symbols() {
        let tokens = tokenize("a + b").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let equality_handle =
                expected_ast.add_child(root_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);

            // a (recursive)
            {
                let plus_minus_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::PlusMinus);
                let mult_div_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::Symbol("a".to_owned())),
                );
            }

            // b
            {
                let mult_div_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::Symbol("b".to_owned())),
                );
            }
            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    /// a basic test for assignment
    #[test]
    fn assignment_symbol() {
        let tokens = tokenize("{ a = b; }").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(statements_handle, Rule::Statement);

                // rhs
                {
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let equality_handle = expected_ast
                        .add_child(expression_handle, Rule::Equality);
                    let comparison_handle = expected_ast
                        .add_child(equality_handle, Rule::Comparison);
                    let plus_minus_handle = expected_ast
                        .add_child(comparison_handle, Rule::PlusMinus);
                    let mult_div_handle = expected_ast
                        .add_child(plus_minus_handle, Rule::MultDiv);
                    let unary_handle =
                        expected_ast.add_child(mult_div_handle, Rule::Unary);
                    let primary_child =
                        expected_ast.add_child(unary_handle, Rule::Primary);
                    expected_ast.add_terminal_child(
                        primary_child,
                        Some(Token::Symbol("b".to_owned())),
                    );
                }

                // lhs
                {
                    expected_ast.add_terminal_child(
                        statement_handle,
                        Some(Token::Symbol("a".to_owned())),
                    );
                }
            }

            // no expression at end of brace expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
            );

            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn brace_expression_with_variable_only() {
        let tokens = tokenize("{ a }").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(statements_handle, Rule::Statement);

                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);
                let equality_handle =
                    expected_ast.add_child(expression_handle, Rule::Equality);
                let comparison_handle =
                    expected_ast.add_child(equality_handle, Rule::Comparison);
                let plus_minus_handle =
                    expected_ast.add_child(comparison_handle, Rule::PlusMinus);
                let mult_div_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(primary_child, None);
            }

            // no expression at end of brace expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("a".to_owned())),
            );

            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn brace_expression_with_expression_only() {
        let tokens = tokenize("{ a + b }");
        unimplemented!();
    }

    #[test]
    fn brace_expression_with_variable_statement_only() {
        let tokens = tokenize("{ a; }").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(statements_handle, Rule::Statement);

                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);
                let equality_handle =
                    expected_ast.add_child(expression_handle, Rule::Equality);
                let comparison_handle =
                    expected_ast.add_child(equality_handle, Rule::Comparison);
                let plus_minus_handle =
                    expected_ast.add_child(comparison_handle, Rule::PlusMinus);
                let mult_div_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::Symbol("a".to_owned())),
                );
            }

            // no expression at end of brace expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
            );

            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn brace_expression_statement_only() {
        let tokens = tokenize("{ a + b; }").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(statements_handle, Rule::Statement);

                // a + b
                {
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let equality_handle = expected_ast
                        .add_child(expression_handle, Rule::Equality);
                    let comparison_handle = expected_ast
                        .add_child(equality_handle, Rule::Comparison);
                    let plus_minus_handle = expected_ast
                        .add_child(comparison_handle, Rule::PlusMinus);

                    // a
                    {
                        let recursive_handle = expected_ast
                            .add_child(plus_minus_handle, Rule::PlusMinus);
                        let mult_div_handle = expected_ast
                            .add_child(recursive_handle, Rule::MultDiv);
                        let unary_handle = expected_ast
                            .add_child(mult_div_handle, Rule::Unary);
                        let primary_child =
                            expected_ast.add_child(unary_handle, Rule::Primary);
                        expected_ast.add_terminal_child(
                            primary_child,
                            Some(Token::Symbol("a".to_owned())),
                        );
                    }

                    // b
                    {
                        let mult_div_handle = expected_ast
                            .add_child(plus_minus_handle, Rule::MultDiv);
                        let unary_handle = expected_ast
                            .add_child(mult_div_handle, Rule::Unary);
                        let primary_child =
                            expected_ast.add_child(unary_handle, Rule::Primary);
                        expected_ast.add_terminal_child(
                            primary_child,
                            Some(Token::Symbol("b".to_owned())),
                        );
                    }
                }
            }

            // no expression at end of braces
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
            );

            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected_ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn assign_expression() {
        let tokens =
            tokenize("{ a = b + c; }").expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn assign_brace_expression() {
        let tokens =
            tokenize("a = {b + c};").expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn assign_brace_expression_with_statements() {
        let tokens = tokenize("a = {b = c + d; a + b};")
            .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn left_right_precedence() {
        let tokens = tokenize("a + b - c").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let equality_handle =
                expected_ast.add_child(root_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);

            // a + b
            {
                let a_plus_b_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::PlusMinus);

                // a
                {
                    let recursive_handle = expected_ast
                        .add_child(a_plus_b_handle, Rule::PlusMinus);
                    let mult_div_handle =
                        expected_ast.add_child(recursive_handle, Rule::MultDiv);
                    let unary_handle =
                        expected_ast.add_child(mult_div_handle, Rule::Unary);
                    let primary_child =
                        expected_ast.add_child(unary_handle, Rule::Primary);
                    expected_ast.add_terminal_child(
                        primary_child,
                        Some(Token::Symbol("a".to_owned())),
                    );
                }

                // b
                {
                    let mult_div_handle =
                        expected_ast.add_child(a_plus_b_handle, Rule::MultDiv);
                    let unary_handle =
                        expected_ast.add_child(mult_div_handle, Rule::Unary);
                    let primary_child =
                        expected_ast.add_child(unary_handle, Rule::Primary);
                    expected_ast.add_terminal_child(
                        primary_child,
                        Some(Token::Symbol("b".to_owned())),
                    );
                }
            }

            // - c
            {
                let mult_div_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::Symbol("c".to_owned())),
                );
            }
            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn add_mult_precedence() {
        let tokens = tokenize("a + b * c").expect("Unexpected tokenize error");
        let ast = parse(&tokens);

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let equality_handle =
                expected_ast.add_child(root_handle, Rule::Equality);
            let comparison_handle =
                expected_ast.add_child(equality_handle, Rule::Comparison);
            let plus_minus_handle =
                expected_ast.add_child(comparison_handle, Rule::PlusMinus);

            // a
            {
                let recursive_plus_minus_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::PlusMinus);
                let mult_div_handle = expected_ast
                    .add_child(recursive_plus_minus_handle, Rule::MultDiv);
                let unary_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Unary);
                let primary_child =
                    expected_ast.add_child(unary_handle, Rule::Primary);
                expected_ast.add_terminal_child(
                    primary_child,
                    Some(Token::Symbol("a".to_owned())),
                );
            }

            // b * c
            {
                let mult_div_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::MultDiv);

                // b
                {
                    let recursive_mult_div_handle =
                        expected_ast.add_child(mult_div_handle, Rule::MultDiv);
                    let unary_handle = expected_ast
                        .add_child(recursive_mult_div_handle, Rule::Unary);
                    let primary_child =
                        expected_ast.add_child(unary_handle, Rule::Primary);
                    expected_ast.add_terminal_child(
                        primary_child,
                        Some(Token::Symbol("b".to_owned())),
                    );
                }

                // c
                {
                    let unary_handle =
                        expected_ast.add_child(mult_div_handle, Rule::Unary);
                    let primary_child =
                        expected_ast.add_child(unary_handle, Rule::Primary);
                    expected_ast.add_terminal_child(
                        primary_child,
                        Some(Token::Symbol("c".to_owned())),
                    );
                }
            }
            expected_ast
        };

        println!("ast:");
        ast.print();
        println!("expected ast:");
        expected_ast.print();
        assert!(Ast::equivalent(&ast, &expected_ast));
    }

    #[test]
    fn nested_groups() {
        let tokens =
            tokenize("a * (b - (c + d))").expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn nested_brace_expressions() {
        let tokens = tokenize(
            "
            {
                a = b;
                c = {
                    d = 2 * a;
                    d
                };
                a + b + c
            }
        ",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn nested_brace_expressions_brace_first() {
        let tokens = tokenize(
            "
            {
                c = {
                    d = 2 * a;
                    d
                };
                a = b;
                a + b + c
            }
        ",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn if_only() {
        let tokens = tokenize("if (a + b) == c {d = c;}")
            .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn if_else() {
        let tokens = tokenize(
            "
            if (a + b) == c {
                d = c;
            } else {
                d = a;
            }
        ",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn if_else_if() {
        let tokens = tokenize(
            "
            if a == b {
                d = b;
            } else if a == c {
                d = c;
            } else {
                e = 2 * e;
                d = e;
            }
        ",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn assign_if_else_if() {
        let tokens = tokenize(
            "
            d = if a == b {
                b
            } else if {
                c
            } else {
                e = 2 * e;
                e
            };
        ",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn for_loop() {
        let tokens = tokenize(
            "
            for (a = 0; a < 10; a = a + 1;) {
                b = 2 * b;
            }
        ",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn for_loop_brace() {
        let tokens = tokenize(
            "
            for (a = 0; a < 10; {a = a + 1; a = 2 * a}) {
                b = 2 * b;
            }
        ",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    /// multiple braced statements without an expression
    #[test]
    fn brace_statements() {
        let tokens = tokenize(
            "
            {
                a = b;
                c = d;
                e = f;
            }",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn braced_statements_and_expression() {
        let tokens = tokenize(
            "
            {
                a = b;
                c = d;
                e = f;
                e
            }",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn braced_statements_and_braced_expression() {
        let tokens = tokenize(
            "
            {
                a = b;
                c = d;
                e = f;
                {
                    g = a + f;
                    g
                }
            }",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens);
        unimplemented!();
    }

    #[test]
    fn basic_parse_error() {
        unimplemented!();
    }

    #[test]
    fn binary_op_and_assign() {
        unimplemented!();
    }
}
