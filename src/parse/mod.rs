/*
Grammar

this grammar expands in a way that matches operator precedence

expression -> function_def | brace_expression | if_else | for_loop | equality;
function_def -> "fn" SYMBOL "(" function_def_parameters ")" brace_expression;
function_def_parameters -> (function_def_parameters",")? declaration ","?;
declaration -> SYMBOL SYMBOL;
brace_expression -> "{" brace_statements? expression "}";
brace_statements -> brace_statements? (statement | return_statement);
return_statement -> "return" expression ";";
statement -> plus_equals_statement | minus_equals_statement | times_equals_statement | div_equals_statement | assign_statment;
plus_equals_statement -> ((expression | declaration) "+=" expression ";") | (expression ";");
minus_equals_statement -> ((expression | declaration) "-=" expression ";") | (expression ";");
times_equals_statement -> ((expression | declaration) "*=" expression ";") | (expression ";");
div_equals_statement -> ((expression | declaration) "/=" expression ";") | (expression ";");
assign_statement -> ((expression | declaration) "=" expression ";") | (expression ";");
if_else -> "if" expression brace_expression ("else" expression)?;
for_loop -> "for" "(" statement statement statement ")" brace_expression;
equality -> (equality ("==" | "!=") comparison) | comparison;
comparison -> (comparison (">" | ">=" | "<" | "<=") plus_minus) | plus_minus;
plus_minus -> (plus_minus ("+" | "-") mult_div) | mult_div;
mult_div -> (mult_div ("*" | "/") unary) | unary;
unary -> (("!" | "-") unary) | function_call;
function_call -> SYMBOL"(" function_arguments ")" | primary;
function_arguments -> (function_arguments ",")? expression ","?;
primary -> TRUE | FALSE | SYMBOL | NUMBER | STRING | NONE | "(" expression ")" | brace_expression;
*/

pub mod ast;
pub mod error;
pub mod rule;
mod test;
mod token_search;

use core::panic;
use std::todo;

use token_search::find_prev_matching_level_token_all_groups;

use crate::tokenize::tokens::{Token, Tokens};

use self::{
    ast::{Ast, AstNodeHandle},
    error::ParseError,
    rule::Rule,
    token_search::{
        find_final_matching_level_token_all_groups,
        find_matching_group_indices, find_matching_group_indices_end,
        find_next_matching_level_token,
        find_next_matching_level_token_all_groups,
    },
};

struct SearchData {
    start: usize,
    end: usize, // end is one-past the final included element in the search data
    node_handle: AstNodeHandle,
}

/// parses tokens and returns an abstract syntax tree
pub fn parse(tokens: &Tokens) -> Result<Ast, Vec<ParseError>> {
    let mut result = Ast::new();
    let mut parse_errors: Vec<ParseError> = Vec::new();

    // Handle special case where empty list of tokens is passed in
    if tokens.len() == 0 {
        return Ok(result);
    }

    let root_handle = result.add_root(Rule::Expression);

    let mut stack: Vec<SearchData> = vec![SearchData {
        start: 0,
        end: tokens.len(),
        node_handle: root_handle,
    }];
    while let Some(search_data) = stack.pop() {
        let rule = match result.get_node(search_data.node_handle) {
            Some(node) => node.rule,
            None => panic!("Missing node handle"),
        };

        match rule {
            Rule::Expression => {
                match parse_expression_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::FunctionDef => {
                match parse_function_def_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::FunctionDefParameters => {
                match parse_function_parameters_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::Declaration => {
                match parse_declaration_rule(tokens, &search_data, &mut result)
                {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::BraceExpression => {
                match parse_brace_expression_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::BraceStatements => {
                match parse_brace_statements_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::Statement => {
                match parse_statement_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::ReturnStatement => {
                match parse_return_statement(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::IfElse => {
                match parse_if_else_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::ForLoop => {
                match parse_for_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::Equality => {
                match parse_equality_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::Comparison => {
                match parse_comparison_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::PlusMinus => {
                match parse_plus_minus_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::MultDiv => {
                match parse_mult_div_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::Unary => {
                match parse_unary_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::FunctionCall => {
                match parse_function_call_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::FunctionArguments => {
                match parse_function_arguments_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::Primary => {
                match parse_primary_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::Terminal => {}
        }
    }

    if parse_errors.len() > 0 {
        Err(parse_errors)
    } else {
        Ok(result)
    }
}

/// function for creating a child and adding it to the search stack
fn add_child_to_search_stack(
    parent_handle: AstNodeHandle,
    child_rule: Rule,
    start: usize,
    end: usize,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    let child_handle = ast.add_child(parent_handle, child_rule);

    // add child to search stack
    stack.push(SearchData { start, end, node_handle: child_handle });
}

/// function for all data updates related to moving through one grammar rule and onto the next one
fn next_rule_updates(
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
    next_rule: Rule,
) {
    // update current node with next rule
    let node = match ast.get_node_mut(search_data.node_handle) {
        Some(node) => node,
        None => todo!(),
    };
    node.rule = next_rule;

    // push back onto the stack
    stack.push(SearchData {
        start: search_data.start,
        end: search_data.end,
        node_handle: search_data.node_handle,
    });
}

fn parse_expression_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    let start_token = match tokens.get(search_data.start) {
        Some(token) => token,
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Can not parse 'expression' rule (empty tokens)"
                    .to_owned(),
            })
        }
    };

    let rule = match start_token {
        Token::LBrace => Rule::BraceExpression,
        Token::If => Rule::IfElse,
        Token::For => Rule::ForLoop,
        Token::Function => Rule::FunctionDef,
        _ => Rule::Equality,
    };

    add_child_to_search_stack(
        search_data.node_handle,
        rule,
        search_data.start,
        search_data.end,
        ast,
        stack,
    );

    Ok(())
}

/// parses the for rule
fn parse_for_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    match tokens.get(search_data.end - 1) {
        Some(end_token) => {
            if *end_token != Token::RBrace {
                return Err(ParseError {
                    line_number: 0,
                    info: "Missing rbrace at the end of 'for' loop".to_owned(),
                });
            }
        }
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Can not parse 'for' rule (empty tokens)".to_owned(),
            });
        }
    }

    let lparen_index = search_data.start + 1;

    // check for leading lparen
    match tokens.get(lparen_index) {
        Some(expected_lparen) => {
            if *expected_lparen != Token::LParen {
                return Err(ParseError {
                    line_number: 0,
                    info: "Missing lparen after 'for'".to_owned(),
                });
            }
        }
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Missing lparen after 'for'".to_owned(),
            });
        }
    }

    let rparen_index = match find_matching_group_indices(
        tokens,
        &Token::LParen,
        &Token::RParen,
        lparen_index,
        search_data.end,
    ) {
        Some(rparen_index) => rparen_index,
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Missing matching rparen for 'for' statements".to_owned(),
            });
        }
    };

    // set up init, condition, increment statements
    let (
        init_semicolon_index,
        condition_semicolon_index,
        increment_semicolon_index,
    ) =
        {
            let init_semicolon_index =
                match find_next_matching_level_token_all_groups(
                    tokens,
                    &[Token::EndStatement],
                    lparen_index + 1,
                    rparen_index,
                ) {
                    Some(index) => index,
                    None => {
                        return Err(ParseError {
                            line_number: 0,
                            info: "Missing init statement in 'for' statements"
                                .to_owned(),
                        });
                    }
                };
            let condition_semicolon_index =
                match find_next_matching_level_token_all_groups(
                    tokens,
                    &[Token::EndStatement],
                    init_semicolon_index + 1,
                    rparen_index,
                ) {
                    Some(index) => index,
                    None => return Err(ParseError {
                        line_number: 0,
                        info: "Missing condition statement in 'for' statements"
                            .to_owned(),
                    }),
                };
            let increment_semicolon_index =
                match find_next_matching_level_token_all_groups(
                    tokens,
                    &[Token::EndStatement],
                    condition_semicolon_index + 1,
                    rparen_index,
                ) {
                    Some(index) => index,
                    None => return Err(ParseError {
                        line_number: 0,
                        info: "Missing increment statement in 'for' statements"
                            .to_owned(),
                    }),
                };

            (
                init_semicolon_index,
                condition_semicolon_index,
                increment_semicolon_index,
            )
        };

    // set up brace expression
    let (expression_lbrace_index, expression_rbrace_index) = {
        let lbrace_index = rparen_index + 1;
        match tokens.get(lbrace_index) {
            Some(expected_lbrace) => {
                if *expected_lbrace != Token::LBrace {
                    return Err(ParseError {
                        line_number: 0,
                        info:
                            "Missing expected lbrace after for loop statements"
                                .to_owned(),
                    });
                }
            }
            None => {
                return Err(ParseError {
                    line_number: 0,
                    info: "Missing expected lbrace after for loop statements"
                        .to_owned(),
                });
            }
        }

        let rbrace_index = match find_matching_group_indices(
            tokens,
            &Token::LBrace,
            &Token::RBrace,
            lbrace_index,
            search_data.end,
        ) {
            Some(rbrace_index) => rbrace_index,
            None => {
                return Err(ParseError {
                    line_number: 0,
                    info: "Could not find expected rbrace".to_owned(),
                })
            }
        };

        (lbrace_index, rbrace_index)
    };

    // mutate ast and search stacks after we have confirmed that no errors have been hit

    // init statement
    add_child_to_search_stack(
        search_data.node_handle,
        Rule::Statement,
        lparen_index + 1,
        init_semicolon_index + 1,
        ast,
        stack,
    );

    // condition statement
    add_child_to_search_stack(
        search_data.node_handle,
        Rule::Statement,
        init_semicolon_index + 1,
        condition_semicolon_index + 1,
        ast,
        stack,
    );

    // increment statement
    add_child_to_search_stack(
        search_data.node_handle,
        Rule::Statement,
        condition_semicolon_index + 1,
        increment_semicolon_index + 1,
        ast,
        stack,
    );

    add_child_to_search_stack(
        search_data.node_handle,
        Rule::BraceExpression,
        expression_lbrace_index,
        expression_rbrace_index + 1,
        ast,
        stack,
    );

    return Ok(());
}

/// parses the brace expression rule
fn parse_brace_expression_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    let first_token = match tokens.get(search_data.start) {
        Some(token) => token,
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Missing expected lbrace at start of brace expression"
                    .to_owned(),
            })
        }
    };

    if *first_token != Token::LBrace {
        return Err(ParseError {
            line_number: 0,
            info: "Missing expected lbrace at start of brace expression"
                .to_owned(),
        });
    }

    let final_token = match tokens.get(search_data.end - 1) {
        Some(token) => token,
        None => panic!("Search data end index out of range."),
    };

    if *final_token != Token::RBrace {
        return Err(ParseError {
            line_number: 0,
            info: "Missing rbrace at end of brace expression".to_owned(),
        });
    }

    let brace_contents_start = search_data.start + 1;
    let brace_contents_end = search_data.end - 1;

    let end_brace_statements: Option<usize> =
        match find_final_matching_level_token_all_groups(
            tokens,
            &[Token::EndStatement],
            brace_contents_start,
            brace_contents_end,
        ) {
            Some((index, _)) => Some(index + 1),
            None => None,
        };

    match end_brace_statements {
        Some(end_brace_statements) => {
            // some braces don't have brace statements
            if brace_contents_start < end_brace_statements {
                add_child_to_search_stack(
                    search_data.node_handle,
                    Rule::BraceStatements,
                    brace_contents_start,
                    end_brace_statements,
                    ast,
                    stack,
                );
            }

            add_child_to_search_stack(
                search_data.node_handle,
                Rule::Expression,
                end_brace_statements,
                brace_contents_end,
                ast,
                stack,
            );
        }
        None => {
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::Expression,
                brace_contents_start,
                brace_contents_end,
                ast,
                stack,
            );
        }
    }

    Ok(())
}

/// parse brace_statements rule
fn parse_brace_statements_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    // find brace_statement terminal
    let (non_recursive_start_index, non_recursive_end_index): (usize, usize) = {
        let non_recursive_end_index: usize =
            match find_prev_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                search_data.start,
                search_data.end,
            ) {
                Some(index) => index,
                None => {
                    return Err(ParseError {
                        line_number: 0,
                        info: "Could not find expected end statement in brace statements".to_owned(),
                    });
                }
            };

        // find the previous EndStatement at the same level as the end of the terminal
        let non_recursive_start_index =
            match find_prev_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                search_data.start,
                non_recursive_end_index,
            ) {
                Some(prev_end_index) => prev_end_index + 1,
                None => search_data.start,
            };

        (non_recursive_start_index, non_recursive_end_index + 1)
    };

    // push the preceding statements for a recursive expansion
    if search_data.start < non_recursive_start_index {
        add_child_to_search_stack(
            search_data.node_handle,
            Rule::BraceStatements,
            search_data.start,
            non_recursive_start_index,
            ast,
            stack,
        );
    }

    let non_recursive_start_token = match tokens.get(non_recursive_start_index)
    {
        Some(non_recursive_start_token) => non_recursive_start_token,
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Non recursive brace statement out of range".to_owned(),
            });
        }
    };

    let non_recursive_rule = if *non_recursive_start_token == Token::Return {
        Rule::ReturnStatement
    } else {
        Rule::Statement
    };

    // non recursive expansion
    add_child_to_search_stack(
        search_data.node_handle,
        non_recursive_rule,
        non_recursive_start_index,
        non_recursive_end_index,
        ast,
        stack,
    );

    Ok(())
}

// TODO: document me!
fn binary_comp_statement(
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
    assign_index: usize,
    composite_rule: Rule,
    composite_token: Token,
) {
    // LHS of statement
    add_child_to_search_stack(
        search_data.node_handle,
        Rule::Expression,
        search_data.start,
        assign_index,
        ast,
        stack,
    );

    // Statement RHS. Requires some scaffolding to set up a consistent parse with non-composed version
    let rhs_expression =
        ast.add_child(search_data.node_handle, Rule::Expression);
    let rhs_op_node = ast.add_child_with_data(
        rhs_expression,
        composite_rule,
        Some(composite_token),
    );

    // LHS (same as statement LHS)
    add_child_to_search_stack(
        rhs_op_node,
        Rule::Expression,
        search_data.start,
        assign_index,
        ast,
        stack,
    );

    // RHS (everything past the binary op and assignment token)
    add_child_to_search_stack(
        rhs_op_node,
        Rule::Expression,
        assign_index + 1,
        search_data.end - 1,
        ast,
        stack,
    );
}

/// parse statement rule
fn parse_statement_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    // Find assign token to split on
    match find_next_matching_level_token_all_groups(
        tokens,
        &[
            Token::Assign,
            Token::PlusEquals,
            Token::MinusEquals,
            Token::TimesEquals,
            Token::DivideEquals,
        ],
        search_data.start,
        search_data.end,
    ) {
        Some(assign_index) => {
            // Hand the assignment statement case
            match tokens.get(search_data.end - 1) {
                Some(expected_end_statement) => {
                    if *expected_end_statement != Token::EndStatement {
                        return Err(ParseError {
                            line_number: 0,
                            info: "Missing expected end statement token"
                                .to_owned(),
                        });
                    }

                    let assign_token = match tokens.get(assign_index) {
                        Some(assign_token) => assign_token,
                        None => {
                            // the assign token *should* always be there since it was found by
                            // -- find_next_matching_level_token_all_groups. Panic instead of error.
                            panic!("Missing assign token");
                        }
                    };

                    // Handle various kinds of assignment tokens
                    match assign_token {
                        Token::Assign => {
                            // LHS expression
                            add_child_to_search_stack(
                                search_data.node_handle,
                                Rule::Expression,
                                search_data.start,
                                assign_index,
                                ast,
                                stack,
                            );

                            // RHS expression
                            add_child_to_search_stack(
                                search_data.node_handle,
                                Rule::Expression,
                                assign_index + 1,
                                search_data.end - 1,
                                ast,
                                stack,
                            );
                        }
                        Token::PlusEquals => {
                            binary_comp_statement(
                                search_data,
                                ast,
                                stack,
                                assign_index,
                                Rule::PlusMinus,
                                Token::Plus,
                            );
                        }
                        Token::MinusEquals => {
                            binary_comp_statement(
                                search_data,
                                ast,
                                stack,
                                assign_index,
                                Rule::PlusMinus,
                                Token::Minus,
                            );
                        }
                        Token::TimesEquals => {
                            binary_comp_statement(
                                search_data,
                                ast,
                                stack,
                                assign_index,
                                Rule::MultDiv,
                                Token::Times,
                            );
                        }
                        Token::DivideEquals => {
                            binary_comp_statement(
                                search_data,
                                ast,
                                stack,
                                assign_index,
                                Rule::MultDiv,
                                Token::Divide,
                            );
                        }
                        _ => {
                            // panics instead of returning an error b/c the find error case
                            // is already handled
                            panic!("Unexpected token for assignment");
                        }
                    }
                }
                None => {
                    return Err(ParseError {
                        line_number: 0,
                        info: "No end statement (end of tokens).".to_owned(),
                    });
                }
            }
        }
        None => {
            // Handle the non-assignment statement case

            // TODO: why is this - 2 instead of - 1? add a comment explaining?
            // -- Should be caught during code coverage...
            match tokens.get(search_data.end - 2) {
                Some(expected_end_statement) => {
                    if *expected_end_statement != Token::EndStatement {
                        add_child_to_search_stack(
                            search_data.node_handle,
                            Rule::Expression,
                            search_data.start,
                            search_data.end - 1,
                            ast,
                            stack,
                        );
                    } else {
                        return Err(ParseError {
                            line_number: 0,
                            info: "Missing expected end statement.".to_owned(),
                        });
                    }
                }
                None => {
                    return Err(ParseError {
                        line_number: 0,
                        info: "No end statement found (end of tokens)."
                            .to_owned(),
                    });
                }
            }
        }
    }

    Ok(())
}

fn parse_return_statement(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    let start = search_data.start + 1; // exclude Return token
    let end_statement_index = search_data.end - 1;
    match tokens.get(end_statement_index) {
        Some(token) => {
            if *token != Token::EndStatement {
                return Err(ParseError {
                    line_number: 0,
                    info: "Syntax error: missing end statement".to_owned(),
                });
            }
        }
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Syntax error: missing end statement".to_owned(),
            })
        }
    }

    add_child_to_search_stack(
        search_data.node_handle,
        Rule::Expression,
        start,
        end_statement_index,
        ast,
        stack,
    );

    Ok(())
}

/// parse the if_else rule
fn parse_if_else_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    match tokens.get(search_data.start) {
        Some(expected_if) => {
            if *expected_if != Token::If {
                return Err(ParseError {
                    line_number: 0,
                    info: "Missing if keyword".to_owned(),
                });
            }
        }
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Missing if keyword".to_owned(),
            })
        }
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
            let if_lbrace_index = match find_matching_group_indices_end(
                tokens,
                &Token::LBrace,
                &Token::RBrace,
                search_data.start,
                else_index,
            ) {
                Some(lbrace_index) => lbrace_index,
                None => {
                    return Err(ParseError {
                        line_number: 0,
                        info: "Missing lbrace".to_owned(),
                    });
                }
            };

            // condition expression
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::Expression,
                search_data.start + 1,
                if_lbrace_index,
                ast,
                stack,
            );

            // executed expression
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::BraceExpression,
                if_lbrace_index,
                else_index,
                ast,
                stack,
            );

            // to differentiate between if condition expression and else
            // expression, we always need to add the else expression second
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::Expression,
                else_index + 1,
                search_data.end,
                ast,
                stack,
            );
        }
        None => {
            // find the matching lbrace for this rbrace
            let lbrace_index = match find_matching_group_indices_end(
                tokens,
                &Token::LBrace,
                &Token::RBrace,
                search_data.start,
                search_data.end,
            ) {
                Some(lbrace_index) => lbrace_index,
                None => {
                    return Err(ParseError {
                        line_number: 0,
                        info: "Missing lbrace".to_owned(),
                    })
                }
            };

            // condition expression
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::Expression,
                search_data.start + 1,
                lbrace_index,
                ast,
                stack,
            );

            // executed expression
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::BraceExpression,
                lbrace_index,
                search_data.end,
                ast,
                stack,
            );
        }
    }

    Ok(())
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
) -> Result<(), ParseError> {
    match find_final_matching_level_token_all_groups(
        tokens,
        matching_tokens,
        search_data.start,
        search_data.end,
    ) {
        Some((split_index, binary_op_token)) => {
            /***
             * Special case: Minus token is both a binary operator and a unary
             * operator. In general, binary operators cannot be adjacent, so we know
             * that if we see a minus by another binary operator, we should treat it
             * as a unary. If we see a different binary op, we should raise an error
             ***/
            let (is_unary, split_index, binary_op_token) = {
                let mut split_index = split_index;
                let mut binary_op_token = binary_op_token;
                let mut is_unary: bool = false;
                loop {
                    if binary_op_token == Token::Minus {
                        if split_index == 0 {
                            // leading minus operator must be a unary or a failed parse, not a binary op
                            is_unary = true;
                            break;
                        }

                        // check previous token to see if it's a binary op token
                        let prev_token_index = split_index - 1;
                        let prev_token = match tokens.get(prev_token_index) {
                            Some(prev_token) => prev_token,
                            // This means (split_index - 1) >= tokens.len,
                            // which means find_final_matching_level_token_all_groups is messed up
                            None => panic!(),
                        };

                        if matching_tokens.contains(prev_token) {
                            // keep looking for a binary operator
                            split_index = prev_token_index;
                            binary_op_token = prev_token.clone();
                        } else {
                            // Minus token must be a binary operator. Break
                            break;
                        }
                    } else {
                        // not a minus token, so must not be a unary operator, continue
                        break;
                    }
                }

                (is_unary, split_index, binary_op_token)
            };

            if is_unary {
                // modify current node with next rule
                let node = match ast.get_node_mut(search_data.node_handle) {
                    Some(node) => node,
                    None => panic!("Bad handle"),
                };
                node.rule = next_rule;
                // push back onto the stack
                stack.push(SearchData {
                    start: search_data.start,
                    end: search_data.end,
                    node_handle: search_data.node_handle,
                });
            } else if split_index == search_data.start {
                // there are no leading tokens for the binary op, therefore this cannot
                // -- be parsed as a binary op. therefore, pass on to the next rule
                next_rule_updates(search_data, ast, stack, next_rule);
            } else {
                // update the token data in the expanding node
                match ast.get_node_mut(search_data.node_handle) {
                    Some(node) => {
                        node.data = Some(binary_op_token);
                    }
                    None => panic!("Bad handle"),
                }

                // add children and add them to the search stack
                add_child_to_search_stack(
                    search_data.node_handle,
                    recursive_rule,
                    search_data.start,
                    split_index,
                    ast,
                    stack,
                );

                add_child_to_search_stack(
                    search_data.node_handle,
                    next_rule,
                    split_index + 1,
                    search_data.end,
                    ast,
                    stack,
                );
            }
        }
        None => {
            next_rule_updates(search_data, ast, stack, next_rule);
        }
    }

    Ok(())
}

// parses the equality rule
fn parse_equality_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
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
) -> Result<(), ParseError> {
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
) -> Result<(), ParseError> {
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
) -> Result<(), ParseError> {
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
) -> Result<(), ParseError> {
    match tokens.get(search_data.start) {
        Some(first_token) => {
            if *first_token == Token::Not || *first_token == Token::Minus {
                // add data to current node
                let node = match ast.get_node_mut(search_data.node_handle) {
                    Some(node) => node,
                    None => panic!("Bad handle"),
                };
                node.data = Some(first_token.clone());

                // recursion for unary expansion
                add_child_to_search_stack(
                    search_data.node_handle,
                    Rule::Unary,
                    search_data.start + 1,
                    search_data.end,
                    ast,
                    stack,
                );
            } else {
                next_rule_updates(search_data, ast, stack, Rule::FunctionCall);
            }
        }
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Empty unary rule".to_owned(),
            });
        }
    }

    Ok(())
}

fn parse_function_call_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    match tokens.get(search_data.start) {
        Some(start_token) => match start_token {
            Token::Symbol(_) => {
                // check for left and right parens
                let has_lparen: bool = match tokens.get(search_data.start + 1) {
                    Some(possible_lparen) => {
                        if *possible_lparen == Token::LParen {
                            true
                        } else {
                            false
                        }
                    }
                    None => {
                        // if there aren't other tokens, then move on to the next rule
                        false
                    }
                };
                let has_rparen = match tokens.get(search_data.end - 1) {
                    Some(possible_rparen) => {
                        if *possible_rparen == Token::RParen {
                            true
                        } else {
                            false
                        }
                    }
                    None => false,
                };

                if has_lparen && has_rparen {
                    // update current node to include function name
                    let node = match ast.get_node_mut(search_data.node_handle) {
                        Some(node) => node,
                        None => todo!("node handle bad???"),
                    };
                    node.data = Some(start_token.clone());

                    add_child_to_search_stack(
                        search_data.node_handle,
                        Rule::FunctionArguments,
                        search_data.start + 2,
                        search_data.end - 1,
                        ast,
                        stack,
                    );
                } else if has_lparen || has_rparen {
                    // has_lparen xor has_rparen == true
                    if has_lparen {
                        return Err(ParseError {
                            line_number: 0,
                            info: "Missing rparen for function call".to_owned(),
                        });
                    } else {
                        return Err(ParseError {
                            line_number: 0,
                            info: "Missing lparen for function call".to_owned(),
                        });
                    }
                } else {
                    // no parens, symbol can't be parsed as function call
                    next_rule_updates(search_data, ast, stack, Rule::Primary);
                }
            }
            _ => {
                // if not a symbol, move on to next rule
                next_rule_updates(search_data, ast, stack, Rule::Primary);
            }
        },
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Empty function call rule".to_owned(),
            });
        }
    }

    Ok(())
}

fn parse_function_arguments_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    // find the final comma in the search range
    match find_final_matching_level_token_all_groups(
        tokens,
        &[Token::Comma],
        search_data.start,
        search_data.end,
    ) {
        Some((final_comma_index, _)) => {
            // find the RHS expression start and end. if there is no left hand side,
            // this block of code will add to the stack in place
            let (added_to_stack, rhs_start, rhs_end) =
                if final_comma_index == (search_data.end - 1) {
                    let (added_to_stack, prev_arg_comma_index) =
                        match find_final_matching_level_token_all_groups(
                            tokens,
                            &[Token::Comma],
                            search_data.start,
                            final_comma_index,
                        ) {
                            Some((prev_arg_comma_index, _)) => {
                                (false, prev_arg_comma_index)
                            }
                            None => {
                                add_child_to_search_stack(
                                    search_data.node_handle,
                                    Rule::Expression,
                                    search_data.start,
                                    final_comma_index,
                                    ast,
                                    stack,
                                );
                                (true, 0)
                            }
                        };
                    (
                        added_to_stack,
                        prev_arg_comma_index + 1, // don't include the comma
                        final_comma_index,
                    )
                } else {
                    (
                        false,
                        final_comma_index + 1, // don't include the comma
                        search_data.end,
                    )
                };

            if !added_to_stack {
                // LHS is the recursive side
                add_child_to_search_stack(
                    search_data.node_handle,
                    Rule::FunctionArguments,
                    search_data.start,
                    rhs_start,
                    ast,
                    stack,
                );

                // RHS is an expression
                add_child_to_search_stack(
                    search_data.node_handle,
                    Rule::Expression,
                    rhs_start,
                    rhs_end,
                    ast,
                    stack,
                );
            }
        }
        None => {
            // the entire search range must be the final expression
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::Expression,
                search_data.start,
                search_data.end,
                ast,
                stack,
            );
        }
    }

    Ok(())
}

fn parse_function_def_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    match tokens.get(search_data.start) {
        Some(start_token) => match start_token {
            Token::Function => {}
            _ => {
                return Err(ParseError {
                    line_number: 0,
                    info: "Missing expected function def token".to_owned(),
                });
            }
        },
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Missing expected function def token".to_owned(),
            });
        }
    }

    // check for left and right parens
    let (has_lparen, lparen_index, has_rparen, rparen_index) = {
        let lparen_index = search_data.start + 2;
        let has_lparen: bool = match tokens.get(lparen_index) {
            Some(possible_lparen) => {
                if *possible_lparen == Token::LParen {
                    true
                } else {
                    false
                }
            }
            None => {
                // if there aren't other tokens, then move on to the next rule
                false
            }
        };
        let (has_rparen, rparen_index) =
            match find_next_matching_level_token_all_groups(
                &tokens,
                &[Token::RParen],
                lparen_index + 1,
                search_data.end,
            ) {
                Some(index) => (true, index),
                None => (false, 0),
            };

        (has_lparen, lparen_index, has_rparen, rparen_index)
    };

    if has_lparen && !has_rparen {
        return Err(ParseError {
            line_number: 0,
            info: "Function def has no rparen".to_owned(),
        });
    } else if !has_lparen && has_rparen {
        return Err(ParseError {
            line_number: 0,
            info: "Function def has no lparen".to_owned(),
        });
    } else if !has_lparen && !has_rparen {
        return Err(ParseError {
            line_number: 0,
            info: "Function def has no parens".to_owned(),
        });
    }

    // update current node to include function name
    let node = match ast.get_node_mut(search_data.node_handle) {
        Some(node) => node,
        None => panic!("Bad node handle"),
    };
    let function_name = match tokens.get(search_data.start + 1) {
        Some(expected_symbol) => match expected_symbol {
            Token::Symbol(name) => name,
            _ => {
                return Err(ParseError {
                    line_number: 0,
                    info: "Function tokens must be followed by a symbol."
                        .to_owned(),
                });
            }
        },
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Function tokens must be followed by a symbol".to_owned(),
            })
        }
    };
    node.data = Some(Token::Symbol(function_name.clone()));

    add_child_to_search_stack(
        search_data.node_handle,
        Rule::FunctionDefParameters,
        lparen_index + 1,
        rparen_index,
        ast,
        stack,
    );

    add_child_to_search_stack(
        search_data.node_handle,
        Rule::BraceExpression,
        rparen_index + 1,
        search_data.end,
        ast,
        stack,
    );

    Ok(())
}

fn parse_function_parameters_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    if search_data.start == search_data.end {
        // this function has no parameters. nothing to do
        return Ok(());
    }

    // get the final two symbols (the type and the parameter name)
    match tokens.get(search_data.end - 1) {
        Some(final_token) => {
            // check for trailing comma
            let final_symbol_index = match final_token {
                Token::Comma => search_data.end - 2,
                Token::Symbol(_) => search_data.end - 1,
                _ => {
                    return Err(ParseError {
                        line_number: 0,
                        info: "Syntax error. Expected Symbol or Comma."
                            .to_owned(),
                    });
                }
            };
            let declaration_start = final_symbol_index - 1;

            // LHS is the recursive side
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::FunctionDefParameters,
                search_data.start,
                declaration_start,
                ast,
                stack,
            );

            // RHS is a declaration
            add_child_to_search_stack(
                search_data.node_handle,
                Rule::Declaration,
                declaration_start,
                final_symbol_index + 1,
                ast,
                stack,
            );
        }
        None => {
            return Err(ParseError {
                line_number: 0,
                info: "Function parameters token out of range".to_owned(),
            });
        }
    }

    Ok(())
}

fn parse_declaration_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
) -> Result<(), ParseError> {
    // check that search data is len 2
    if (search_data.end - search_data.start) != 2 {
        return Err(ParseError {
            line_number: 0,
            info: "Declaration rule has more than two tokens.".to_owned(),
        });
    }

    match tokens.get(search_data.start) {
        Some(token) => match token {
            Token::Symbol(_) => {
                ast.add_terminal_child(
                    search_data.node_handle,
                    Some(token.clone()),
                );
            }
            _ => {
                return Err(ParseError {
                    line_number: 0,
                    info: "First token in declaration is not a symbol"
                        .to_owned(),
                })
            }
        },
        None => panic!(
            "This can not occur as the search length has already been checked."
        ),
    }

    match tokens.get(search_data.start + 1) {
        Some(token) => match token {
            Token::Symbol(_) => {
                ast.add_terminal_child(
                    search_data.node_handle,
                    Some(token.clone()),
                );
            }
            _ => {
                return Err(ParseError {
                    line_number: 0,
                    info: "Second token in declaration is not a symbol"
                        .to_owned(),
                })
            }
        },
        None => panic!(
            "This can not occur as the search length has already been checked."
        ),
    }

    Ok(())
}

fn parse_primary_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) -> Result<(), ParseError> {
    if search_data.start == search_data.end {
        // handle empty expression case
        let node = match ast.get_node_mut(search_data.node_handle) {
            Some(node) => node,
            None => todo!(),
        };
        node.rule = Rule::Terminal;
        node.data = None;
    } else {
        match tokens.get(search_data.start) {
            Some(token) => match token {
                Token::Symbol(_)
                | Token::IntLiteral(_)
                | Token::FloatLiteral(_)
                | Token::StringLiteral(_) => {
                    if (search_data.end - search_data.start) != 1 {
                        return Err(ParseError {
                            line_number: 0,
                            info: "Primary did not begin with grouping token but contained multiple tokens".to_owned(),
                        });
                    }
                    // update current primary to terminal
                    let node = match ast.get_node_mut(search_data.node_handle) {
                        Some(node) => node,
                        None => {
                            panic!("Missing node handle")
                        }
                    };
                    node.rule = Rule::Terminal;
                    node.data = Some(token.clone());
                }
                Token::LParen => {
                    // update current node to expression rule
                    let node = match ast.get_node_mut(search_data.node_handle) {
                        Some(node) => node,
                        None => panic!("Missing node handle"),
                    };
                    node.rule = Rule::Expression;

                    let expected_rparen_index = search_data.end - 1;
                    match tokens.get(expected_rparen_index) {
                        Some(expected_rparen) => {
                            // check whether we have mismatched parens
                            if *expected_rparen == Token::RParen {
                                // add back to stack
                                stack.push(SearchData {
                                    start: search_data.start + 1,
                                    end: expected_rparen_index,
                                    node_handle: search_data.node_handle,
                                });
                            } else {
                                return Err(ParseError {
                                    line_number: 0,
                                    info: "Mismatched parens".to_owned(),
                                });
                            }
                        }
                        None => {
                            return Err(ParseError {
                                line_number: 0,
                                info: "Mismatched parens".to_owned(),
                            });
                        }
                    }
                }
                Token::LBrace => {
                    // update current node to BraceExpression rule
                    let node = match ast.get_node_mut(search_data.node_handle) {
                        Some(node) => node,
                        None => panic!("Missing node handle"),
                    };
                    node.rule = Rule::BraceExpression;

                    let expected_rbrace_index = search_data.end - 1;
                    match tokens.get(expected_rbrace_index) {
                        Some(expected_rbrace) => {
                            // check whether we have mismatched braces
                            if *expected_rbrace == Token::RBrace {
                                stack.push(SearchData {
                                    start: search_data.start,
                                    end: expected_rbrace_index + 1,
                                    node_handle: search_data.node_handle,
                                });
                            } else {
                                return Err(ParseError {
                                    line_number: 0,
                                    info: "Mismatched braces".to_owned(),
                                });
                            }
                        }
                        None => {
                            return Err(ParseError {
                                line_number: 0,
                                info: "Mismatched braces".to_owned(),
                            })
                        }
                    }
                }
                _ => {
                    return Err(ParseError {
                        line_number: 0,
                        info: "Unexpected token".to_owned(),
                    })
                }
            },
            None => {
                return Err(ParseError {
                    line_number: 0,
                    info: "Empty primary rule".to_owned(),
                })
            }
        }
    }

    Ok(())
}
