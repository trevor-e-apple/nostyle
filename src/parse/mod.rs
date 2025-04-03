/*
Grammar

this grammar expands in a way that matches operator precedence

expression -> function_defs | brace_expression | paren_expression | if_else | for_loop | data_struct_def| equality;
if_else -> "if" expression brace_expression ("else" expression)?;
for_loop -> "for" "(" statement statement statement ")" brace_expression;
equality -> (equality ("==" | "!=") comparison) | comparison;
comparison -> (comparison (">" | ">=" | "<" | "<=") plus_minus) | plus_minus;
plus_minus -> (plus_minus ("+" | "-") mult_div) | mult_div;
mult_div -> (mult_div ("*" | "/") unary) | unary;
unary -> (("!" | "-") unary) | struct_access;
struct_access -> (struct_access "." function_call) | function_call;
function_call -> SYMBOL"(" function_arguments ")" | paren_expression;
function_arguments -> (function_arguments ",")? expression ","?;
paren_expression -> "(" expression ")" | brace_expression;
brace_expression -> "{" brace_statements? expression "}" | primary;
brace_statements -> brace_statements? (statement | return_statement);
primary -> TRUE | FALSE | SYMBOL | NUMBER | STRING | NONE;

declaration -> SYMBOL SYMBOL;

return_statement -> "return" expression ";";
statement ->
    plus_equals_statement | minus_equals_statement | times_equals_statement | div_equals_statement | assign_statment |
    effect_statement;
plus_equals_statement -> ((expression | declaration) "+=" expression ";");
minus_equals_statement -> ((expression | declaration) "-=" expression ";");
times_equals_statement -> ((expression | declaration) "*=" expression ";") | (expression ";");
div_equals_statement -> ((expression | declaration) "/=" expression ";") | (expression ";");
assign_statement -> ((expression | declaration) "=" expression ";");
effect_statement -> expression ";";

function_defs -> function_defs? function_def;
function_def -> "fn" SYMBOL "(" function_def_parameters ")" returns_data? brace_expression;
returns_data -> "returns" SYMBOL;
function_def_parameters -> (function_def_parameters",")? declaration ","?;

declaration_statements -> declaration_statements? declaration ";";
data_structure -> "struct" SYMBOL "{" declaration_statements? "}";
*/

pub mod ast;
pub mod error;
pub mod rule;
#[cfg(test)]
mod test;
mod token_search;

use core::panic;

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

/// parses tokens and returns an abstract syntax tree
pub fn parse(tokens: &Tokens) -> Result<Ast, Vec<ParseError>> {
    let mut result = Ast::new();
    let mut parse_errors: Vec<ParseError> = Vec::new();

    // Handle special case where empty list of tokens is passed in
    if tokens.len() == 0 {
        return Ok(result);
    }

    let root_handle = result.add_root(Rule::Expression, 0, tokens.len());

    let mut stack: Vec<AstNodeHandle> = vec![root_handle];
    while let Some(node_handle) = stack.pop() {
        let rule = {
            let node = result.get_node(node_handle);
            node.rule
        };

        match rule {
            Rule::Expression => {
                match parse_expression_rule(
                    tokens,
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::ParenExpression => {
                match parse_paren_expression_rule(
                    tokens,
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::FunctionDefs => {
                match parse_function_defs_rule(
                    tokens,
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::FunctionDef => {
                match parse_function_def_rule(
                    tokens,
                    node_handle,
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
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::ReturnsData => {
                match parse_returns_data_rule(
                    tokens,
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::Declaration => {
                match parse_declaration_rule(tokens, node_handle, &mut result) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::BraceExpression => {
                match parse_brace_expression_rule(
                    tokens,
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
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
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                };
            }
            Rule::Terminal => {}
            Rule::DeclarationStatements => {
                match parse_declaration_statements(
                    tokens,
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::DataStructure => {
                match parse_data_structure(
                    tokens,
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
            Rule::StructAccess => {
                match parse_struct_access(
                    tokens,
                    node_handle,
                    &mut result,
                    &mut stack,
                ) {
                    Ok(_) => {}
                    Err(error) => parse_errors.push(error),
                }
            }
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
    len: usize,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) {
    let child_handle = ast.add_child(parent_handle, child_rule, start, len);

    // add child to search stack
    stack.push(child_handle);
}

/// function for all data updates related to moving through one grammar rule and onto the next one
fn next_rule_updates(
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
    next_rule: Rule,
) {
    // update current node with next rule
    let node = ast.get_node_mut(node_handle);
    node.rule = next_rule;

    // push back onto the stack
    stack.push(node_handle);
}

fn make_final_line_error(
    tokens: &Tokens,
    start_line: usize,
    info: String,
) -> ParseError {
    return ParseError { start_line, end_line: tokens.get_final_line(), info };
}

fn parse_expression_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let node = ast.get_node(node_handle);
    let (node_start, node_end) = (node.start, node.get_end_index());

    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    let start_token = match tokens.get_token(node_start) {
        Some(token) => token,
        None => {
            return Err(make_final_line_error(
                tokens,
                0,
                "Can not parse 'expression' rule (empty tokens)".to_owned(),
            ));
        }
    };

    // handle null expression
    if node.len == 0 {
        ast.add_child(node_handle, Rule::Terminal, node_start, node.len);
        return Ok(());
    }

    let end_token = match tokens.get_token(node_end) {
        Some(token) => token,
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Can not parse 'expression' rule (end out of bounds)"
                    .to_owned(),
            });
        }
    };

    let rule = match start_token {
        Token::LParen => {
            if *end_token == Token::RParen {
                Rule::ParenExpression
            } else {
                Rule::Equality
            }
        }
        Token::LBrace => {
            if *end_token == Token::RBrace {
                Rule::BraceExpression
            } else {
                Rule::Equality
            }
        }
        Token::If => Rule::IfElse,
        Token::For => Rule::ForLoop,
        Token::Function => Rule::FunctionDefs,
        Token::Struct => Rule::DataStructure,
        _ => Rule::Equality,
    };

    add_child_to_search_stack(
        node_handle,
        rule,
        node.start,
        node.len,
        ast,
        stack,
    );

    Ok(())
}

fn parse_paren_expression_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let node = ast.get_node(node_handle);
    let (node_start, node_end) = (node.start, node.get_end_index());
    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    let start_token = match tokens.get_token(node_start) {
        Some(token) => token,
        None => {
            return Err(make_final_line_error(
                tokens,
                start_line,
                "Can not parse 'paren_expression' rule (missing lparen)"
                    .to_owned(),
            ));
        }
    };

    if *start_token != Token::LParen {
        next_rule_updates(node_handle, ast, stack, Rule::BraceExpression);
        Ok(())
    } else {
        let end_token = match tokens.get_token(node_end) {
            Some(token) => token,
            None => {
                return Err(make_final_line_error(
                    tokens,
                    start_line,
                    "Can not parse 'paren_expression' rule (missing rparen)"
                        .to_owned(),
                ));
            }
        };
        if *end_token != Token::RParen {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing rparen".to_owned(),
            });
        };

        // add child while removing parens
        add_child_to_search_stack(
            node_handle,
            Rule::Expression,
            node.start + 1,
            node.len - 2,
            ast,
            stack,
        );

        Ok(())
    }
}

/// parses the for rule
fn parse_for_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let node = ast.get_node(node_handle);

    let (start_line, end_line) =
        get_start_end_lines(tokens, node.start, node.get_end_index());

    match tokens.get(node.get_end_index()) {
        Some((end_token, line_number)) => {
            if end_token != Token::RBrace {
                return Err(ParseError {
                    start_line,
                    end_line: line_number,
                    info: "Missing rbrace at the end of 'for' loop".to_owned(),
                });
            }
        }
        None => {
            return Err(make_final_line_error(
                tokens,
                start_line,
                "Can not parse 'for' rule (empty tokens)".to_owned(),
            ));
        }
    }

    let lparen_index = node.start + 1;

    // check for leading lparen
    match tokens.get(lparen_index) {
        Some((expected_lparen, line_number)) => {
            if expected_lparen != Token::LParen {
                return Err(ParseError {
                    start_line,
                    end_line: line_number,
                    info: "Missing lparen after 'for'".to_owned(),
                });
            }
        }
        None => {
            return Err(make_final_line_error(
                tokens,
                start_line,
                "Missing lparen after 'for'".to_owned(),
            ));
        }
    }

    let rparen_index = match find_matching_group_indices(
        tokens,
        &Token::LParen,
        &Token::RParen,
        lparen_index,
        node.start + node.len,
    ) {
        Some(rparen_index) => rparen_index,
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing rparen for 'for' rule".to_owned(),
            });
        }
    };

    let rparen_line_number = match tokens.get(rparen_index) {
        Some((_, line_number)) => line_number,
        None => panic!("This should never happen"),
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
                            start_line,
                            end_line: rparen_line_number,
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
                        start_line,
                        end_line: rparen_line_number,
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
                        start_line,
                        end_line: rparen_line_number,
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
            Some((expected_lbrace, line_number)) => {
                if expected_lbrace != Token::LBrace {
                    return Err(ParseError {
                        start_line,
                        end_line: line_number,
                        info:
                            "Missing expected lbrace after for loop statements"
                                .to_owned(),
                    });
                }
            }
            None => {
                return Err(ParseError {
                    start_line,
                    end_line: start_line,
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
            node.start + node.len,
        ) {
            Some(rbrace_index) => rbrace_index,
            None => {
                return Err(ParseError {
                    start_line,
                    end_line,
                    info: "Could not find expected rbrace".to_owned(),
                })
            }
        };

        (lbrace_index, rbrace_index)
    };

    // mutate ast and search stacks after we have confirmed that no errors have been hit

    // init statement
    add_child_to_search_stack(
        node_handle,
        Rule::Statement,
        lparen_index + 1,
        init_semicolon_index - lparen_index,
        ast,
        stack,
    );

    // condition statement
    add_child_to_search_stack(
        node_handle,
        Rule::Statement,
        init_semicolon_index + 1,
        condition_semicolon_index - init_semicolon_index,
        ast,
        stack,
    );

    // increment statement
    add_child_to_search_stack(
        node_handle,
        Rule::Statement,
        condition_semicolon_index + 1,
        increment_semicolon_index - condition_semicolon_index,
        ast,
        stack,
    );

    add_child_to_search_stack(
        node_handle,
        Rule::BraceExpression,
        expression_lbrace_index,
        expression_rbrace_index - expression_lbrace_index + 1,
        ast,
        stack,
    );

    return Ok(());
}

/// parses the brace expression rule
fn parse_brace_expression_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let node = ast.get_node(node_handle);

    // check for first token
    let (first_token, start_line) = match tokens.get(node.start) {
        Some(token) => token,
        None => {
            panic!("Empty brace expression to parse.");
        }
    };

    if first_token != Token::LBrace {
        next_rule_updates(node_handle, ast, stack, Rule::Primary);
        Ok(())
    } else {
        let final_token = match tokens.get_token(node.get_end_index()) {
            Some(token) => token,
            None => {
                return Err(ParseError {
                    start_line,
                    end_line: start_line,
                    info: "Missing rbrace at end of brace expression"
                        .to_owned(),
                })
            }
        };

        if *final_token != Token::RBrace {
            return Err(ParseError {
                start_line,
                end_line: start_line,
                info: "Missing rbrace at end of brace expression".to_owned(),
            });
        }

        let brace_contents_start = node.start + 1;
        let rbrace_index = node.get_end_index();

        let end_brace_statements: Option<usize> =
            match find_final_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                brace_contents_start,
                rbrace_index,
            ) {
                Some((index, _)) => Some(index + 1),
                None => None,
            };

        match end_brace_statements {
            Some(end_brace_statements) => {
                // some braces don't have brace statements
                if brace_contents_start < end_brace_statements {
                    add_child_to_search_stack(
                        node_handle,
                        Rule::BraceStatements,
                        brace_contents_start,
                        end_brace_statements - brace_contents_start,
                        ast,
                        stack,
                    );
                }

                add_child_to_search_stack(
                    node_handle,
                    Rule::Expression,
                    end_brace_statements,
                    rbrace_index - end_brace_statements,
                    ast,
                    stack,
                );
            }
            None => {
                add_child_to_search_stack(
                    node_handle,
                    Rule::Expression,
                    brace_contents_start,
                    rbrace_index - brace_contents_start,
                    ast,
                    stack,
                );
            }
        }

        Ok(())
    }
}

/// parse brace_statements rule
fn parse_brace_statements_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let node = ast.get_node(node_handle);

    let (start_line, end_line) =
        get_start_end_lines(tokens, node.start, node.get_end_index());

    // find brace_statement terminal
    let (non_recursive_start_index, non_recursive_end_index): (usize, usize) = {
        let non_recursive_end_index: usize =
            match find_prev_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                node.start,
                node.get_end_index() + 1,
            ) {
                Some(index) => index,
                None => {
                    return Err(ParseError {
                        start_line,
                        end_line,
                        info: "Could not find expected end statement in brace statements".to_owned(),
                    })
                },
            };

        // find the previous EndStatement at the same level as the end of the terminal
        let non_recursive_start_index =
            match find_prev_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                node.start,
                non_recursive_end_index,
            ) {
                Some(prev_end_index) => prev_end_index + 1,
                None => node.start,
            };

        (non_recursive_start_index, non_recursive_end_index)
    };

    let (non_recursive_start_token, _) = match tokens
        .get(non_recursive_start_index)
    {
        Some(non_recursive_start_token) => non_recursive_start_token,
        None => panic!("Unexpected non recursive brace statements start index"),
    };

    let non_recursive_rule = if non_recursive_start_token == Token::Return {
        Rule::ReturnStatement
    } else {
        Rule::Statement
    };

    // push the preceding statements for a recursive expansion
    if node.start < non_recursive_start_index {
        add_child_to_search_stack(
            node_handle,
            Rule::BraceStatements,
            node.start,
            non_recursive_start_index - node.start,
            ast,
            stack,
        );
    }

    // non recursive expansion
    add_child_to_search_stack(
        node_handle,
        non_recursive_rule,
        non_recursive_start_index,
        non_recursive_end_index - non_recursive_start_index + 1,
        ast,
        stack,
    );

    Ok(())
}

// TODO: document me!
fn binary_comp_statement(
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
    assign_index: usize,
    composite_rule: Rule,
    composite_token: Token,
) {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };

    // LHS of statement
    add_child_to_search_stack(
        node_handle,
        Rule::Expression,
        node_start,
        assign_index - node_start,
        ast,
        stack,
    );

    // Statement RHS. Requires some scaffolding to set up a consistent parse with non-composed version
    let rhs_expression_len = node_end - node_start; // don't include trailing semicolon
    let rhs_expression = ast.add_child(
        node_handle,
        Rule::Expression,
        node_start,
        rhs_expression_len,
    );
    let rhs_op_node = ast.add_child_with_data(
        rhs_expression,
        composite_rule,
        Some(composite_token),
        node_start,
        rhs_expression_len,
    );

    // LHS (same as statement LHS)
    add_child_to_search_stack(
        rhs_op_node,
        Rule::Expression,
        node_start,
        assign_index - node_start,
        ast,
        stack,
    );

    // RHS (everything past the binary op and assignment token)
    // don't include trailing semi colon or assignment token
    add_child_to_search_stack(
        rhs_op_node,
        Rule::Expression,
        assign_index + 1,
        node_end - 1 - assign_index,
        ast,
        stack,
    );
}

fn get_start_end_lines(
    tokens: &Tokens,
    start: usize,
    end: usize,
) -> (usize, usize) {
    // TODO: document that end is the end index
    let start_line = match tokens.get_line_number(start) {
        Some(line_number) => line_number,
        None => panic!("Cannot parse empty tokens as statement"),
    };
    let end_line = match tokens.get_line_number(end) {
        Some(line_number) => line_number,
        None => {
            panic!("Bad parse range.");
        }
    };

    return (start_line, end_line);
}

/// parse statement rule
fn parse_statement_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };

    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

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
        node_start,
        node_end,
    ) {
        Some(assign_index) => {
            // Handle the assignment statement case
            match tokens.get(node_end) {
                Some((expected_end_statement, line_number)) => {
                    if expected_end_statement != Token::EndStatement {
                        return Err(ParseError {
                            start_line: line_number,
                            end_line: line_number,
                            info: "Missing expected end statement token"
                                .to_owned(),
                        });
                    }

                    let assign_token = match tokens.get(assign_index) {
                        Some((assign_token, _)) => assign_token,
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
                                node_handle,
                                Rule::Expression,
                                node_start,
                                assign_index - node_start,
                                ast,
                                stack,
                            );

                            // RHS expression
                            add_child_to_search_stack(
                                node_handle,
                                Rule::Expression,
                                assign_index + 1,
                                node_end - assign_index - 1, // remove semicolon
                                ast,
                                stack,
                            );
                        }
                        Token::PlusEquals => {
                            binary_comp_statement(
                                node_handle,
                                ast,
                                stack,
                                assign_index,
                                Rule::PlusMinus,
                                Token::Plus,
                            );
                        }
                        Token::MinusEquals => {
                            binary_comp_statement(
                                node_handle,
                                ast,
                                stack,
                                assign_index,
                                Rule::PlusMinus,
                                Token::Minus,
                            );
                        }
                        Token::TimesEquals => {
                            binary_comp_statement(
                                node_handle,
                                ast,
                                stack,
                                assign_index,
                                Rule::MultDiv,
                                Token::Times,
                            );
                        }
                        Token::DivideEquals => {
                            binary_comp_statement(
                                node_handle,
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
                        start_line,
                        end_line,
                        info: "No end statement (end of tokens).".to_owned(),
                    });
                }
            }
        }
        None => {
            // Handle the non-assignment statement case (drop the semicolon)
            add_child_to_search_stack(
                node_handle,
                Rule::Expression,
                node_start,
                node_end - node_start,
                ast,
                stack,
            );
        }
    }

    Ok(())
}

fn parse_return_statement(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let node = ast.get_node(node_handle);
    let (start_line, end_line) =
        get_start_end_lines(tokens, node.start, node.get_end_index());

    let start_index = node.start + 1; // exclude Return token
    let end_statement_index = node.get_end_index();
    match tokens.get(end_statement_index) {
        Some((token, line_number)) => {
            if token != Token::EndStatement {
                return Err(ParseError {
                    start_line,
                    end_line: line_number,
                    info: "Syntax error: missing end statement".to_owned(),
                });
            }
        }
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing end statement in return statement".to_owned(),
            });
        }
    }

    add_child_to_search_stack(
        node_handle,
        Rule::Expression,
        start_index,
        end_statement_index,
        ast,
        stack,
    );

    Ok(())
}

/// parse the if_else rule
fn parse_if_else_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };
    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    match tokens.get(node_start) {
        Some((expected_if, line_number)) => {
            if expected_if != Token::If {
                return Err(ParseError {
                    start_line,
                    end_line: line_number,
                    info: "Missing if keyword".to_owned(),
                });
            }
        }
        None => {
            panic!("Unexpected empty if parse");
        }
    }

    // check if we have an if-else or just an if
    match find_next_matching_level_token(
        tokens,
        &[Token::Else],
        node_start + 1,
        node_end + 1,
        &Token::LBrace,
        &Token::RBrace,
    ) {
        Some(else_index) => {
            let if_lbrace_index = match find_matching_group_indices_end(
                tokens,
                &Token::LBrace,
                &Token::RBrace,
                node_start,
                else_index,
            ) {
                Some(lbrace_index) => lbrace_index,
                None => match tokens.get_line_number(else_index) {
                    Some(line_number) => {
                        return Err(ParseError {
                            start_line,
                            end_line: line_number,
                            info: "Missing lbrace".to_owned(),
                        });
                    }
                    None => {
                        return Err(ParseError {
                            start_line,
                            end_line,
                            info: "Missing lbrace".to_owned(),
                        });
                    }
                },
            };

            // condition expression
            let condition_start = node_start + 1;
            add_child_to_search_stack(
                node_handle,
                Rule::Expression,
                condition_start,
                if_lbrace_index - condition_start,
                ast,
                stack,
            );

            // executed expression
            add_child_to_search_stack(
                node_handle,
                Rule::BraceExpression,
                if_lbrace_index,
                else_index - if_lbrace_index,
                ast,
                stack,
            );

            // to differentiate between if condition expression and else
            // expression, we always need to add the else expression second
            let else_expression_start = else_index + 1;
            add_child_to_search_stack(
                node_handle,
                Rule::Expression,
                else_expression_start,
                node_end - else_expression_start + 1,
                ast,
                stack,
            );
        }
        None => {
            // check for rbrace
            let rbrace_line = match tokens.get(node_end) {
                Some((token, line_number)) => {
                    if token != Token::RBrace {
                        return Err(ParseError {
                            start_line: line_number,
                            end_line: line_number,
                            info: format!(
                                "Missing expected rbrace on line {}",
                                line_number
                            )
                            .to_owned(),
                        });
                    } else {
                        line_number
                    }
                }
                None => panic!("Out of range error"),
            };

            // find the matching lbrace for this rbrace
            let lbrace_index = match find_matching_group_indices_end(
                tokens,
                &Token::LBrace,
                &Token::RBrace,
                node_start,
                node_end,
            ) {
                Some(lbrace_index) => lbrace_index,
                None => {
                    return Err(ParseError {
                        start_line,
                        end_line,
                        info: format!("Missing expected lbrace matching rbrace on line {}", rbrace_line).to_owned(),
                    });
                }
            };

            // condition expression
            {
                let condition_start = node_start + 1;
                add_child_to_search_stack(
                    node_handle,
                    Rule::Expression,
                    condition_start,
                    lbrace_index - condition_start,
                    ast,
                    stack,
                );
            }

            // executed expression
            add_child_to_search_stack(
                node_handle,
                Rule::BraceExpression,
                lbrace_index,
                node_end - lbrace_index + 1,
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
    node_handle: AstNodeHandle,
    matching_tokens: &[Token],
    recursive_rule: Rule,
    next_rule: Rule,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };

    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    match find_final_matching_level_token_all_groups(
        tokens,
        matching_tokens,
        node_start,
        node_end,
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
                        let prev_token = match tokens.get_token(prev_token_index) {
                            Some(prev_token) => prev_token,
                            // This means (split_index - 1) >= tokens.len,
                            // which means find_final_matching_level_token_all_groups is messed up
                            None => panic!("find_final_matching_level_token_all_groups led to out-of-range prev_token_index"),
                        };

                        if matching_tokens.contains(&prev_token) {
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
                let node = ast.get_node_mut(node_handle);
                node.rule = next_rule;
                // push back onto the stack
                stack.push(node_handle);
            } else if split_index == node_start {
                // there are no leading tokens for the binary op, therefore this cannot
                // -- be parsed as a binary op. therefore, pass on to the next rule
                next_rule_updates(node_handle, ast, stack, next_rule);
            } else {
                // update the token data in the expanding node
                let node = ast.get_node_mut(node_handle);
                node.data = Some(binary_op_token);

                // check to see if the split is possible
                if split_index == node_end {
                    return Err(ParseError {
                        start_line,
                        end_line,
                        info: "Unexpected binary op at end of expression"
                            .to_owned(),
                    });
                }

                // add children and add them to the search stack
                add_child_to_search_stack(
                    node_handle,
                    recursive_rule,
                    node_start,
                    split_index - node_start, // b/c we don't include the split, don't add one here for length
                    ast,
                    stack,
                );

                let rhs_start = split_index + 1;
                add_child_to_search_stack(
                    node_handle,
                    next_rule,
                    rhs_start,
                    (node_end - rhs_start) + 1,
                    ast,
                    stack,
                );
            }
        }
        None => {
            next_rule_updates(node_handle, ast, stack, next_rule);
        }
    }

    Ok(())
}

// parses the equality rule
fn parse_equality_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    parse_binary_op_rule(
        tokens,
        node_handle,
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
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    parse_binary_op_rule(
        tokens,
        node_handle,
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
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    parse_binary_op_rule(
        tokens,
        node_handle,
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
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    parse_binary_op_rule(
        tokens,
        node_handle,
        &[Token::Times, Token::Divide],
        Rule::MultDiv,
        Rule::Unary,
        ast,
        stack,
    )
}

fn parse_unary_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };

    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    match tokens.get_token(node_start) {
        Some(first_token) => {
            if *first_token == Token::Not || *first_token == Token::Minus {
                // add data to current node
                let node = ast.get_node_mut(node_handle);
                node.data = Some(first_token.clone());

                // recursion for unary expansion
                add_child_to_search_stack(
                    node_handle,
                    Rule::Unary,
                    node_start + 1,
                    node_end - node_start,
                    ast,
                    stack,
                );
            } else {
                next_rule_updates(node_handle, ast, stack, Rule::StructAccess);
            }
        }
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Empty unary rule".to_owned(),
            });
        }
    }

    Ok(())
}

fn parse_function_call_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };
    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    match tokens.get_token(node_start) {
        Some(start_token) => match start_token {
            Token::Symbol(_) => {
                // check for left and right parens
                let has_lparen: bool = match tokens.get_token(node_start + 1) {
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
                let has_rparen = match tokens.get_token(node_end) {
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
                    let node = ast.get_node_mut(node_handle);
                    node.data = Some(start_token.clone());

                    add_child_to_search_stack(
                        node_handle,
                        Rule::FunctionArguments,
                        node_start + 2,
                        node_end - node_start - 2,
                        ast,
                        stack,
                    );
                } else if has_lparen || has_rparen {
                    // has_lparen xor has_rparen == true
                    if has_lparen {
                        return Err(ParseError {
                            start_line,
                            end_line,
                            info: "Missing rparen for function call".to_owned(),
                        });
                    } else {
                        return Err(ParseError {
                            start_line,
                            end_line,
                            info: "Missing lparen for function call".to_owned(),
                        });
                    }
                } else {
                    // no parens in correct position, symbol can't be parsed as function call
                    next_rule_updates(node_handle, ast, stack, Rule::Primary);
                }
            }
            _ => {
                // if not a symbol, move on to next rule
                next_rule_updates(
                    node_handle,
                    ast,
                    stack,
                    Rule::ParenExpression,
                );
            }
        },
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Empty function call rule".to_owned(),
            });
        }
    }

    Ok(())
}

fn parse_function_arguments_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end, node_len) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index(), node.len)
    };

    // find the final comma in the search range
    match find_final_matching_level_token_all_groups(
        tokens,
        &[Token::Comma],
        node_start,
        node_end,
    ) {
        Some((final_comma_index, _)) => {
            // find the RHS expression start and end. if there is no left hand side,
            // this block of code will add to the stack in place
            let (added_to_stack, rhs_start, rhs_end) =
                if final_comma_index == node_end {
                    let (added_to_stack, prev_arg_comma_index) =
                        match find_final_matching_level_token_all_groups(
                            tokens,
                            &[Token::Comma],
                            node_start,
                            final_comma_index,
                        ) {
                            Some((prev_arg_comma_index, _)) => {
                                (false, prev_arg_comma_index)
                            }
                            None => {
                                add_child_to_search_stack(
                                    node_handle,
                                    Rule::Expression,
                                    node_start,
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
                        node_end,
                    )
                };

            if !added_to_stack {
                // LHS is the recursive side
                add_child_to_search_stack(
                    node_handle,
                    Rule::FunctionArguments,
                    node_start,
                    rhs_start - node_start,
                    ast,
                    stack,
                );

                // RHS is an expression
                add_child_to_search_stack(
                    node_handle,
                    Rule::Expression,
                    rhs_start,
                    rhs_end - rhs_start + 1,
                    ast,
                    stack,
                );
            }
        }
        None => {
            // the entire search range must be the final expression
            add_child_to_search_stack(
                node_handle,
                Rule::Expression,
                node_start,
                node_len,
                ast,
                stack,
            );
        }
    }

    Ok(())
}

fn parse_function_defs_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };
    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    let ultimate_function_def_token_index =
        match find_prev_matching_level_token_all_groups(
            &tokens,
            &[Token::Function],
            node_start,
            node_end,
        ) {
            Some(index) => index,
            None => {
                return Err(ParseError {
                    start_line,
                    end_line,
                    info: "Expected function definition token not found"
                        .to_owned(),
                })
            }
        };

    // add a recursive rule if necessary
    match find_prev_matching_level_token_all_groups(
        tokens,
        &[Token::Function],
        node_start,
        ultimate_function_def_token_index,
    ) {
        Some(_) => {
            add_child_to_search_stack(
                node_handle,
                Rule::FunctionDefs,
                node_start,
                ultimate_function_def_token_index,
                ast,
                stack,
            );
            add_child_to_search_stack(
                node_handle,
                Rule::FunctionDef,
                ultimate_function_def_token_index,
                node_end,
                ast,
                stack,
            );
        }
        None => {
            // can modify current node rule since this is a single function def
            next_rule_updates(node_handle, ast, stack, Rule::FunctionDef);
        }
    };

    Ok(())
}

fn parse_function_def_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };
    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    match tokens.get_token(node_start) {
        Some(start_token) => match start_token {
            Token::Function => {}
            _ => {
                return Err(ParseError {
                    start_line,
                    end_line,
                    info: "Missing expected function def token".to_owned(),
                });
            }
        },
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing expected function def token".to_owned(),
            });
        }
    }

    // check for left and right parens
    let (has_lparen, lparen_index, has_rparen, rparen_index) = {
        let lparen_index = node_start + 2;
        let has_lparen: bool = match tokens.get_token(lparen_index) {
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
                node_end,
            ) {
                Some(index) => (true, index),
                None => (false, 0),
            };

        (has_lparen, lparen_index, has_rparen, rparen_index)
    };

    if has_lparen && !has_rparen {
        return Err(ParseError {
            start_line,
            end_line,
            info: "Function def has no rparen".to_owned(),
        });
    } else if !has_lparen && has_rparen {
        return Err(ParseError {
            start_line,
            end_line,
            info: "Function def has no lparen".to_owned(),
        });
    } else if !has_lparen && !has_rparen {
        return Err(ParseError {
            start_line,
            end_line,
            info: "Function def has no parens".to_owned(),
        });
    }

    // update current node to include function name
    let node = ast.get_node_mut(node_handle);
    let function_name = match tokens.get_token(node_start + 1) {
        Some(expected_symbol) => match expected_symbol {
            Token::Symbol(name) => name,
            _ => {
                return Err(ParseError {
                    start_line,
                    end_line,
                    info: "Function tokens must be followed by a symbol."
                        .to_owned(),
                });
            }
        },
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Function tokens must be followed by a symbol".to_owned(),
            })
        }
    };
    node.data = Some(Token::Symbol(function_name.clone()));

    let (returns_index, lbrace_index) = {
        let next_token_index = rparen_index + 1;
        let next_token = match tokens.get_token(next_token_index) {
            Some(next_token) => next_token,
            None => {
                return Err(ParseError {
                    start_line,
                    end_line,
                    info: "Missing token after function def rparen".to_owned(),
                })
            }
        };

        if *next_token == Token::Returns {
            (Some(next_token_index), next_token_index + 2)
        } else if *next_token == Token::LBrace {
            (None, next_token_index)
        } else {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing expected token (returns, lbrace) after rparen"
                    .to_owned(),
            });
        }
    };

    add_child_to_search_stack(
        node_handle,
        Rule::FunctionDefParameters,
        lparen_index + 1,
        rparen_index,
        ast,
        stack,
    );

    match returns_index {
        Some(returns_index) => {
            add_child_to_search_stack(
                node_handle,
                Rule::ReturnsData,
                returns_index,
                lbrace_index,
                ast,
                stack,
            );
        }
        None => {}
    }

    add_child_to_search_stack(
        node_handle,
        Rule::BraceExpression,
        lbrace_index,
        node_end,
        ast,
        stack,
    );

    Ok(())
}

fn parse_function_parameters_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let node = ast.get_node(node_handle);

    if node.start == node.get_end_index() {
        // this function has no parameters. nothing to do
        return Ok(());
    }

    let (start_line, end_line) =
        get_start_end_lines(tokens, node.start, node.get_end_index());

    // get the final two symbols (the type and the parameter name)
    match tokens.get(node.get_end_index()) {
        Some((final_token, final_token_line)) => {
            // check for trailing comma
            let final_symbol_index = match final_token {
                Token::Comma => node.get_end_index() - 1,
                Token::Symbol(_) => node.get_end_index(),
                _ => {
                    return Err(ParseError {
                        start_line: final_token_line,
                        end_line: final_token_line,
                        info: "Missing expected Symbol or Comma at end of function parameters"
                            .to_owned(),
                    });
                }
            };
            let declaration_start = final_symbol_index - 1;

            // LHS is the recursive side
            add_child_to_search_stack(
                node_handle,
                Rule::FunctionDefParameters,
                node.start,
                declaration_start,
                ast,
                stack,
            );

            // RHS is a declaration
            add_child_to_search_stack(
                node_handle,
                Rule::Declaration,
                declaration_start,
                final_symbol_index + 1,
                ast,
                stack,
            );
        }
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Function parameters token out of range".to_owned(),
            });
        }
    }

    Ok(())
}

fn parse_returns_data_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let node = ast.get_node(node_handle);

    let (start_line, end_line) =
        get_start_end_lines(tokens, node.start, node.get_end_index());

    let token = match tokens.get_token(node.start + 1) {
        Some(token) => match *token {
            Token::Symbol(_) => token,
            _ => {
                return Err(ParseError {
                    start_line,
                    end_line,
                    info: "Missing expected symbol token after returns"
                        .to_owned(),
                })
            }
        },
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing expected symbol token after returns".to_owned(),
            })
        }
    };

    ast.add_child_with_data(
        node_handle,
        Rule::Terminal,
        Some(token.clone()),
        node.start,
        node.len,
    );

    Ok(())
}

fn parse_declaration_rule(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };

    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    // check that search data is len 2
    if (node_end - node_start + 1) != 2 {
        return Err(ParseError {
            start_line,
            end_line,
            info: "Declaration rule has more than two tokens".to_owned(),
        });
    }

    match tokens.get_token(node_start) {
        Some(token) => match token {
            Token::Symbol(_) => {
                ast.add_terminal_child(
                    node_handle,
                    Some(token.clone()),
                    node_start,
                    node_end,
                );
            }
            _ => {
                return Err(ParseError {
                    start_line,
                    end_line: start_line,
                    info: "First token in declaration is not a symbol"
                        .to_owned(),
                })
            }
        },
        None => panic!(
            "This can not occur as the search length has already been checked."
        ),
    }

    match tokens.get(node_start + 1) {
        Some((token, line_number)) => match token {
            Token::Symbol(_) => {
                ast.add_terminal_child(
                    node_handle,
                    Some(token.clone()),
                    node_start,
                    node_end,
                );
            }
            _ => {
                return Err(ParseError {
                    start_line: line_number,
                    end_line: line_number,
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
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end, node_len) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index(), node.len)
    };
    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);

    match tokens.get(node_start) {
        Some((token, line_number)) => match token {
            Token::Symbol(_)
            | Token::IntLiteral(_)
            | Token::FloatLiteral(_)
            | Token::StringLiteral(_) => {
                if node_len > 1 {
                    return Err(ParseError {
                            start_line,
                            end_line,
                            info: "Primary did not begin with grouping token but contained multiple tokens".to_owned(),
                        });
                }
                // update current primary to terminal
                let node = ast.get_node_mut(node_handle);
                node.rule = Rule::Terminal;
                node.data = Some(token.clone());
            }
            _ => {
                return Err(ParseError {
                    start_line: line_number,
                    end_line: line_number,
                    info: "Unexpected start token for primary rule".to_owned(),
                });
            }
        },
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Empty primary rule".to_owned(),
            })
        }
    }

    Ok(())
}

fn parse_declaration_statements(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };
    // search for semicolon to split recursive and non recrusive declaration on
    match find_prev_matching_level_token_all_groups(
        tokens,
        &[Token::EndStatement],
        node_start,
        node_end - 1, // don't include the final semicolon
    ) {
        Some(split_index) => {
            add_child_to_search_stack(
                node_handle,
                Rule::DeclarationStatements,
                node_start,
                split_index + 1,
                ast,
                stack,
            );
            add_child_to_search_stack(
                node_handle,
                Rule::Declaration,
                split_index + 1,
                node_end - 1, // exclude trailing semicolon
                ast,
                stack,
            );
        }
        None => {
            add_child_to_search_stack(
                node_handle,
                Rule::Declaration,
                node_start,
                node_end - 1, // exclude trailing semicolon
                ast,
                stack,
            );
        }
    }

    Ok(())
}

fn parse_data_structure(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index())
    };

    let (start_line, end_line) =
        get_start_end_lines(tokens, node_start, node_end);
    // verify first token is struct
    match tokens.get_token(node_start) {
        Some(start_token) => {
            if *start_token != Token::Struct {
                return Err(ParseError { start_line, end_line, info: "Missing struct token at start of expected structure definition".to_owned() });
            } else {
            }
        }
        None => return Err(ParseError {
            start_line,
            end_line,
            info:
                "Missing struct token at start of expected structure definition"
                    .to_owned(),
        }),
    }

    let struct_name_index = node_start + 1;
    let struct_name_token = match tokens.get_token(struct_name_index) {
        Some(struct_name_token) => struct_name_token,
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing struct name".to_owned(),
            })
        }
    };

    // find lbrace
    let lbrace_index = node_start + 2;
    match tokens.get_token(lbrace_index) {
        Some(expected_lbrace) => {
            if *expected_lbrace != Token::LBrace {
                return Err(ParseError {
                    start_line,
                    end_line,
                    info: "Missing lbrace at start of structure definition"
                        .to_owned(),
                });
            } else {
            }
        }
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing lbrace at start of structure definition"
                    .to_owned(),
            })
        }
    };

    // find rbrace
    let rbrace_index = node_end - 1;
    match tokens.get_token(rbrace_index) {
        Some(expected_rbrace) => {
            if *expected_rbrace != Token::RBrace {
                return Err(ParseError {
                    start_line,
                    end_line,
                    info: "Missing rbrace token at end of structure definition"
                        .to_owned(),
                });
            } else {
            }
        }
        None => {
            return Err(ParseError {
                start_line,
                end_line,
                info: "Missing rbrace token at end of structure definition"
                    .to_owned(),
            });
        }
    };

    let semicolon_index = rbrace_index - 1;
    let has_declarations = match tokens.get_token(semicolon_index) {
        Some(token) => {
            if *token == Token::EndStatement {
                true
            } else {
                false
            }
        }
        None => {
            panic!("This should never happen, rbrace has already been found")
        }
    };

    if has_declarations {
        add_child_to_search_stack(
            node_handle,
            Rule::DeclarationStatements,
            lbrace_index + 1,
            rbrace_index,
            ast,
            stack,
        );
    }

    let node = ast.get_node_mut(node_handle);
    node.data = Some(struct_name_token.clone());

    Ok(())
}

fn parse_struct_access(
    tokens: &Tokens,
    node_handle: AstNodeHandle,
    ast: &mut Ast,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), ParseError> {
    let (node_start, node_end, node_len) = {
        let node = ast.get_node(node_handle);
        (node.start, node.get_end_index(), node.len)
    };

    // split on the right most dot
    match find_final_matching_level_token_all_groups(
        tokens,
        &[Token::Dot],
        node_start,
        node_end,
    ) {
        Some((split_index, _)) => {
            add_child_to_search_stack(
                node_handle,
                Rule::StructAccess,
                node_start,
                split_index - node_start,
                ast,
                stack,
            );

            // exclude dot token by adding 1
            let terminal_start = split_index + 1;
            add_child_to_search_stack(
                node_handle,
                Rule::FunctionCall,
                terminal_start,
                node_end - split_index,
                ast,
                stack,
            );
            Ok(())
        }
        None => {
            next_rule_updates(node_handle, ast, stack, Rule::FunctionCall);
            Ok(())
        }
    }
}
