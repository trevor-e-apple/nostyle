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
statement -> ((expression | declaration) "=" expression ";") | (expression ";");
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
pub mod rule;
mod test;
mod token_search;

use core::panic;
use std::todo;

use token_search::find_prev_matching_level_token_all_groups;

use crate::tokenize::tokens::{Token, Tokens};

use self::{
    ast::{Ast, AstNodeHandle},
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
pub fn parse(tokens: &Tokens) -> Ast {
    let mut result = Ast::new();

    // Handle special case where empty list of tokens is passed in
    if tokens.len() == 0 {
        return result;
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
            Rule::FunctionDef => {
                parse_function_def_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::FunctionDefParameters => {
                parse_function_parameters_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::Declaration => {
                parse_declaration_rule(tokens, &search_data, &mut result);
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
            Rule::ReturnStatement => {
                parse_return_statement(
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
            Rule::FunctionCall => {
                parse_function_call_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
            }
            Rule::FunctionArguments => {
                parse_function_arguments_rule(
                    tokens,
                    &search_data,
                    &mut result,
                    &mut stack,
                );
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
) {
    let start_token = match tokens.get(search_data.start) {
        Some(token) => token,
        None => todo!("Parse error (panic?)"),
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
}

/// parses the for rule
fn parse_for_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match tokens.get(search_data.end - 1) {
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

    // set up init, condition, increment statements
    {
        let init_semicolon_index =
            match find_next_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                lparen_index + 1,
                rparen_index,
            ) {
                Some(index) => index,
                None => todo!("Syntax error"),
            };
        let condition_semicolon_index =
            match find_next_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                init_semicolon_index + 1,
                rparen_index,
            ) {
                Some(index) => index,
                None => todo!("Syntax error"),
            };
        let increment_semicolon_index =
            match find_next_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                condition_semicolon_index + 1,
                rparen_index,
            ) {
                Some(index) => index,
                None => todo!("Syntax error"),
            };

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
    }

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

        add_child_to_search_stack(
            search_data.node_handle,
            Rule::BraceExpression,
            lbrace_index,
            rbrace_index + 1,
            ast,
            stack,
        );
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
        let non_recursive_end_index: usize =
            match find_prev_matching_level_token_all_groups(
                tokens,
                &[Token::EndStatement],
                search_data.start,
                search_data.end,
            ) {
                Some(index) => index,
                None => todo!("Syntax error"),
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
        None => todo!("Syntax error"),
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
}

/// parse statement rule
fn parse_statement_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match find_next_matching_level_token_all_groups(
        tokens,
        &[Token::Assign],
        search_data.start,
        search_data.end,
    ) {
        Some(assign_index) => match tokens.get(search_data.end - 1) {
            Some(expected_end_statement) => {
                if *expected_end_statement == Token::EndStatement {
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
                } else {
                    todo!("Syntax error");
                }
            }
            None => todo!("Syntax error?"),
        },
        None => match tokens.get(search_data.end - 2) {
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
                    todo!("Syntax error");
                }
            }
            None => todo!("Syntax error"),
        },
    }
}

fn parse_return_statement(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    let start = search_data.start + 1; // exclude Return token
    let end_statement_index = search_data.end - 1;
    match tokens.get(end_statement_index) {
        Some(token) => {
            if *token != Token::EndStatement {
                todo!("Syntax error: missing end statement");
            }
        }
        None => todo!("Syntax error"),
    }

    add_child_to_search_stack(
        search_data.node_handle,
        Rule::Expression,
        start,
        end_statement_index,
        ast,
        stack,
    );
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
            let if_lbrace_index = match find_matching_group_indices_end(
                tokens,
                &Token::LBrace,
                &Token::RBrace,
                search_data.start,
                else_index,
            ) {
                Some(lbrace_index) => lbrace_index,
                None => todo!("Syntax error"),
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
                None => todo!("Syntax error"),
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
                    None => todo!(),
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
                    None => todo!("Bad handle"),
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
                println!("{:?}", first_token);
                // add data to current node
                let node = match ast.get_node_mut(search_data.node_handle) {
                    Some(node) => node,
                    None => todo!(),
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
        None => todo!("Syntax error"),
    }
}

fn parse_function_call_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
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
                    todo!("Syntax error");
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
            todo!("Syntax error? Bit weird to not find this token at all");
        }
    }
}

fn parse_function_arguments_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
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
}

fn parse_function_def_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    match tokens.get(search_data.start) {
        Some(start_token) => match start_token {
            Token::Function => {}
            _ => {
                todo!("Something has gone horribly wrong. You should not arrive here without a leading function token.");
            }
        },
        None => {
            todo!("Syntax error? Bit weird to not find this token at all");
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

    if has_lparen && has_rparen {
        // update current node to include function name
        let node = match ast.get_node_mut(search_data.node_handle) {
            Some(node) => node,
            None => todo!("node handle bad???"),
        };
        let function_name = match tokens.get(search_data.start + 1) {
            Some(expected_symbol) => match expected_symbol {
                Token::Symbol(name) => name,
                _ => {
                    todo!("Syntax error. Function tokens must be followed by a symbol.")
                }
            },
            None => todo!(),
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
    } else if has_lparen || has_rparen {
        todo!("Syntax error: function defhas lparen but no rparen");
    } else {
        // no parens, symbol can't be parsed as function call despite leading function token.
        // syntax error
        todo!("Syntax error");
    }
}

fn parse_function_parameters_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
    stack: &mut Vec<SearchData>,
) {
    if search_data.start == search_data.end {
        // this function has no parameters. nothing to do
    } else {
        // get the final two symbols (the type and the parameter name)
        match tokens.get(search_data.end - 1) {
            Some(final_token) => {
                // check for trailing comma
                let final_symbol_index = match final_token {
                    Token::Comma => search_data.end - 2,
                    Token::Symbol(_) => search_data.end - 1,
                    _ => todo!("Syntax error. Expected Symbol or Comma."),
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
            None => todo!("Syntax error"),
        }
    }
}

fn parse_declaration_rule(
    tokens: &Tokens,
    search_data: &SearchData,
    ast: &mut Ast,
) {
    // check that search data is len 2
    if (search_data.end - search_data.start) != 2 {
        todo!("Syntax error. Declaration rule expects length 2.");
    }

    match tokens.get(search_data.start) {
        Some(token) => match token {
            Token::Symbol(_) => {
                ast.add_terminal_child(
                    search_data.node_handle,
                    Some(token.clone()),
                );
            }
            _ => todo!("Syntax error: token was not a symbol"),
        },
        None => todo!("Bad search range?"),
    }

    match tokens.get(search_data.start + 1) {
        Some(token) => match token {
            Token::Symbol(_) => {
                ast.add_terminal_child(
                    search_data.node_handle,
                    Some(token.clone()),
                );
            }
            _ => todo!("Syntax error: token was not a symbol"),
        },
        None => todo!("Bad search range?"),
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
                        todo!(
                            concat!(
                                "Syntax error: primary did not begin with grouping token,",
                                "but contained more than one token."
                            )
                        );
                    }
                    // update current primary to terminal
                    let node = match ast.get_node_mut(search_data.node_handle) {
                        Some(node) => node,
                        None => todo!(),
                    };
                    node.rule = Rule::Terminal;
                    node.data = Some(token.clone());
                }
                Token::LParen => {
                    // update current node to expression rule
                    let node = match ast.get_node_mut(search_data.node_handle) {
                        Some(node) => node,
                        None => todo!(),
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
                                todo!("Syntax error (mismatched parens)")
                            }
                        }
                        None => todo!("panic?"),
                    }
                }
                Token::LBrace => {
                    // update current node to BraceExpression rule
                    let node = match ast.get_node_mut(search_data.node_handle) {
                        Some(node) => node,
                        None => todo!(),
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
                                todo!("Syntax error (mismatched braces)")
                            }
                        }
                        None => todo!("panic?"),
                    }
                }
                _ => todo!("Syntax error, unexpected token"),
            },
            None => todo!("Syntax error"),
        }
    }
}
