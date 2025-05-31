pub mod error;

use std::collections::HashMap;

use error::TypeError;

use crate::{
    parse::{
        ast::{Ast, AstNode, AstNodeHandle},
        rule::Rule,
    },
    tokenize::tokens::{Token, Tokens},
};

struct FunctionTypeData {
    argument_types: Vec<String>,
    return_type: Option<String>,
}

struct StructTypeData {
    struct_to_type: HashMap<String, String>,
}

pub fn type_check(tokens: &Tokens, ast: &Ast) -> Result<(), Vec<TypeError>> {
    // TODO: create a operation table for mapping unary and binary operations to valid operands

    // Find all function and struct definitions
    let (function_type_map, struct_type_map) =
        find_function_struct_definitions(ast);

    // Perform type checks on AST
    {
        let ast_root = match ast.get_root() {
            Some(root_handle) => root_handle,
            None => todo!(),
        };

        // TODO: we have to break this up function by function (sets up parallelization)
        let mut errors: Vec<TypeError> = vec![];
        let mut stack = vec![ast_root];
        let mut node_type_info = HashMap::<AstNodeHandle, String>::new();

        // For values, None means that an error occurred and future errors based on this variable info should be ignored (since its type is ambiguous)
        let mut variable_type_info = HashMap::<String, Option<String>>::new();

        loop {
            let node_handle = if stack.len() > 0 {
                match stack.get(stack.len() - 1) {
                    Some(node_handle) => node_handle.clone(),
                    None => panic!("This should never happen"),
                }
            } else {
                break;
            };

            let node = ast.get_node(node_handle);

            match node.rule {
                Rule::Declaration => {
                    // LHS child is the type, RHS is the variable
                    let lhs_child_handle = node.children[0].clone();
                    let type_name =
                        ast.expect_node_name(lhs_child_handle).clone();

                    let rhs_child_handle = node.children[1].clone();
                    let variable_name =
                        ast.expect_node_name(rhs_child_handle).clone();

                    node_type_info.insert(lhs_child_handle, type_name.clone());
                    variable_type_info
                        .insert(variable_name, Some(type_name.clone()));
                }
                Rule::Statement => {
                    match type_check_statement(
                        ast,
                        tokens,
                        node,
                        &mut node_type_info,
                        &mut variable_type_info,
                        &mut stack,
                    ) {
                        Ok(_) => {}
                        Err(e) => errors.push(e),
                    }
                }
                Rule::Terminal => {
                    let node_token = node
                        .data
                        .as_ref()
                        .expect("Missing token for terminal node");
                    match node_token {
                        Token::Symbol(_) => {
                            todo!("Symbol terminals not yet implemented")
                        }
                        Token::IntLiteral(_) => {
                            // TODO: type inference
                            update_node_type_info(
                                &mut node_type_info,
                                node_handle,
                                "int32".to_owned(),
                                &mut stack,
                            );
                        }
                        Token::FloatLiteral(_) => {
                            // TODO: type inference
                            update_node_type_info(
                                &mut node_type_info,
                                node_handle,
                                "float32".to_owned(),
                                &mut stack,
                            );
                        }
                        Token::StringLiteral(_) => todo!(
                            "String literal terminals not yet implemented"
                        ),
                        _ => panic!("Unexpected token for terminal node"),
                    }
                }
                _ => {
                    if node.children.len() == 1 {
                        match type_check_one_child(
                            &node_handle,
                            node,
                            &mut node_type_info,
                            &mut stack,
                        ) {
                            Ok(_) => {}
                            Err(e) => errors.push(e),
                        }
                    } else {
                        match type_check_two_children(
                            tokens,
                            &node_handle,
                            node,
                            &mut node_type_info,
                            &mut stack,
                        ) {
                            Ok(_) => {}
                            Err(e) => errors.push(e),
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

/// Update the node's type information and pop the node off of the stack
fn update_node_type_info(
    node_type_info: &mut HashMap<AstNodeHandle, String>,
    key: AstNodeHandle,
    value: String,
    stack: &mut Vec<AstNodeHandle>,
) {
    node_type_info.insert(key, value);
    stack.pop();
}

fn type_check_statement(
    ast: &Ast,
    tokens: &Tokens,
    node: &AstNode,
    node_type_info: &mut HashMap<AstNodeHandle, String>,
    variable_type_info: &mut HashMap<String, Option<String>>,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), TypeError> {
    if node.children.len() == 1 {
        // Effect statement
        stack.pop(); // doesn't change type info, can be popped off

        // child nodes must still be inspected
        for child in &node.children {
            stack.push(*child);
        }
    } else if node.children.len() == 2 {
        // Assignment statement
        let lhs_handle = node.children[0];
        let rhs_handle = node.children[1];

        // get LHS variable name
        let variable_name = ast.expect_node_name(lhs_handle).clone();

        // check if both children have their type information
        match node_type_info.get(&lhs_handle) {
            Some(lhs_type_info) => match node_type_info.get(&rhs_handle) {
                Some(rhs_type_info) => {
                    if lhs_type_info != rhs_type_info {
                        variable_type_info.insert(variable_name, None);
                        return Err(TypeError {
                            start_line: tokens.expect_line_number(node.start),
                            end_line: tokens
                                .expect_line_number(node.start + node.len - 1),
                            info: format!(
                                "LHS type {} does not match RHS type {}",
                                lhs_type_info, rhs_type_info
                            ),
                        });
                    } else {
                        variable_type_info
                            .insert(variable_name, Some(lhs_type_info.clone()));
                    }
                }
                None => panic!("LHS info without RHS info"),
            },
            None => {
                for child in &node.children {
                    stack.push(*child);
                }
            }
        }
    } else {
        panic!("Statement node without children");
    }

    Ok(())
}

fn type_check_one_child(
    node_handle: &AstNodeHandle,
    node: &AstNode,
    node_type_info: &mut HashMap<AstNodeHandle, String>,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), TypeError> {
    let child_handle = node.children[0];

    match node_type_info.get(&child_handle) {
        Some(child_type) => {
            // inherit type from child
            update_node_type_info(
                node_type_info,
                *node_handle,
                child_type.clone(),
                stack,
            );
        }
        None => {
            // child not evaluated, add to stack
            stack.push(child_handle);
        }
    }

    Ok(())
}

/// A type check for two child nodes that can either evaluate whether there is a type error or
/// add the two children to the stack for evaluation
fn type_check_two_children(
    tokens: &Tokens,
    node_handle: &AstNodeHandle,
    node: &AstNode,
    node_type_info: &mut HashMap<AstNodeHandle, String>,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), TypeError> {
    // Assignment statement
    let lhs_handle = node.children[0];
    let rhs_handle = node.children[1];

    match node_type_info.get(&lhs_handle) {
        Some(lhs_type_info) => match node_type_info.get(&rhs_handle) {
            Some(rhs_type_info) => {
                if lhs_type_info != rhs_type_info {
                    return Err(TypeError {
                        start_line: tokens.expect_line_number(node.start),
                        end_line: tokens
                            .expect_line_number(node.start + node.len - 1),
                        info: format!(
                            "LHS type {} does not match RHS type {}",
                            lhs_type_info, rhs_type_info
                        ),
                    });
                } else {
                    update_node_type_info(
                        node_type_info,
                        *node_handle,
                        lhs_type_info.clone(),
                        stack,
                    );
                }
            }
            None => panic!("LHS info without RHS info"),
        },
        None => {
            for child in &node.children {
                stack.push(*child);
            }
        }
    }

    Ok(())
}

fn find_function_struct_definitions(
    ast: &Ast,
) -> (HashMap<String, FunctionTypeData>, HashMap<String, StructTypeData>) {
    let mut function_type_map: HashMap<String, FunctionTypeData> =
        HashMap::new();
    let mut struct_type_map: HashMap<String, StructTypeData> = HashMap::new();

    let ast_root = match ast.get_root() {
        Some(ast_root) => ast_root,
        None => return (function_type_map, struct_type_map),
    };

    let mut stack = vec![ast_root];

    while let Some(node_handle) = stack.pop() {
        let node = ast.get_node(node_handle);

        match node.rule {
            Rule::FunctionDef => {
                let function_name = match &node.data {
                    Some(token) => match token {
                        Token::Symbol(name) => name.clone(),
                        _ => {
                            todo!()
                        }
                    },
                    None => todo!(),
                };

                let mut argument_types: Vec<String> = vec![];
                let mut return_type: Option<String> = None;
                let mut function_def_stack = node.children.clone();
                while let Some(descendent_handle) = function_def_stack.pop() {
                    let descendent_node = ast.get_node(descendent_handle);
                    match descendent_node.rule {
                        Rule::FunctionArguments => {
                            for child in &descendent_node.children {
                                function_def_stack.push(*child);
                            }
                        }
                        Rule::Declaration => {
                            assert_eq!(descendent_node.children.len(), 2);
                            let symbol_one_name = {
                                let symbol_node =
                                    ast.get_node(descendent_node.children[0]);
                                match &symbol_node.data {
                                    Some(token) => match token {
                                        Token::Symbol(name) => name.clone(),
                                        _ => todo!(),
                                    },
                                    None => todo!(),
                                }
                            };
                            argument_types.push(symbol_one_name);
                        }
                        Rule::ReturnsData => match &descendent_node.data {
                            Some(token) => match token {
                                Token::Symbol(name) => {
                                    return_type = Some(name.clone());
                                }
                                _ => todo!(),
                            },
                            None => todo!(),
                        },
                        _ => {
                            todo!()
                        }
                    }
                }

                match function_type_map.insert(
                    function_name,
                    FunctionTypeData {
                        argument_types: argument_types,
                        return_type: return_type,
                    },
                ) {
                    Some(_) => todo!(
                        "Function is getting redefined. Should return error"
                    ),
                    None => {} // No issue with successful insertion
                };
            }
            Rule::DataStructure => {
                todo!()
            }
            _ => {
                // The default is to pass the dependent ancestor onto the children
                for child in &node.children {
                    stack.push(child.clone());
                }
            }
        }
    }

    (function_type_map, struct_type_map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;
    use crate::tokenize::tokenize;

    #[test]
    fn find_function_defs() {
        todo!()
    }

    #[test]
    fn find_struct_defs() {
        todo!()
    }

    #[test]
    fn arithmetic_expression() {
        let tokens = tokenize("1 + 2").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => {}
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn multiple_type_errors() {
        todo!()
    }

    #[test]
    fn unknown_function() {
        todo!()
    }
}
