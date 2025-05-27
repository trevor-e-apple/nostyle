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
            let node_handle = match stack.get(stack.len()) {
                Some(node_handle) => node_handle.clone(),
                None => break,
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
                    type_check_statement(ast, tokens, node, &mut node_type_info, &mut variable_type_info, &mut stack);
                }
                Rule::PlusMinus => {
                    type_check_two_children(ast, tokens, node, &mut node_type_info, &mut variable_type_info, &mut stack);
                }
                Rule::MultDiv => {
                    type_check_two_children(ast, tokens, node, &mut node_type_info, &mut variable_type_info, &mut stack);
                }
                _ => {
                    let children_evaluated: bool = {
                        let mut children_evaluated = true;
                        for child in &node.children {
                            if !node_type_info.contains_key(child) {
                                children_evaluated = false;
                                break;
                            }
                        }
                        children_evaluated
                    };

                    if !children_evaluated {
                        for child in &node.children {
                            stack.push(*child);
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn type_check_statement(
    ast: &Ast,
    tokens: &Tokens,
    node: &AstNode,
    node_type_info: &mut HashMap<AstNodeHandle, String>,
    variable_type_info: &mut HashMap::<String, Option<String>>,
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
        match type_check_two_children(ast, tokens, node, node_type_info, variable_type_info, stack) {
            Ok(_) => {},
            Err(e) => return Err(e),
        }
    } else {
        panic!("Statement node without children");
    }

    Ok(())
}

/// A type check for two child nodes that can either evaluate whether there is a type error or
/// add the two children to the stack for evaluation
fn type_check_two_children(
    ast: &Ast,
    tokens: &Tokens,
    node: &AstNode,
    node_type_info: &mut HashMap<AstNodeHandle, String>,
    variable_type_info: &mut HashMap::<String, Option<String>>,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), TypeError> {
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
