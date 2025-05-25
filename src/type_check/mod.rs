pub mod error;

use std::{collections::HashMap, hash::Hash};

use error::TypeError;

use crate::{
    parse::{
        ast::{Ast, AstNodeHandle},
        rule::Rule,
    },
    tokenize::tokens::Token,
};

struct FunctionTypeData {
    argument_types: Vec<String>,
    return_type: Option<String>,
}

struct StructTypeData {
    struct_to_type: HashMap<String, String>,
}

pub fn type_check(ast: &Ast) -> Result<(), Vec<TypeError>> {
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

        // TODO: we have to break this up function by function

        let mut stack = vec![ast_root];
        let mut node_type_info = HashMap::<AstNodeHandle, String>::new();
        let mut variable_type_info = HashMap::<String, String>::new();

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
                    let type_name = {
                        let node = ast.get_node(lhs_child_handle);

                        match &node.data {
                            Some(name_token) => match name_token {
                                Token::Symbol(name) => name.clone(),
                                _ => {
                                    todo!("This shouldn't happen");
                                }
                            },
                            None => todo!("This shouldn't happen"),
                        }
                    };

                    let variable_name = {
                        let handle = &node.children[1];
                        let node = ast.get_node(*handle);

                        match &node.data {
                            Some(name_token) => match name_token {
                                Token::Symbol(name) => name.clone(),
                                _ => {
                                    todo!("This shouldn't happen");
                                }
                            },
                            None => todo!("This shouldn't happen"),
                        }
                    };
                    todo!("Clean up getting node's name");

                    node_type_info.insert(lhs_child_handle, type_name.clone());
                    variable_type_info.insert(variable_name, type_name.clone());
                }
                Rule::Statement => {
                    todo!("We need to track variable type info")
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

                    if children_evaluated {
                        todo!("Determine the parent's type and throw an error if the evaluation is invalid");
                    } else {
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

fn type_check_statement() {}

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
