pub mod error;

use std::{collections::HashMap, hash::Hash};

use error::TypeError;

use crate::{
    parse::{ast::Ast, rule::Rule},
    tokenize::tokens::Token,
};

struct FunctionTypeData {
    argument_types: Vec<String>,
    return_type: Option<String>,
}

struct StructTypeData {
    filed_to_type: HashMap<String, String>,
}

pub fn type_check(ast: &Ast) -> Result<(), Vec<TypeError>> {
    // TODO: create a operation table for mapping unary and binary operations to valid operands

    // step 1: find all function and struct definitions

    // step 2: perform type checks on ast
    Ok(())
}

fn find_function_struct_definitions(
    ast: &Ast,
) -> (HashMap<String, FunctionTypeData>, HashMap<String, StructTypeData>) {
    let function_type_map: HashMap<String, FunctionTypeData> = HashMap::new();
    let struct_type_map: HashMap<String, StructTypeData> = HashMap::new();

    let ast_root = match ast.get_root() {
        Some(ast_root) => ast_root,
        None => return (function_type_map, struct_type_map),
    };

    let mut stack = vec![ast_root];

    while let Some(node_handle) = stack.pop() {
        let node = ast.get_node(node_handle);

        match node.rule {
            Rule::FunctionDef => {
                let function_name = match node.data {
                    Some(token) => match token {
                        Token::Symbol(name) => name,
                        _ => {
                            todo!()
                        }
                    },
                    None => todo!(),
                };

                let mut argument_types: Vec<String> = vec![];
                let mut return_type: Option<String> = None;
                for child in &node.children {
                    let child_node = ast.get_node(*child);
                    match child_node.rule {
                        Rule::FunctionArguments => {
                            todo!()
                        }
                        Rule::Declaration => {
                            assert_eq!(child_node.children.len(), 2);
                            let symbol_one_name = {
                                let symbol_node =
                                    ast.get_node(child_node.children[0]);
                                match symbol_node.data {
                                    Some(token) => match token {
                                        Token::Symbol(name) => name,
                                        _ => todo!(),
                                    },
                                    None => todo!(),
                                }
                            };
                            argument_types.push(symbol_one_name);
                        }
                        Rule::ReturnsData => {
                            todo!()
                        }
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
                    Some(_) => todo!(), // this function is getting redefined
                    None => {}
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

    Some(dependency_graph)
}
