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
    let errors = {
        let ast_root = match ast.get_root() {
            Some(root_handle) => root_handle,
            None => todo!(),
        };

        // TODO: we have to break this up function by function (sets up parallelization)
        let mut errors: Vec<TypeError> = vec![];
        let mut stack = vec![ast_root];

        // Key value pair not present -> child hasn't been evaluated
        // Value is None -> variable / branch had an error
        // Value is Some(String) -> variable / branch evaluated to a type correctly
        let mut node_type_info =
            HashMap::<AstNodeHandle, Option<String>>::new();
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

                    node_type_info
                        .insert(lhs_child_handle, Some(type_name.clone()));
                    variable_type_info
                        .insert(variable_name, Some(type_name.clone()));
                }
                Rule::Statement => {
                    todo!();
                }
                Rule::Terminal => {
                    match node.data.as_ref() {
                        Some(node_token) => {
                            match node_token {
                                Token::Symbol(_) => {
                                    todo!(
                                        "Symbol terminals not yet implemented"
                                    )
                                }
                                Token::IntLiteral(_) => {
                                    // TODO: type inference
                                    update_node_type_info(
                                        &mut node_type_info,
                                        node_handle,
                                        Some("int32".to_owned()),
                                        &mut stack,
                                    );
                                }
                                Token::FloatLiteral(_) => {
                                    // TODO: type inference
                                    update_node_type_info(
                                        &mut node_type_info,
                                        node_handle,
                                        Some("float32".to_owned()),
                                        &mut stack,
                                    );
                                }
                                Token::StringLiteral(_) => todo!(
                            "String literal terminals not yet implemented"
                        ),
                                _ => {
                                    panic!("Unexpected token for terminal node")
                                }
                            }
                        }
                        None => {
                            // No data for this terminal (null terminal)
                            update_node_type_info(
                                &mut node_type_info,
                                node_handle,
                                None,
                                &mut stack,
                            );
                        }
                    };
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

        errors
    };

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Update the node's type information and pop the node off of the stack
fn update_node_type_info(
    node_type_info: &mut HashMap<AstNodeHandle, Option<String>>,
    key: AstNodeHandle,
    value: Option<String>,
    stack: &mut Vec<AstNodeHandle>,
) {
    node_type_info.insert(key, value);
    stack.pop();
}

fn type_check_one_child(
    node_handle: &AstNodeHandle,
    node: &AstNode,
    node_type_info: &mut HashMap<AstNodeHandle, Option<String>>,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), TypeError> {
    let child_handle = node.children[0];

    match node_type_info.get(&child_handle) {
        Some(child_type) => {
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
    node_type_info: &mut HashMap<AstNodeHandle, Option<String>>,
    stack: &mut Vec<AstNodeHandle>,
) -> Result<(), TypeError> {
    // Assignment statement
    let lhs_handle = node.children[0];
    let rhs_handle = node.children[1];

    match node_type_info.get(&lhs_handle) {
        Some(lhs_eval_info) => match node_type_info.get(&rhs_handle) {
            Some(rhs_eval_info) => {
                let lhs_type_info = match lhs_eval_info {
                    Some(type_info) => type_info,
                    None => {
                        // left-hand branch should have already added error
                        update_node_type_info(
                            node_type_info,
                            *node_handle,
                            None,
                            stack,
                        );
                        return Ok(());
                    }
                };
                let rhs_type_info = match rhs_eval_info {
                    Some(type_info) => type_info,
                    None => {
                        // right-hand branch should have already added error
                        update_node_type_info(
                            node_type_info,
                            *node_handle,
                            None,
                            stack,
                        );
                        return Ok(());
                    }
                };

                if lhs_type_info != rhs_type_info {
                    update_node_type_info(
                        node_type_info,
                        *node_handle,
                        None,
                        stack,
                    );
                    return Err(TypeError {
                        start_line: tokens.expect_line_number(node.start),
                        end_line: tokens
                            .expect_line_number(node.start + node.len - 1),
                        info: format!("LHS type does not match RHS type",),
                    });
                } else {
                    update_node_type_info(
                        node_type_info,
                        *node_handle,
                        Some(lhs_type_info.clone()),
                        stack,
                    );
                }
            }
            None => panic!("LHS evaluated without RHS evaluation"),
        },
        None => {
            // LHS is not evaluated
            match node_type_info.get(&rhs_handle) {
                Some(_) => panic!("RHS evaluated without LHS evaluation"),
                None => {
                    // Neither side is evaluated, add to stack for evaluation
                    for child in &node.children {
                        stack.push(*child);
                    }
                }
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
                        Rule::FunctionDefParameters => {
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
                        Rule::ReturnsData => {
                            let terminal_child_handle =
                                descendent_node.children[0];
                            let terminal_child_node =
                                ast.get_node(terminal_child_handle);
                            match &terminal_child_node.data {
                                Some(token) => match token {
                                    Token::Symbol(name) => {
                                        return_type = Some(name.clone());
                                    }
                                    _ => todo!(),
                                },
                                None => todo!(),
                            }
                        }
                        Rule::BraceExpression => {
                            // nothing to do with a brace expression
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
        let tokens = tokenize(
            "
            fn test1(int32 a, int32 b) {
            }

            fn test2(float32 a, float32 b) returns float64 {
            }

            fn test3(mytype a) {
            }
        ",
        )
        .expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        let (function_data, _) = find_function_struct_definitions(&ast);

        let test1_data =
            function_data.get("test1").expect("Missing test1 data");
        assert_eq!(test1_data.argument_types.len(), 2);
        assert_eq!(test1_data.argument_types[0], "int32");
        assert_eq!(test1_data.argument_types[1], "int32");
        match test1_data.return_type {
            Some(_) => assert!(false),
            None => {}
        };

        let test2_data =
            function_data.get("test2").expect("Missing test2 data");
        assert_eq!(test2_data.argument_types.len(), 2);
        assert_eq!(test2_data.argument_types[0], "float32");
        assert_eq!(test2_data.argument_types[1], "float32");
        let test2_return_type =
            test2_data.return_type.as_ref().expect("Missing test2 return type");
        assert_eq!(test2_return_type, "float64");

        let test3_data =
            function_data.get("test3").expect("Missing test3 data");
        assert_eq!(test3_data.argument_types.len(), 1);
        assert_eq!(test3_data.argument_types[0], "mytype");
        match test1_data.return_type {
            Some(_) => assert!(false),
            None => {}
        };
    }

    #[test]
    fn find_struct_defs() {
        todo!()
    }

    #[test]
    fn float_and_int() {
        let tokens = tokenize("1 + 2.0").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => {
                assert!(false)
            }
            Err(errors) => {
                assert_eq!(errors.len(), 1)
            }
        }
    }

    #[test]
    fn three_term_float_and_int() {
        let tokens =
            tokenize("1 + 2.0 * 3.0").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => {
                assert!(false)
            }
            Err(errors) => {
                assert_eq!(errors.len(), 1)
            }
        }
    }

    #[test]
    fn four_term_float_and_int() {
        let tokens =
            tokenize("2 * 3.0 + 4 * 5.0").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => {
                assert!(false)
            }
            Err(errors) => {
                assert_eq!(errors.len(), 2)
            }
        }
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
    fn arithmetic_expression_float() {
        let tokens = tokenize("1.0 + 2.0").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => {}
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn three_term_expression() {
        let tokens = tokenize("1 + 2 * 3").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => {}
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn function_call_no_arguments() {
        let tokens = tokenize("test()").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => {}
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn function_call_one_argument() {
        let tokens = tokenize("
            fn test(int32 a) {
                a
            }

            fn test2 {
                int32 me = 0;
                test(me);
            }").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => {},
            Err(_) => {assert!(false)},
        }
    }

    #[test]
    fn function_call_one_argument_undeclared() {
        let tokens = tokenize("
            fn test(int32 a) {
                a
            }

            fn test2 {
                test(me);
            }
        ").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => todo!(),
            Err(_) => todo!(),
        }
    }

    #[test]
    fn function_call_one_argument_bad_type() {
        let tokens = tokenize("
            fn test(int32 a) {
                a
            }

            fn test2 {
                float32 me = 0.0;
                test(me);
            }
        ").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        match type_check(&tokens, &ast) {
            Ok(_) => todo!(),
            Err(_) => todo!(),
        }
    }

    #[test]
    fn missing_function_definition() {
        todo!()
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
