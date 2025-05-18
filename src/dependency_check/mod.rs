// TODO: check for function call dependency validity
// TODO: check for structure
use std::collections::HashMap;

use crate::parse::{
    ast::{Ast, AstNodeHandle},
    rule::Rule,
};

// The dependency graph abstractly is a directed graph. The list of edges is the list of dependencies for
type DependencyGraph = HashMap<AstNodeHandle, Vec<AstNodeHandle>>;

struct SearchData {
    ast_handle: AstNodeHandle,
    dependent_ancestor: Option<AstNodeHandle>,
}

enum DependencyError {
    CircularDependency,
}

/// If the search data contains a dependent ancestor, update
fn add_dependency(
    search_data: &SearchData,
    new_dependency_handle: AstNodeHandle,
    dependency_graph: &mut DependencyGraph,
) {
    match search_data.dependent_ancestor {
        Some(dependent_ancestor) => {
            let edges = match dependency_graph.get_mut(&dependent_ancestor) {
                Some(edges) => edges,
                None => panic!("Bad dependent ancestor"),
            };

            edges.push(new_dependency_handle);
        }
        None => {} // nothing to do if there is no dependent ancestor
    };
}

fn make_dependency_graph(ast: &Ast) -> Option<DependencyGraph> {
    let ast_root = match ast.get_root() {
        Some(ast_root) => ast_root,
        None => return None,
    };

    let mut stack =
        vec![SearchData { ast_handle: ast_root, dependent_ancestor: None }];
    let mut dependency_graph = DependencyGraph::new();

    while let Some(search_data) = stack.pop() {
        let node_handle = search_data.ast_handle;

        let node = ast.get_node(node_handle);

        match node.rule {
            Rule::FunctionDef => {
                dependency_graph.insert(node_handle, vec![]);

                for child in &node.children {
                    stack.push(SearchData {
                        ast_handle: child.clone(),
                        dependent_ancestor: Some(node_handle),
                    });
                }
            }
            Rule::FunctionCall => {
                add_dependency(
                    &search_data,
                    node_handle,
                    &mut dependency_graph,
                );

                for child in &node.children {
                    stack.push(SearchData {
                        ast_handle: child.clone(),
                        dependent_ancestor: search_data.dependent_ancestor,
                    });
                }
            }
            Rule::DataStructure => {
                dependency_graph.insert(node_handle, vec![]);
                add_dependency(
                    &search_data,
                    node_handle,
                    &mut dependency_graph,
                );

                for child in &node.children {
                    stack.push(SearchData {
                        ast_handle: child.clone(),
                        dependent_ancestor: Some(node_handle),
                    });
                }
            }
            _ => {
                // The default is to pass the dependent ancestor onto the children
                for child in &node.children {
                    stack.push(SearchData {
                        ast_handle: child.clone(),
                        dependent_ancestor: search_data.dependent_ancestor,
                    });
                }
            }
        }
    }

    Some(dependency_graph)
}

/// Makes the dependency graph and checks it for circular dependencies
/// Provides a mapping of functions to their return types (for use by type check stage)
pub fn dependency_check(ast: &Ast) -> Result<(), Vec<DependencyError>> {
    let dependency_graph = make_dependency_graph(ast);

    // TODO: check for circular dependencies
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::{parse::parse, tokenize::tokenize};

    use super::*;

    #[test]
    fn no_function_def() {
        let tokens = tokenize("a + b").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        let dependency_graph = make_dependency_graph(&ast)
            .expect("Unexpected error making dependency graph");
        assert_eq!(dependency_graph.len(), 0);
    }

    #[test]
    fn no_dependencies() {
        let tokens =
            tokenize("fn test() {a + b}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");
        let dependency_graph = make_dependency_graph(&ast)
            .expect("Unexpected error making dependency graph");
        assert_eq!(dependency_graph.len(), 1);

        for (node_handle, dependencies) in dependency_graph {
            let node = ast.get_node(node_handle);
            assert_eq!(node.rule, Rule::FunctionDef);
            assert_eq!(dependencies.len(), 0);
        }
    }

    #[test]
    fn one_dependency() {
        todo!();
    }

    #[test]
    fn two_dependencies() {
        todo!();
    }

    #[test]
    fn three_dependencies() {
        todo!();
    }

    #[test]
    fn multiple_function_defs_with_dependencies() {
        todo!();
    }

    #[test]
    fn out_of_order_dependencies() {
        todo!();
    }

    #[test]
    fn circular_dependency() {
        todo!();
    }
}
