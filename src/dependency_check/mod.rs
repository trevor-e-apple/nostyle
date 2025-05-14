// TODO: check for function call dependency validity
// TODO: check for structure 
use std::{collections::HashMap};

use crate::parse::{ast::{Ast, AstNodeHandle}, rule::Rule};

// The dependency graph abstractly is a directed graph. The list of edges is the list of dependencies for   
type DependencyGraph = HashMap<AstNodeHandle, Vec<AstNodeHandle>>;

struct SearchData {
    ast_handle: AstNodeHandle,
    dependent_ancestor: Option<AstNodeHandle>,
}

/// If the search data contains a dependent ancestor, update
fn add_dependency(search_data: &SearchData, node_handle: AstNodeHandle, dependency_graph: &mut DependencyGraph) {
    match search_data.dependent_ancestor {
        Some(dependent_ancestor) => {
            let edges = match dependency_graph.get_mut(&dependent_ancestor) {
                Some(edges) => edges,
                None => panic!("Bad dependent ancestor"),
            };

            edges.push(node_handle);
        },
        None => {}, // nothing to do if there is no dependent ancestor
    };
}

/// Makes the dependency graph and checks it for circular dependencies
/// Provides a mapping of functions to their return types (for use by type check stage)
pub fn dependency_check(ast: Ast) -> Option<DependencyGraph> {
    let ast_root = match ast.get_root() {
        Some(ast_root) => ast_root,
        None => return None,
    };

    let mut stack = vec![SearchData {ast_handle: ast_root, dependent_ancestor: None}];
    let mut dependency_graph = DependencyGraph::new();

    while let Some(search_data) = stack.pop() {
        let node_handle = search_data.ast_handle;

        let node = ast.get_node(node_handle);

        match node.rule {
            Rule::FunctionDef => {
                dependency_graph.insert(node_handle, vec![]);

                for child in &node.children {
                    stack.push(SearchData {ast_handle: child.clone(), dependent_ancestor: Some(node_handle)});
                }
            },
            Rule::FunctionCall => {
                add_dependency(&search_data, node_handle, &mut dependency_graph);

                for child in &node.children {
                    stack.push(SearchData {ast_handle: child.clone(), dependent_ancestor: search_data.dependent_ancestor});
                }
            }
            Rule::DataStructure => {
                dependency_graph.insert(node_handle, vec![]);
                add_dependency(&search_data, node_handle, &mut dependency_graph);

                for child in &node.children {
                    stack.push(SearchData {ast_handle: child.clone(), dependent_ancestor: Some(node_handle)});
                }
            },
            _ => {
                // The default is to pass the dependent ancestor onto the children
                for child in &node.children {
                    stack.push(SearchData {ast_handle: child.clone(), dependent_ancestor: search_data.dependent_ancestor});
                }
            }
        }
    }

    // TODO: check for circular dependencies
    todo!()
}
