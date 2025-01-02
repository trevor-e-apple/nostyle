mod dependency_graph;
pub mod error;

use dependency_graph::DependencyGraph;
use error::TypeError;

use crate::parse::ast::Ast;

pub fn type_check(ast: &Ast) -> Result<(), Vec<TypeError>> {
    // step 1 construct a dependency tree, raise an error if there is a circular dependency anywhere
    let dependency_graph = make_dependency_tree(ast);
    // step 2 perform type checking within each function
    Ok(())
}

fn make_dependency_tree(function_asts: &Ast) -> DependencyGraph {
    todo!()
}

fn function_level_type_check() -> Result<(), TypeError> {
    todo!()
}
