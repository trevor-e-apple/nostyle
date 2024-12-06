use std::borrow::Cow;

use crate::tokenize::tokens::Token;

use super::rule::Rule;

type AstEdge = (AstNodeHandle, AstNodeHandle);

pub struct Ast {
    nodes: Vec<AstNode>,
}

impl Ast {
    pub fn new() -> Self {
        Self { nodes: vec![] }
    }

    pub fn add_root(&mut self, rule: Rule) -> AstNodeHandle {
        assert!(self.nodes.len() == 0);
        self.nodes.push(AstNode {
            rule,
            parent: None,
            children: vec![],
            data: None,
        });
        AstNodeHandle { index: 0 }
    }

    pub fn get_root(&self) -> Option<AstNodeHandle> {
        if self.nodes.is_empty() {
            None
        } else {
            Some(AstNodeHandle { index: 0 })
        }
    }

    pub fn add_child_with_data(
        &mut self,
        parent_handle: AstNodeHandle,
        rule: Rule,
        data: Option<Token>,
    ) -> AstNodeHandle {
        let current_len = self.nodes.len();
        self.nodes.push(AstNode {
            rule,
            parent: Some(parent_handle),
            children: vec![],
            data,
        });

        let result = AstNodeHandle { index: current_len };

        match self.get_node_mut(parent_handle) {
            Some(parent_node) => parent_node.children.push(result.clone()),
            None => todo!("panic?"),
        }

        result
    }

    pub fn add_child(
        &mut self,
        parent_handle: AstNodeHandle,
        rule: Rule,
    ) -> AstNodeHandle {
        self.add_child_with_data(parent_handle, rule, None)
    }

    pub fn add_terminal_child(
        &mut self,
        parent_handle: AstNodeHandle,
        data: Option<Token>,
    ) -> AstNodeHandle {
        let current_len = self.nodes.len();
        self.nodes.push(AstNode {
            rule: Rule::Terminal,
            parent: Some(parent_handle),
            children: vec![],
            data,
        });

        let result = AstNodeHandle { index: current_len };

        match self.get_node_mut(parent_handle) {
            Some(parent_node) => parent_node.children.push(result.clone()),
            None => todo!("panic?"),
        }

        result
    }

    pub fn get_node(&self, node_handle: AstNodeHandle) -> Option<&AstNode> {
        self.nodes.get(node_handle.index)
    }

    pub fn get_node_mut(
        &mut self,
        node_handle: AstNodeHandle,
    ) -> Option<&mut AstNode> {
        self.nodes.get_mut(node_handle.index)
    }

    /// whether or not two ast's are equivalent
    #[cfg(test)]
    pub fn equivalent(a: &Self, b: &Self) -> bool {
        if a.nodes.len() == 0 && b.nodes.len() == 0 {
            return true;
        }

        let a_root = if let Some(a_root) = a.get_root() {
            a_root
        } else {
            return false;
        };
        let b_root = if let Some(b_root) = b.get_root() {
            b_root
        } else {
            return false;
        };

        let mut a_stack: Vec<AstNodeHandle> = vec![a_root];
        let mut b_stack: Vec<AstNodeHandle> = vec![b_root];

        loop {
            if let Some(a_node_handle) = a_stack.pop() {
                if let Some(b_node_handle) = b_stack.pop() {
                    let a_node =
                        a.get_node(a_node_handle).expect("Bad AST handle");
                    let b_node =
                        b.get_node(b_node_handle).expect("Bad AST handle");

                    if a_node != b_node {
                        return false;
                    } else {
                        // add children to the stack
                        for child in &a_node.children {
                            a_stack.push(*child);
                        }
                        for child in &b_node.children {
                            b_stack.push(*child);
                        }
                    }
                } else {
                    // a still had data, but b did not
                    return false;
                }
            } else {
                break;
            }
        }

        // loop broke because a_stack was out of data. b_stack should be too
        b_stack.len() == 0
    }

    /// prints an AST
    #[cfg(test)]
    pub fn print(&self) {
        use std::panic;

        struct DfsData {
            node_handle: AstNodeHandle,
            depth: i32,
        }

        let root = if let Some(root) = self.get_root() {
            root
        } else {
            // nothing to print
            return;
        };

        let mut stack: Vec<DfsData> =
            vec![DfsData { node_handle: root, depth: 0 }];

        while let Some(dfs_data) = stack.pop() {
            let node = if let Some(node) = self.get_node(dfs_data.node_handle) {
                node
            } else {
                panic!();
            };

            // add children to stack in reverse so they are expanded from left to right
            for child_handle in (&node.children).into_iter().rev() {
                stack.push(DfsData {
                    node_handle: child_handle.clone(),
                    depth: dfs_data.depth + 1,
                });
            }

            for _ in 0..dfs_data.depth {
                print!("    ");
            }
            println!("{:?}", node.make_terse_string());
        }
    }
}

#[derive(Copy, Clone)]
pub struct AstNodeHandle {
    index: usize,
}

pub struct AstNode {
    pub rule: Rule,
    pub parent: Option<AstNodeHandle>,
    pub children: Vec<AstNodeHandle>,
    pub data: Option<Token>,
}

impl AstNode {
    #[cfg(test)]
    pub fn make_terse_string(&self) -> String {
        format!("Rule: {:?} Data: {:?}", self.rule, self.data)
    }
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        if self.rule == other.rule {
            if let Some(self_data) = &self.data {
                if let Some(other_data) = &other.data {
                    // same data
                    *self_data == *other_data
                } else {
                    // self has data, but other does not
                    false
                }
            } else {
                if let Some(_) = &other.data {
                    // other has data, but self does not
                    false
                } else {
                    // neither has data
                    true
                }
            }
        } else {
            // rules don't match
            false
        }
    }
}

impl<'a> dot::Labeller<'a, AstNodeHandle, AstEdge> for Ast {
    fn graph_id(&'a self) -> dot::Id<'a> {
        // TODO: hard-coded since not used?
        dot::Id::new("graphid").unwrap()
    }

    fn node_id(&'a self, node_handle: &AstNodeHandle) -> dot::Id<'a> {
        match dot::Id::new(format!("N{:?}", node_handle.index)) {
            Ok(dot_id) => dot_id,
            Err(error) => {
                println!("{:?}", error);
                todo!()
            }
        }
    }

    fn node_label<'b>(
        &'b self,
        node_handle: &AstNodeHandle,
    ) -> dot::LabelText<'b> {
        let node = self
            .get_node(*node_handle)
            .expect("Unexpected failure to get node");
        dot::LabelText::LabelStr(
            format!("Rule: {:?}, Data: {:?}", node.rule, node.data).into(),
        )
    }
}

impl<'a> dot::GraphWalk<'a, AstNodeHandle, AstEdge> for Ast {
    fn nodes(&self) -> dot::Nodes<'a, AstNodeHandle> {
        let mut nodes = Vec::with_capacity(self.nodes.len());
        for index in 0..self.nodes.len() {
            nodes.push(AstNodeHandle { index });
        }
        Cow::Owned(nodes)
    }

    fn edges(&'a self) -> dot::Edges<'a, AstEdge> {
        let mut stack: Vec<AstNodeHandle> = Vec::new();
        let mut edges: Vec<AstEdge> = Vec::with_capacity(2 * self.nodes.len());
        let root = match self.get_root() {
            Some(root) => root,
            None => return Cow::Owned(edges),
        };

        stack.push(root);
        loop {
            let node_handle = match stack.pop() {
                Some(node_handle) => node_handle,
                None => break,
            };

            let node = match self.get_node(node_handle) {
                Some(node) => node,
                None => continue,
            };

            for child in &node.children {
                edges.push((node_handle, *child));
                stack.push(*child);
            }
        }
        Cow::Owned(edges)
    }

    fn source(&self, e: &AstEdge) -> AstNodeHandle {
        e.0
    }

    fn target(&self, e: &AstEdge) -> AstNodeHandle {
        e.1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_empty() {
        let a = Ast::new();
        let mut b = Ast::new();
        b.add_root(Rule::Expression);
        assert!(!Ast::equivalent(&a, &b));
    }

    #[test]
    fn b_empty() {
        let mut a = Ast::new();
        let b = Ast::new();
        a.add_root(Rule::Expression);
        assert!(!Ast::equivalent(&a, &b));
    }

    #[test]
    fn same_size_not_equivalent() {
        let mut a = Ast::new();
        let mut b = Ast::new();

        let root_handle = a.add_root(Rule::Expression);
        a.add_child(root_handle, Rule::BraceExpression);

        let root_handle = b.add_root(Rule::Expression);
        b.add_child(root_handle, Rule::Equality);

        assert!(!Ast::equivalent(&a, &b));
    }

    #[test]
    fn a_excess_data() {
        let mut a = Ast::new();
        let mut b = Ast::new();

        let root_handle = a.add_root(Rule::Expression);
        let brace_expression_handle =
            a.add_child(root_handle, Rule::BraceExpression);
        a.add_child(brace_expression_handle, Rule::BraceStatements);

        let root_handle = b.add_root(Rule::Expression);
        b.add_child(root_handle, Rule::BraceExpression);

        assert!(!Ast::equivalent(&a, &b));
    }
}
