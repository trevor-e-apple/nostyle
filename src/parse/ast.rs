use crate::tokenize::tokens::Token;

use super::rule::Rule;

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

        while let Some(bfs_data) = stack.pop() {
            let node = if let Some(node) = self.get_node(bfs_data.node_handle) {
                node
            } else {
                panic!();
            };

            // add children to stack
            for child in &node.children {
                stack.push(DfsData {
                    node_handle: child.clone(),
                    depth: bfs_data.depth + 1,
                });
            }

            for _ in 0..bfs_data.depth {
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
