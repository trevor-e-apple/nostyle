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
        self.nodes.push(AstNode { rule, parent: None, children: vec![] });
        AstNodeHandle { index: 0 }
    }

    pub fn add_child(
        &mut self,
        parent_handle: AstNodeHandle,
        rule: Rule,
    ) -> AstNodeHandle {
        let current_len = self.nodes.len();
        self.nodes.push(AstNode {
            rule,
            parent: Some(parent_handle),
            children: vec![],
        });

        let result = AstNodeHandle { index: current_len };

        match self.get_node(parent_handle) {
            Some(parent_node) => parent_node.children.push(result.clone()),
            None => todo!("panic?"),
        }

        result
    }

    pub fn get_node(&self, node_handle: AstNodeHandle) -> Option<&AstNode> {
        self.nodes.get(node_handle.index)
    }
}

#[derive(Clone)]
pub struct AstNodeHandle {
    index: usize,
}

pub struct AstNode {
    pub rule: Rule,
    pub parent: Option<AstNodeHandle>,
    pub children: Vec<AstNodeHandle>,
}
