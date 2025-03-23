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

    pub fn add_root(
        &mut self,
        rule: Rule,
        start: usize,
        len: usize,
    ) -> AstNodeHandle {
        assert!(self.nodes.len() == 0);
        self.nodes.push(AstNode {
            rule,
            parent: None,
            children: vec![],
            data: None,
            start,
            len,
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
        start: usize,
        len: usize,
    ) -> AstNodeHandle {
        let current_len = self.nodes.len();
        self.nodes.push(AstNode {
            rule,
            parent: Some(parent_handle),
            children: vec![],
            data,
            start,
            len,
        });

        let result = AstNodeHandle { index: current_len };

        let parent_node = self.get_node_mut(parent_handle);
        parent_node.children.push(result.clone());

        result
    }

    pub fn add_child(
        &mut self,
        parent_handle: AstNodeHandle,
        rule: Rule,
        start: usize,
        len: usize,
    ) -> AstNodeHandle {
        self.add_child_with_data(parent_handle, rule, None, start, len)
    }

    pub fn add_terminal_child(
        &mut self,
        parent_handle: AstNodeHandle,
        data: Option<Token>,
        start: usize,
        len: usize,
    ) -> AstNodeHandle {
        let current_len = self.nodes.len();
        self.nodes.push(AstNode {
            rule: Rule::Terminal,
            parent: Some(parent_handle),
            children: vec![],
            data,
            start,
            len,
        });

        let result = AstNodeHandle { index: current_len };

        let parent_node = self.get_node_mut(parent_handle);
        parent_node.children.push(result.clone());

        result
    }

    pub fn get_node(&self, node_handle: AstNodeHandle) -> &AstNode {
        match self.nodes.get(node_handle.index) {
            Some(node) => node,
            None => panic!("Bad node handle"),
        }
    }

    pub fn get_node_mut(&mut self, node_handle: AstNodeHandle) -> &mut AstNode {
        match self.nodes.get_mut(node_handle.index) {
            Some(node) => node,
            None => panic!("Bad node handle"),
        }
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
                    let a_node = a.get_node(a_node_handle);
                    let b_node = b.get_node(b_node_handle);

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
            let node = self.get_node(dfs_data.node_handle);

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

#[cfg(test)]
/// prints a tree up to the point where the two input trees diverge
pub fn get_diff_string(ast_one: &Ast, ast_two: &Ast) -> String {
    let mut result = String::new();
    struct DfsData {
        node_handle: AstNodeHandle,
        depth: i32,
    }

    let mut stack_one: Vec<DfsData> = vec![];
    if let Some(root) = ast_one.get_root() {
        stack_one.push(DfsData { node_handle: root, depth: 0 })
    }
    let mut stack_two: Vec<DfsData> = vec![];
    if let Some(root) = ast_two.get_root() {
        stack_two.push(DfsData { node_handle: root, depth: 0 })
    }

    loop {
        let dfs_data_one = stack_one.pop();
        let dfs_data_two = stack_two.pop();

        match dfs_data_one {
            Some(dfs_data_one) => match dfs_data_two {
                Some(dfs_data_two) => {
                    // data from 1 and 2
                    let node_one = ast_one.get_node(dfs_data_one.node_handle);
                    let node_two = ast_two.get_node(dfs_data_two.node_handle);

                    for _ in 0..dfs_data_one.depth {
                        result.push_str("    ");
                    }
                    result.push_str(&format!(
                        "{}\n",
                        node_one.make_terse_string()
                    ));

                    if node_one == node_two {
                        for (child_one_handle, child_two_handle) in (&node_one
                            .children)
                            .into_iter()
                            .rev()
                            .zip((&node_two.children).into_iter().rev())
                        {
                            stack_one.push(DfsData {
                                node_handle: child_one_handle.clone(),
                                depth: dfs_data_one.depth + 1,
                            });
                            stack_two.push(DfsData {
                                node_handle: child_two_handle.clone(),
                                depth: dfs_data_one.depth + 1,
                            });
                        }
                    } else {
                        // do not push children onto the stack if the two nodes are unequal
                    }
                }
                None => {
                    // data from 1, not 2
                    panic!("Unequal stack sizes")
                }
            },
            None => match dfs_data_two {
                Some(_) => {
                    // no data from 1. data from 2
                    panic!("Unequal stack sizes")
                }
                None => {
                    // no data from 1 or from 2
                    break;
                }
            },
        }
    }

    result
}

#[derive(Copy, Clone, Debug)]
pub struct AstNodeHandle {
    index: usize,
}

#[derive(Debug)]
pub struct AstNode {
    pub rule: Rule,
    pub parent: Option<AstNodeHandle>,
    pub children: Vec<AstNodeHandle>,
    pub data: Option<Token>,
    pub start: usize,
    pub len: usize,
}

impl AstNode {
    #[cfg(test)]
    pub fn make_terse_string(&self) -> String {
        format!(
            "Rule: {:?} Data: {:?} Start: {:?} Len: {:?} ChildrenLen: {:?}",
            self.rule,
            self.data,
            self.start,
            self.len,
            self.children.len()
        )
    }

    pub fn get_end_index(&self) -> usize {
        self.start + self.len - 1
    }
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        if self.rule == other.rule {
            if let Some(self_data) = &self.data {
                if let Some(other_data) = &other.data {
                    // same data
                    (*self_data == *other_data)
                        && (self.start == other.start)
                        && (self.len == other.len)
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
                    (self.start == other.start) && (self.len == other.len)
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
        let node = self.get_node(*node_handle);
        dot::LabelText::LabelStr(
            format!(
                "Rule: {:?}, Data: {:?}, Start: {:?}, Len: {:?}",
                node.rule, node.data, node.start, node.len
            )
            .into(),
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

            let node = self.get_node(node_handle);

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
        b.add_root(Rule::Expression, 0, 1);
        assert!(!Ast::equivalent(&a, &b));
    }

    #[test]
    fn b_empty() {
        let mut a = Ast::new();
        let b = Ast::new();
        a.add_root(Rule::Expression, 0, 1);
        assert!(!Ast::equivalent(&a, &b));
    }

    fn are_equivalent_asts() -> (Ast, Ast) {
        let mut a = Ast::new();
        let mut b = Ast::new();

        let root_handle = a.add_root(Rule::Expression, 0, 1);
        a.add_child(root_handle, Rule::Equality, 0, 1);

        let root_handle = b.add_root(Rule::Expression, 0, 1);
        b.add_child(root_handle, Rule::Equality, 0, 1);

        (a, b)
    }

    #[test]
    fn are_equivalent() {
        let (a, b) = are_equivalent_asts();

        assert!(Ast::equivalent(&a, &b));
    }

    #[test]
    fn are_equivalent_diff_string() {
        let (a, b) = are_equivalent_asts();
        let diff_string = get_diff_string(&a, &b);
        
        // expected to print the entire tree
        let expected_string = concat!(
            "Rule: Expression Data: None Start: 0 Len: 1 ChildrenLen: 1\n",
            "    Rule: Equality Data: None Start: 0 Len: 1 ChildrenLen: 0\n"
        );
        assert_eq!(expected_string, diff_string);
    }

    fn same_size_not_equivalent_asts() -> (Ast, Ast) {
        let mut a = Ast::new();
        let mut b = Ast::new();

        let root_handle = a.add_root(Rule::Expression, 0, 1);
        a.add_child(root_handle, Rule::BraceExpression, 0, 1);

        let root_handle = b.add_root(Rule::Expression, 0, 1);
        b.add_child(root_handle, Rule::Equality, 0, 1);

        (a, b)
    }

    #[test]
    fn same_size_not_equivalent_equivalent() {
        let (a, b) = same_size_not_equivalent_asts();
        assert!(!Ast::equivalent(&a, &b));
    }

    #[test]
    fn same_size_not_equivalent_diff_string() {
        let (a, b) = same_size_not_equivalent_asts();
        let diff_string = get_diff_string(&a, &b);
        // expected to print the whole first tree, since the diff is on a leaf
        let expected_string = concat!(
            "Rule: Expression Data: None Start: 0 Len: 1 ChildrenLen: 1\n",
            "    Rule: BraceExpression Data: None Start: 0 Len: 1 ChildrenLen: 0\n"
        );
        print!("{}", expected_string);
        print!("{}", diff_string);
        assert_eq!(diff_string, expected_string);
    }

    fn same_structure_diff_len_asts() -> (Ast, Ast) {
        let mut a = Ast::new();
        let mut b = Ast::new();

        let root_handle = a.add_root(Rule::Expression, 0, 2);
        a.add_child(root_handle, Rule::BraceExpression, 0, 2);

        let root_handle = b.add_root(Rule::Expression, 0, 1);
        b.add_child(root_handle, Rule::BraceExpression, 0, 1);

        (a, b)
    }

    #[test]
    fn same_structure_not_equivalent_len() {
        let (a, b) = same_structure_diff_len_asts();
        assert!(!Ast::equivalent(&a, &b));
    }

    #[test]
    fn same_structure_diff_len_diff_string() {
        let (a, b) = same_structure_diff_len_asts();
        let diff_string = get_diff_string(&a, &b);
        // different len is at the root. the root of a is expected to print
        let expected_string = concat!(
            "Rule: Expression Data: None Start: 0 Len: 2 ChildrenLen: 1\n"
        );
        print!("Expected:\n{}", expected_string);
        print!("Actual:\n{}", diff_string);
        assert!(diff_string == expected_string);
    }

    #[test]
    fn same_structure_not_equivalent_start() {
        let mut a = Ast::new();
        let mut b = Ast::new();

        let root_handle = a.add_root(Rule::Expression, 0, 2);
        a.add_child(root_handle, Rule::BraceExpression, 1, 2);

        let root_handle = b.add_root(Rule::Expression, 0, 2);
        b.add_child(root_handle, Rule::BraceExpression, 0, 2);

        assert!(!Ast::equivalent(&a, &b));
    }

    #[test]
    fn a_excess_data() {
        let mut a = Ast::new();
        let mut b = Ast::new();

        let root_handle = a.add_root(Rule::Expression, 0, 3);
        let brace_expression_handle =
            a.add_child(root_handle, Rule::BraceExpression, 0, 3);
        a.add_child(brace_expression_handle, Rule::BraceStatements, 1, 1);

        let root_handle = b.add_root(Rule::Expression, 0, 3);
        b.add_child(root_handle, Rule::BraceExpression, 0, 3);

        assert!(!Ast::equivalent(&a, &b));
    }

    #[test]
    fn ast_node_data_mismatch_a() {
        let a = AstNode {
            rule: Rule::Expression,
            parent: None,
            children: vec![],
            data: Some(Token::Assign),
            start: 0,
            len: 3,
        };
        let b = AstNode {
            rule: Rule::Expression,
            parent: None,
            children: vec![],
            data: None,
            start: 0,
            len: 3,
        };

        assert_ne!(&a, &b);
    }

    #[test]
    fn ast_node_data_mismatch_b() {
        let a = AstNode {
            rule: Rule::Expression,
            parent: None,
            children: vec![],
            data: None,
            start: 0,
            len: 3,
        };
        let b = AstNode {
            rule: Rule::Expression,
            parent: None,
            children: vec![],
            data: Some(Token::Assign),
            start: 0,
            len: 3,
        };

        assert_ne!(&a, &b);
    }
}
