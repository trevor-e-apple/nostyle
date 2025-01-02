pub struct DependencyGraph {
    nodes: Vec<DependencyGraphNode>,
}

pub struct DependencyGraphHandle {
    index: usize,
}

pub struct DependencyGraphNode {
    parent: DependencyGraphHandle,
    children: Vec<DependencyGraphHandle>,
}
