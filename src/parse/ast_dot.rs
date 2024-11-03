use dot;
use std::borrow::Cow;
use std::io::Write;

type Nd = isize;
type Ed = (isize, isize);
struct Edges(Vec<Ed>);

pub fn render_to<W: Write>(output: &mut W) {
    let edges = Edges(vec![(0, 1), (0, 2), (1, 3), (2, 3), (3, 4), (4, 4)]);
    dot::render(&edges, output).unwrap()
}

impl<'a> dot::Labeller<'a, Nd, Ed> for Edges {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("example1").unwrap()
    }

    fn node_id(&'a self, n: &Nd) -> dot::Id<'a> {
        dot::Id::new(format!("N{}", *n)).unwrap()
    }
}

impl<'a> dot::GraphWalk<'a, Nd, Ed> for Edges {
    fn nodes(&self) -> dot::Nodes<'a, Nd> {
        // (assumes that |N| \approxeq |E|)
        let &Edges(ref v) = self;
        let mut nodes = Vec::with_capacity(v.len());
        for &(s, t) in v {
            nodes.push(s);
            nodes.push(t);
        }
        nodes.sort();
        nodes.dedup();
        Cow::Owned(nodes)
    }

    fn edges(&'a self) -> dot::Edges<'a, Ed> {
        let &Edges(ref edges) = self;
        Cow::Borrowed(&edges[..])
    }

    fn source(&self, e: &Ed) -> Nd {
        e.0
    }

    fn target(&self, e: &Ed) -> Nd {
        e.1
    }
}
