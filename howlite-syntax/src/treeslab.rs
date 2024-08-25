use std::marker::PhantomData;

pub struct TreeSlab<T: Node> {
    nodes: Vec<T>,
}

pub trait Node {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId<T> {
    _t: PhantomData<T>,
    index: usize,
}

impl<T> std::fmt::Debug for NodeId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[#{}]", self.index)
    }
}

impl<T: Node> TreeSlab<T> {
    pub fn push(&mut self, t: T) -> NodeId<T> {
        let id = NodeId {
            index: self.nodes.len(),
            _t: Default::default(),
        };
        self.nodes.push(t);
        id
    }

    pub fn get(&self, r: NodeId<T>) -> &T {
        &self.nodes[r.index]
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.nodes.iter()
    }
}
