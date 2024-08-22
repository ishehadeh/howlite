///! This module implements a DAG, with the dynamic array as the backing storage.

pub struct Tree<T: Node> {
    nodes: Vec<T>,
    edges: Vec<(usize, usize)>,
}

pub trait EdgeMarker {}

pub trait Node {
    type EdgeT: EdgeMarker;
}

pub trait INodeRef {
    type EdgeT: EdgeMarker;
}

pub struct NodeRef {
    index: usize,
}

impl NodeRef<T> {}

impl<T: Node> Tree<T> {
    pub fn add<A: Node>(t: A) -> NodeRef {}
}

// TODO: implement DAG
// TODO: implement safe references
