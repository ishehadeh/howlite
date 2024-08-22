use crate::idvec::{Id, IdVec};
use slab::Slab;

pub type Ast = ();

pub type SyntaxNodeRef {
    slab_ref: usize
}

#[derive(Clone, Debug, Default)]
pub struct SyntaxTreeBuffer {
    slab: Slab<Ast>,
}

impl SyntaxTreeBuffer {
    pub fn new() -> SyntaxTreeBuffer {
        SyntaxTreeBuffer::default()
    }

    pub fn add(&mut self, node: NodeT) -> SyntaxNodeRef {
        self.nodes.push(node.into())
    }

    pub fn get(&self, node: SyntaxNodeRef) -> &Ast {
        self.nodes.get(node)
    }

    pub fn with_root(self, root: SyntaxNodeRef) -> SyntaxTree {
        SyntaxTree {
            nodes: self.nodes,
            root,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxTree {
    nodes: IdVec<Ast>,
    root: SyntaxNodeRef,
}

impl SyntaxTree {
    pub fn root(&self) -> SyntaxNodeRef {
        self.root.clone()
    }

    // iterate through the tree in insertion order (i.e. bottom up)
    pub fn iter(&self) -> impl Iterator<Item = (SyntaxNodeRef, &Ast)> {
        self.nodes.iter()
    }

    pub fn get(&self, node: SyntaxNodeRef) -> &Ast {
        self.nodes.get(node)
    }

    pub fn map<T, F>(&self, f: F) -> SyntaxData<T>
    where
        F: Fn(&SyntaxData<T>, SyntaxNodeRef, &Ast) -> T,
    {
        let mut data = SyntaxData {
            data: Vec::with_capacity(self.nodes.len()),
            token: self.nodes.token(),
        };

        for (r, node) in self.iter() {
            let val = f(&data, r, node);
            data.data.push(val);
        }

        data
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxData<T> {
    data: Vec<T>,
    token: IdVecToken<Ast>,
}

impl<T> SyntaxData<T> {
    pub fn get(&self, node: SyntaxNodeRef) -> &T {
        assert!(self.token.owns(node.clone()));
        &self.data[node.inner()]
    }
}
