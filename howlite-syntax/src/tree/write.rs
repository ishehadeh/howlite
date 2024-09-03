use super::{NodeId, Tree};
use std::{cell::UnsafeCell, marker::PhantomData};

/// Tree Builder is a write-only store for AST nodes.
/// Because the tree is write-only, writers do not need mutable access
pub struct TreeBuilder<T> {
    store: UnsafeCell<Vec<T>>,
}

impl<T> Default for TreeBuilder<T> {
    fn default() -> Self {
        Self {
            store: UnsafeCell::new(Vec::new()),
        }
    }
}

impl<T> TreeBuilder<T> {
    pub fn push(&self, node: T) -> NodeId<T> {
        unsafe {
            let index = (*self.store.get()).len();
            (*self.store.get()).push(node);

            NodeId {
                _t: PhantomData,
                index,
            }
        }
    }

    pub fn finalize(self) -> Tree<T> {
        Tree {
            tree: self.store.into_inner(),
        }
    }
}
