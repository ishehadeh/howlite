use super::{NodeId, Tree};
use allocator_api2::alloc::{Allocator, Global};
use allocator_api2::vec::Vec;
use std::{cell::UnsafeCell, marker::PhantomData};
/// Tree Builder is a write-only store for AST nodes.
/// Because the tree is write-only, writers do not need mutable access
#[derive(Debug)]
pub struct TreeBuilder<T, A: Allocator = Global> {
    store: UnsafeCell<Vec<T, A>>,
}

impl<T: Clone, A: Allocator + Clone> Clone for TreeBuilder<T, A> {
    fn clone(&self) -> Self {
        Self {
            store: UnsafeCell::new(unsafe { self.store.get().as_ref().unwrap().clone() }),
        }
    }
}

impl<T> Default for TreeBuilder<T> {
    fn default() -> Self {
        Self {
            store: UnsafeCell::new(Vec::new()),
        }
    }
}

impl<T> TreeBuilder<T> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T, A: Allocator> TreeBuilder<T, A> {
    pub fn new_in(alloc: A) -> Self {
        Self {
            store: UnsafeCell::new(Vec::new_in(alloc)),
        }
    }

    /// Create a new, blank tree builder using the backing store from an existing tree
    pub fn rebuild(mut base: Tree<T, A>) -> Self {
        base.tree.clear();
        Self {
            store: UnsafeCell::new(base.tree),
        }
    }

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

    pub fn finalize(self) -> Tree<T, A> {
        Tree {
            tree: self.store.into_inner(),
        }
    }
}
