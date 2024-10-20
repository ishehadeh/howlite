use super::{DefaultLinearTreeId, Tree, TreeNodeId};
use allocator_api2::alloc::{Allocator, Global};
use allocator_api2::vec::Vec;
use std::{cell::UnsafeCell, marker::PhantomData};
/// Tree Builder is a write-only store for AST nodes.
/// Because the tree is write-only, writers do not need mutable access
#[derive(Debug)]
pub struct TreeBuilder<
    T,
    IdT: TreeNodeId<Internal = usize> = DefaultLinearTreeId,
    A: Allocator = Global,
> {
    _id_t: PhantomData<IdT>,
    store: UnsafeCell<Vec<T, A>>,
}

impl<T: Clone, IdT: TreeNodeId<Internal = usize>, A: Allocator + Clone> Clone
    for TreeBuilder<T, IdT, A>
{
    fn clone(&self) -> Self {
        Self {
            _id_t: PhantomData,
            store: UnsafeCell::new(unsafe { self.store.get().as_ref().unwrap().clone() }),
        }
    }
}

impl<T> Default for TreeBuilder<T> {
    fn default() -> Self {
        Self {
            store: UnsafeCell::new(Vec::new()),
            _id_t: PhantomData,
        }
    }
}

impl<T> TreeBuilder<T> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T, IdT: TreeNodeId<Internal = usize>, A: Allocator> TreeBuilder<T, IdT, A> {
    pub fn new_in(alloc: A) -> Self {
        Self {
            _id_t: PhantomData,
            store: UnsafeCell::new(Vec::new_in(alloc)),
        }
    }

    /// Create a new, blank tree builder using the backing store from an existing tree
    pub fn rebuild(mut base: Tree<T, IdT, A>) -> Self {
        base.tree.clear();
        Self {
            _id_t: PhantomData,
            store: UnsafeCell::new(base.tree),
        }
    }

    pub fn push(&self, node: T) -> IdT {
        unsafe {
            let index = (*self.store.get()).len();
            (*self.store.get()).push(node);

            IdT::mint(index)
        }
    }

    pub fn finalize(self) -> Tree<T, IdT, A> {
        Tree {
            tree: self.store.into_inner(),
            _id_t: self._id_t,
        }
    }
}
