use super::{DefaultLinearTreeId, Tree, TreeNodeId};
use allocator_api2::alloc::{Allocator, Global};
use allocator_api2::vec::Vec;
use std::marker::PhantomData;
use std::sync::Mutex;
/// Tree Builder is a write-only store for AST nodes.
/// Because the tree is write-only, writers do not need mutable access
#[derive(Debug)]
pub struct TreeBuilder<
    T,
    IdT: TreeNodeId<Internal = usize> = DefaultLinearTreeId,
    A: Allocator = Global,
> {
    _id_t: PhantomData<IdT>,
    store: Mutex<Vec<T, A>>,
}

impl<T> Default for TreeBuilder<T> {
    fn default() -> Self {
        Self {
            store: Mutex::new(Vec::new()),
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
            store: Mutex::new(Vec::new_in(alloc)),
        }
    }

    /// Create a new, blank tree builder using the backing store from an existing tree
    pub fn rebuild(mut base: Tree<T, IdT, A>) -> Self {
        base.tree.clear();
        Self {
            _id_t: PhantomData,
            store: Mutex::new(base.tree),
        }
    }

    pub fn push(&self, node: T) -> IdT {
        let mut guard = self.store.lock().expect("lock poisoned");
        let index = guard.len();
        guard.push(node);

        unsafe { IdT::mint(index) }
    }

    pub fn finalize(self) -> Tree<T, IdT, A> {
        Tree {
            tree: self.store.into_inner().expect("lock poisoned"),
            _id_t: self._id_t,
        }
    }
}
