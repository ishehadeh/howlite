use std::{marker::PhantomData, mem::MaybeUninit};

use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};

use super::{DefaultLinearTreeId, TreeNodeId};

#[derive(Debug, Clone)]
pub struct Tree<T, IdT: TreeNodeId<Internal = usize> = DefaultLinearTreeId, A: Allocator = Global> {
    pub(crate) _id_t: PhantomData<IdT>,
    pub(crate) tree: Vec<T, A>,
}

impl<T, IdT: TreeNodeId<Internal = usize>, A: Allocator> Tree<T, IdT, A> {
    pub fn get(&self, node: IdT) -> &T {
        &self.tree[node.inner()]
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.tree.iter()
    }

    pub fn node_count(&self) -> usize {
        self.tree.len()
    }

    pub fn get_many<const N: usize>(&self, ids: [IdT; N]) -> [&T; N] {
        #[allow(
            clippy::uninit_assumed_init,
            reason = "since N is const we can be sure each of these is initialized"
        )]
        let mut node_refs: [&T; N] = unsafe { MaybeUninit::zeroed().assume_init() };
        for i in 0..N {
            node_refs[i] = &self.tree[ids[i].inner()];
        }

        node_refs
    }
}
