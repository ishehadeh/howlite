use std::mem::MaybeUninit;

use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};

use super::NodeId;

#[derive(Debug, Clone)]
pub struct Tree<T, A: Allocator = Global> {
    pub(crate) tree: Vec<T, A>,
}

impl<T: PartialEq, A: Allocator> PartialEq for Tree<T, A> {
    fn eq(&self, other: &Self) -> bool {
        if other.node_count() != self.node_count() {
            false
        } else {
            for (a, b) in self.iter().zip(other.iter()) {
                if !a.eq(b) {
                    return false;
                }
            }
            true
        }
    }
}

impl<T, A: Allocator> Tree<T, A> {
    pub fn get(&self, node: NodeId<T>) -> &T {
        &self.tree[node.index]
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.tree.iter()
    }

    pub fn node_count(&self) -> usize {
        self.tree.len()
    }

    pub fn get_many<const N: usize>(&self, ids: [NodeId<T>; N]) -> [&T; N] {
        #[allow(
            clippy::uninit_assumed_init,
            reason = "since N is const we can be sure each of these is initialized"
        )]
        let mut node_refs: [&T; N] = unsafe { MaybeUninit::uninit().assume_init() };
        for i in 0..N {
            node_refs[i] = &self.tree[ids[i].index];
        }

        node_refs
    }
}
