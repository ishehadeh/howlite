mod assoc;
mod read;
mod write;

use std::{hash::Hash, marker::PhantomData};

pub use assoc::{AssociatedTree, AssociatedTreeBuildContext, PartialAssociatedTree};
pub use read::*;
pub use write::*;

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct NodeId<T> {
    _t: PhantomData<T>,
    index: usize,
}

impl<T> Hash for NodeId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<T> std::fmt::Debug for NodeId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "NodeId<{}>({:#08x})",
            std::any::type_name::<T>(),
            self.index,
        )
    }
}

impl<T> Eq for NodeId<T> {}
impl<T> PartialEq for NodeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> PartialOrd for NodeId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T> Ord for NodeId<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}
