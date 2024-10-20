mod read;
mod write;

use std::{hash::Hash, marker::PhantomData};

pub use read::*;
pub use write::*;

#[repr(transparent)]
pub struct NodeId<T> {
    _t: PhantomData<T>,
    index: usize,
}

pub trait TreeNodeId: Clone + Copy + std::fmt::Debug {
    /// The internal type used to identify elements in the tree.
    /// If the backing store for the tree is a Vec, this might be an index.
    /// If the backing store is a map, this could be some UUID or hash-like identifier.
    type Internal;

    /// Create a new instance of the identifier, only the tree implementation should call this method
    unsafe fn mint(internal: Self::Internal) -> Self;

    /// Get the internal identifier type
    fn inner(self) -> Self::Internal;
}

macro_rules! make_tree_id {
    ($($maybe_pub:ident$(($pub_modifier:ident))?)? struct $t:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $($maybe_pub$(($pub_modifier))?)? struct $t(usize);

        impl $crate::tree::TreeNodeId for $t {
            type Internal = usize;

            unsafe fn mint(internal: usize) -> Self {
                Self(internal)
            }

            fn inner(self) -> usize {
                self.0
            }
        }
    };
}

make_tree_id!(pub struct DefaultLinearTreeId);

/// Marker trait for tree ids that wrap an index into a vec
pub trait LinearTreeNodeId: TreeNodeId<Internal = usize> {}

impl<T> LinearTreeNodeId for T where T: TreeNodeId<Internal = usize> {}
