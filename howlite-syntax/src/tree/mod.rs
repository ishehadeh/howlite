mod read;
mod write;

use std::marker::PhantomData;

pub use read::*;
pub use write::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct NodeId<T> {
    _t: PhantomData<T>,
    index: usize,
}
