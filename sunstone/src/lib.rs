pub mod bitfield;
pub mod multi;
pub mod ops;
pub mod range;
pub mod step_range;
pub mod stripeset;

use std::{fmt::Debug, ops::Neg};

use num::CheckedAdd;
use num_traits::{FromPrimitive, RefNum, ToPrimitive};

pub trait SetElement:
    RefNum<Self>
    + num_integer::Integer
    + Clone
    + Debug
    + TryFrom<usize>
    + ToPrimitive
    + FromPrimitive
    + Debug
    + CheckedAdd
{
}

impl<
        I: RefNum<I>
            + num_integer::Integer
            + Clone
            + Debug
            + TryFrom<usize>
            + ToPrimitive
            + FromPrimitive
            + Debug
            + CheckedAdd,
    > SetElement for I
{
}
