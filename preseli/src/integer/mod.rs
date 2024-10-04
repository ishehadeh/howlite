pub use num_bigint;
use sunstone::{
    multi::DynSet,
    ops::{Bounded, PartialBounded},
};

use crate::Mutation;
// mod range;
// mod set;
// pub use range::*;
// pub use set::IntegerSet;
pub type IntegerSet = sunstone::multi::DynSet<num_bigint::BigInt>;
pub type IntegerRange = sunstone::range::Range<num_bigint::BigInt>;

pub(crate) fn shift_lo_mutation(
    range: &impl PartialBounded<num_bigint::BigInt>,
    upward_shift: num_bigint::BigInt,
) -> Option<Mutation> {
    if let Some(range) = range.partial_bounds() {
        if range.len() < upward_shift {
            Some(Mutation::Instantiate {
                value: DynSet::empty(),
            })
        } else {
            Some(Mutation::BoundLo {
                lo: upward_shift + *range.lo(),
            })
        }
    } else {
        None
    }
}

pub(crate) fn shift_hi_mutation(
    range: &impl PartialBounded<num_bigint::BigInt>,
    downward_shift: num_bigint::BigInt,
) -> Option<Mutation> {
    if let Some(range) = range.partial_bounds() {
        if range.len() < downward_shift {
            Some(Mutation::Instantiate {
                value: DynSet::empty(),
            })
        } else {
            Some(Mutation::BoundHi {
                hi: *range.hi() - downward_shift,
            })
        }
    } else {
        None
    }
}
