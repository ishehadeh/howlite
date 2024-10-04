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

#[doc(hidden)]
#[macro_export]
macro_rules! _iset_helper {
    (@range [$($args:expr),*,] -> $x:literal .. $y:literal, $($rest:tt)*) => {
        $crate::_iset_helper!(@range [($x, $y), $($args),*] -> $($rest)*)
    };

    (@range [$($args:expr),*,] -> $x:literal, $($rest:tt)*) => {
        $crate::_iset_helper!(@range [($x, $x), $($args),*] -> $($rest)*)
    };

    (@range [$($args:expr),*,] -> $x:literal .. $y:literal) => {
         &[($x, $y), $($args),*]
    };

    (@range [$($args:expr),*,] -> $x:literal) => {
        &[($x, $x), $($args),*]
    };

    (@range [$($args:expr),*,] ->) => {
        &[$($args),*]
    };
}

#[macro_export]
macro_rules! iset {
    ($($args:tt)*) => {
        $crate::IntegerSet::new_from_tuples($crate::_iset_helper!(@range [,] -> $($args)*))
    };

}
