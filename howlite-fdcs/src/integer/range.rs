use std::{
    cmp::Ordering,
    ops::{Add, Index, Mul},
};

use num_bigint::BigInt;

use crate::Mutation;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RangeSide {
    Lo,
    Hi,
}

/// An inclusive range of variable-length integers
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerRange {
    /// Smallest value in the range (inclusive)
    pub lo: BigInt,

    /// Largest value in the range (inclusive)
    pub hi: BigInt,
}

impl IntegerRange {
    pub fn new<LoT: Into<BigInt>, HiT: Into<BigInt>>(lo: LoT, hi: HiT) -> IntegerRange {
        let lo_big = lo.into();
        let hi_big = hi.into();
        Self::try_new(lo_big.clone(), hi_big.clone()).unwrap_or_else(|| {
            panic!(
                "IntegerRange::new(): invalid range, lo is larger than hi. lo={}, hi={})",
                lo_big, &hi_big
            )
        })
    }

    /// Like IntegerRange::new, but return None if lo > hi instead of `panic!`-ing.
    pub fn try_new<LoT: Into<BigInt>, HiT: Into<BigInt>>(lo: LoT, hi: HiT) -> Option<IntegerRange> {
        let lo_big = lo.into();
        let hi_big = hi.into();
        if lo_big > hi_big {
            None
        } else {
            Some(IntegerRange {
                lo: lo_big,
                hi: hi_big,
            })
        }
    }

    pub fn contains(&self, value: &BigInt) -> bool {
        value >= &self.lo && value <= &self.hi
    }

    pub fn intersect(&self, other: &IntegerRange) -> Option<IntegerRange> {
        if other.hi < self.lo || self.hi < other.lo {
            // one range is fully below the other
            None
        } else {
            // otherwise, there must be some overlap, so get the innermost range
            // weird reference-then-clone here avoids copying both range's before compare
            Some(IntegerRange::new(
                (&other.lo).max(&self.lo).clone(),
                (&other.hi).min(&self.hi).clone(),
            ))
        }
    }

    pub fn split(&self, value: &BigInt) -> (Option<IntegerRange>, Option<IntegerRange>) {
        if value < &self.lo || value > &self.hi {
            (None, None)
        } else {
            (
                IntegerRange::try_new(self.lo.clone(), value - 1),
                IntegerRange::try_new(value + 1, self.hi.clone()),
            )
        }
    }

    pub fn is_strict_subrange(&self, subrange: &IntegerRange) -> bool {
        self.lo < subrange.lo && self.hi > subrange.hi
    }

    /// split the range in two, creating two new ranges [self.lo, lo] and [hi, self.hi]
    pub fn split_between(
        &self,
        lo: BigInt,
        hi: BigInt,
    ) -> (Option<IntegerRange>, Option<IntegerRange>) {
        if lo < self.lo && hi > self.hi {
            (None, None)
        } else {
            (
                IntegerRange::try_new(self.lo.clone(), lo),
                IntegerRange::try_new(hi, self.hi.clone()),
            )
        }
    }

    pub fn size(&self) -> BigInt {
        &self.hi - &self.lo
    }

    pub fn with_lo(self, lo: impl Into<BigInt>) -> IntegerRange {
        IntegerRange::new(lo, self.hi)
    }

    pub fn with_hi(self, hi: impl Into<BigInt>) -> IntegerRange {
        IntegerRange::new(self.lo, hi)
    }

    pub fn invert(self) -> IntegerRange {
        IntegerRange::new(-self.hi, -self.lo)
    }
}

impl<LoT: Into<BigInt>, HiT: Into<BigInt>> From<(LoT, HiT)> for IntegerRange {
    fn from((lo, hi): (LoT, HiT)) -> Self {
        IntegerRange::new(lo, hi)
    }
}

impl<LoT: Into<BigInt> + Clone, HiT: Into<BigInt> + Clone> From<&(LoT, HiT)> for IntegerRange {
    fn from((lo, hi): &(LoT, HiT)) -> Self {
        IntegerRange::new(lo.clone(), hi.clone())
    }
}

impl Mul<&BigInt> for IntegerRange {
    type Output = IntegerRange;

    fn mul(mut self, rhs: &BigInt) -> Self::Output {
        self.lo *= rhs;
        self.hi *= rhs;
        if self.hi < self.lo {
            std::mem::swap(&mut self.lo, &mut self.hi);
        }
        self
    }
}

impl Add<IntegerRange> for IntegerRange {
    type Output = IntegerRange;

    fn add(mut self, rhs: IntegerRange) -> Self::Output {
        self.lo += &rhs.lo;
        self.hi += &rhs.hi;
        self
    }
}

impl std::cmp::Ord for IntegerRange {
    /// Integer Range less/greater/equal define as so:
    /// ==  <=> lo = lo, hi = hi
    /// <   <=> lo < lo, or if lo = lo, then hi < hi
    /// >   <=> lo > lo, or if lo = lo, then hi > hi
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.lo.cmp(&other.lo), self.hi.cmp(&other.hi)) {
            (Ordering::Equal, hi_order) => hi_order,
            (lo_order, _) => lo_order,
        }
    }
}

impl Index<RangeSide> for IntegerRange {
    type Output = BigInt;

    fn index(&self, index: RangeSide) -> &Self::Output {
        match index {
            RangeSide::Lo => &self.lo,
            RangeSide::Hi => &self.hi,
        }
    }
}

impl std::cmp::PartialOrd<IntegerRange> for IntegerRange {
    /// Integer Range less/greater/equal define as so:
    /// ==  <=> lo = lo, hi = hi
    /// <   <=> lo < lo, or if lo = lo, then hi < hi
    /// >   <=> lo > lo, or if lo = lo, then hi > hi    
    fn partial_cmp(&self, other: &IntegerRange) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Lo;
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Hi;

pub const HI: Hi = Hi {};
pub const LO: Lo = Lo {};

pub trait BoundMarker: Clone + Copy + PartialEq + Eq {
    type Inverse: BoundMarker;
    const IS_LO: bool;
    const INVERSE: Self::Inverse;
}

impl<T: BoundMarker> Index<T> for IntegerRange {
    type Output = BigInt;

    #[inline(always)]
    fn index(&self, _: T) -> &Self::Output {
        if T::IS_LO {
            &self.lo
        } else {
            &self.hi
        }
    }
}


impl BoundMarker for Lo {
    const IS_LO: bool = true;
    const INVERSE: Self::Inverse = HI;
    type Inverse = Hi;
}

impl BoundMarker for Hi {
    const IS_LO: bool = false;
    const INVERSE: Self::Inverse = LO;
    type Inverse = Lo;
}

impl IntegerRange {
    pub fn outward_shift_mutation<B: BoundMarker>(&self, _: B, shift: &BigInt) -> Mutation {
        if B::IS_LO {
            Mutation::BoundLo { lo: &self.lo - shift }
        } else {
            Mutation::BoundHi { hi: &self.hi + shift }
        }
    }
}
