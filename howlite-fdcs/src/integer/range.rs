use std::cmp::Ordering;

use num_bigint::BigInt;

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
        IntegerRange {
            lo: lo.into(),
            hi: hi.into(),
        }
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

    /// Add a value to the beginning and end of the range
    pub fn offset(&self, offset: BigInt) -> IntegerRange {
        IntegerRange::new(self.lo.clone() + offset.clone(), self.hi.clone() + offset)
    }

    pub fn with_lo(self, lo: impl Into<BigInt>) -> IntegerRange {
        IntegerRange::new(lo, self.hi)
    }

    pub fn with_hi(self, hi: impl Into<BigInt>) -> IntegerRange {
        IntegerRange::new(self.lo, hi)
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

impl std::cmp::PartialOrd<IntegerRange> for IntegerRange {
    /// Integer Range less/greater/equal define as so:
    /// ==  <=> lo = lo, hi = hi
    /// <   <=> lo < lo, or if lo = lo, then hi < hi
    /// >   <=> lo > lo, or if lo = lo, then hi > hi    
    fn partial_cmp(&self, other: &IntegerRange) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
