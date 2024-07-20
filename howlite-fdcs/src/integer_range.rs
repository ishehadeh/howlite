use num_bigint::BigInt;

/// An inclusive range of variable-length integers
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerRange {
    /// Smallest value in the range (inclusive)
    pub lo: BigInt,

    /// Largest value in the range (inclusive)
    pub hi: BigInt
}

impl IntegerRange {
    pub fn new<LoT: Into<BigInt>, HiT: Into<BigInt>>(lo: LoT, hi: HiT) -> IntegerRange {
        IntegerRange { lo: lo.into(), hi: hi.into() }
    }

    pub fn intersect(&self, other: &IntegerRange) -> Option<IntegerRange> {
        if other.hi < self.lo || self.hi < other.lo {
            // one range is fully below the other
            None
        } else {
            // otherwise, there must be some overlap, so get the innermost range
            // weird reference-then-clone here avoids copying both range's before compare
            return Some(IntegerRange::new((&other.lo).max(&self.lo).clone(), (&other.hi).min(&self.hi).clone()))
        }
    }
}


impl<LoT: Into<BigInt>, HiT: Into<BigInt>> From<(LoT, HiT)> for IntegerRange {
    fn from((lo, hi): (LoT, HiT)) -> Self {
        return IntegerRange::new(lo, hi)
    }
}