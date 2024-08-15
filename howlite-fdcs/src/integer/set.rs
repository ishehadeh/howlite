use std::collections::BTreeSet;

use super::{num_bigint::BigInt, IntegerRange};

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct IntegerSet {
    ranges: BTreeSet<IntegerRange>,
}

impl IntegerSet {
    pub fn new<RangeT: Into<BigInt> + Clone>(ranges: &[(RangeT, RangeT)]) -> IntegerSet {
        IntegerSet {
            ranges: ranges.iter().map(IntegerRange::from).collect(),
        }
    }

    pub fn intersect(&self, other: &IntegerSet) -> IntegerSet {
        let mut intersect = IntegerSet::default();
        for r0 in other.ranges.iter() {
            for r1 in self.ranges.iter() {
                if let Some(r_intersect) = r0.intersect(r1) {
                    intersect.ranges.insert(r_intersect);
                }
            }
        }

        intersect
    }
}

// IMPL: min/max related functions
impl IntegerSet {
    /// Get the minimum and maximum values of the set.
    pub fn range(&self) -> Option<IntegerRange> {
        match (self.min(), self.max()) {
            (Some(min), Some(max)) => Some(IntegerRange::new(min, max)),
            _ => None,
        }
    }

    pub fn min(&self) -> Option<BigInt> {
        // btree set maintains sort order.
        // for details on how ranges are sorted see: [IntegerRange] impl of Ord
        self.ranges.first().map(|lo_range| lo_range.lo.clone())
    }

    pub fn max(&self) -> Option<BigInt> {
        // btree set maintains sort order, mostly with respect to the lower value,
        // so we need to iterate to find the high.
        // TODO: find a better sort mechanism for ranges.
        self.ranges
            .iter()
            .rev()
            .map(|hi_range| hi_range.hi.clone())
            .max()
    }
}

// IMPL: exclude values
impl IntegerSet {
    pub fn exclude(&mut self, other: &IntegerSet) -> bool {
        let mut has_mutated = false;
        for other_range in &other.ranges {
            has_mutated = self.exclude_range(other_range) || has_mutated
        }

        has_mutated
    }

    pub fn exclude_range(&mut self, other: &IntegerRange) -> bool {
        let mut additions: Vec<IntegerRange> = Vec::new();
        let mut remove: Vec<IntegerRange> = Vec::new();

        for range in &self.ranges {
            if range.lo < other.lo && range.hi > other.hi {
                // other is inside this range, we need to split the range.
                let lo_range = IntegerRange::new(range.lo.clone(), other.lo.clone() - 1);
                let hi_range = IntegerRange::new(other.hi.clone() + 1, range.hi.clone());
                remove.push(range.clone());
                additions.push(lo_range);
                additions.push(hi_range);
            } else if range.lo >= other.lo && range.hi <= other.hi {
                // range completely contained in other
                remove.push(range.clone());
            } else if range.lo >= other.lo && range.hi > other.hi {
                // partial - high
                additions.push(IntegerRange::new(other.hi.clone() + 1, range.hi.clone()));
                remove.push(range.clone())
            } else if range.lo < other.lo && range.hi <= other.hi {
                // partial - low
                additions.push(IntegerRange::new(range.lo.clone(), other.lo.clone() - 1));
                remove.push(range.clone())
            }
        }

        let has_mutated = !additions.is_empty() || !remove.is_empty();
        for r in remove {
            self.ranges.remove(&r);
        }
        for a in additions {
            self.ranges.insert(a);
        }

        has_mutated
    }
}

// IMPL: mutate bounds
impl IntegerSet {
    pub fn exclude_below(&mut self, lo: &BigInt) {
        let too_low: Vec<_> = self
            .ranges
            .iter()
            .take_while(|range| range.lo < *lo)
            .cloned()
            .collect();
        for mut range in too_low {
            self.ranges.remove(&range);
            if range.hi >= *lo {
                range.lo.clone_from(lo);
                self.ranges.insert(range);
            }
        }
    }

    pub fn exclude_above(&mut self, hi: &BigInt) {
        // NOTE: do to sort order mostly being by low,
        //  we can't use .rev().take_while() here.
        let too_high: Vec<_> = self
            .ranges
            .iter()
            .filter(|range| range.hi > *hi)
            .cloned()
            .collect();
        for mut range in too_high {
            self.ranges.remove(&range);
            if range.lo <= *hi {
                range.hi.clone_from(hi);
                self.ranges.insert(range);
            }
        }
    }
}
