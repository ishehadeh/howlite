use std::cmp::Ordering;

use super::{num_bigint::BigInt, IntegerRange};

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct IntegerSet {
    ranges: Vec<IntegerRange>,
}

impl IntegerSet {
    pub fn new_from_tuples<RangeT: Into<BigInt> + Clone>(
        ranges: &[(RangeT, RangeT)],
    ) -> IntegerSet {
        Self::new(ranges.iter().map(|x| x.into()))
    }

    pub fn new<T: IntoIterator<Item = IntegerRange>>(ranges: T) -> IntegerSet {
        let mut set = IntegerSet {
            ranges: Vec::from_iter(ranges),
        };
        set.make_normal();
        set
    }

    pub fn empty() -> IntegerSet {
        Default::default()
    }

    pub fn contains(&self, value: &BigInt) -> bool {
        assert!(self.is_normal());
        self.ranges
            .binary_search_by(|r| {
                if value < &r.lo {
                    Ordering::Less
                } else if value > &r.hi {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .is_ok()
    }

    pub fn intersect(&self, other: &IntegerSet) -> IntegerSet {
        assert!(self.is_normal());
        assert!(other.is_normal());

        let mut intersect = IntegerSet::default();
        for r0 in other.ranges.iter() {
            for r1 in self.ranges.iter() {
                if let Some(r_intersect) = r0.intersect(r1) {
                    intersect.ranges.push(r_intersect);
                }
            }
        }
        intersect.make_normal();
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
        assert!(self.is_normal());

        // ranges are sorted, so the first range will always have the lower bound.
        // for details on how ranges are sorted see: [IntegerRange] impl of Ord
        self.ranges.first().map(|lo_range| lo_range.lo.clone())
    }

    pub fn max(&self) -> Option<BigInt> {
        assert!(self.is_normal());

        // because ranges are non-overlapping and sorted in the normal form, the last range will always hold the upper bound.
        self.ranges.last().map(|r| r.hi.clone())
    }
}

// IMPL: sorting and normalization
impl IntegerSet {
    fn is_sorted(&self) -> bool {
        for (i, range) in self.ranges.iter().enumerate().skip(1) {
            if Self::sort_range(&self.ranges[i - 1], range) == Ordering::Greater {
                return false;
            }
        }

        true
    }

    fn has_overlapping_ranges(&self) -> bool {
        self.ranges
            .iter()
            .enumerate()
            .skip(1)
            .any(|(i, range)| self.ranges[i - 1].hi >= range.lo)
    }

    fn merge_overlapping(&mut self) {
        assert!(self.is_sorted());

        // offset needed after applying mutations to the underlying array
        let mut idx_offset = 0;

        for frozen_idx in 1..self.ranges.len() {
            let i = frozen_idx - idx_offset;
            if self.ranges[i - 1].hi >= self.ranges[i].lo {
                idx_offset += 1;
                let lower_range = self.ranges.remove(i - 1);
                self.ranges[frozen_idx - idx_offset].lo = lower_range.lo;
            }
        }
    }

    fn is_normal(&self) -> bool {
        self.is_sorted() && !self.has_overlapping_ranges()
    }

    fn make_normal(&mut self) {
        self.ranges.sort_by(Self::sort_range);
        self.merge_overlapping();
    }

    fn sort_range(a: &IntegerRange, b: &IntegerRange) -> Ordering {
        if a == b {
            Ordering::Equal
        } else if a.is_strict_subrange(b) {
            Ordering::Greater
        } else if b.is_strict_subrange(a) || a.lo < b.lo {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}

// IMPL: exclude values
impl IntegerSet {
    pub fn exclude(&mut self, other: &IntegerSet) -> bool {
        assert!(self.is_normal());
        assert!(other.is_normal());

        let mut has_mutated = false;
        for other_range in &other.ranges {
            has_mutated = self.exclude_range(other_range) || has_mutated
        }

        has_mutated
    }

    /// remove a value from the set.
    /// Return false if the value was not present
    pub fn exclude_value(&mut self, other: &BigInt) -> bool {
        assert!(self.is_normal());

        let maybe_range_idx = self.ranges.binary_search_by(|r| {
            if other < &r.lo {
                Ordering::Less
            } else if other > &r.hi {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        });

        if let Ok(range_idx) = maybe_range_idx {
            let (lower_range, upper_range) = self.ranges[range_idx].split(other);
            let mut iter = lower_range.into_iter().chain(upper_range);
            if let Some(lower_range) = iter.next() {
                self.ranges[range_idx] = lower_range;
            }
            if let Some(higher_range) = iter.next() {
                self.ranges.insert(range_idx + 1, higher_range);
            }

            // this should maintain sort order and non-overlap, but an assert doesnt hurt...
            assert!(self.is_normal());
            true
        } else {
            // set does not contain value
            false
        }
    }

    pub fn exclude_range(&mut self, other: &IntegerRange) -> bool {
        assert!(self.is_normal());

        let mut mutated = false;
        let mut offset = 0isize;
        for original_idx in 0isize..self.ranges.len() as isize {
            let i = (original_idx + offset) as usize;
            if self.ranges[i].contains(&other.lo) || self.ranges[i].contains(&other.hi) {
                let (lower_range, upper_range) =
                    self.ranges[i].split_between(other.lo.clone() - 1, other.hi.clone() + 1);
                let mut iter = lower_range.into_iter().chain(upper_range.into_iter());
                if let Some(lower_range) = iter.next() {
                    self.ranges[i] = lower_range;
                    if let Some(higher_range) = iter.next() {
                        self.ranges.insert(i + 1, higher_range);
                        offset += 1;
                    }
                } else {
                    self.ranges.remove(i);
                    offset -= 1;
                }

                mutated = true;
            }
        }

        assert!(self.is_normal());
        mutated
    }
}

// IMPL: mutate bounds
impl IntegerSet {
    pub fn exclude_below(&mut self, lo: &BigInt) {
        let partial_range_idx = self.ranges.iter().take_while(|r| &r.hi > lo).count();
        self.ranges
            .drain(..(partial_range_idx + 1).min(self.ranges.len() - 1));
        dbg!(&partial_range_idx);
        if !self.ranges.is_empty() {
            self.ranges[0].lo.clone_from(lo);
        }
    }

    pub fn exclude_above(&mut self, hi: &BigInt) {
        let partial_range_idx = self.ranges.iter().rev().take_while(|r| &r.lo > hi).count();
        self.ranges.drain(self.ranges.len() - partial_range_idx..);
        if !self.ranges.is_empty() {
            let last_idx = self.ranges.len() - 1;
            self.ranges[last_idx].hi.clone_from(hi);
        }
    }
}
