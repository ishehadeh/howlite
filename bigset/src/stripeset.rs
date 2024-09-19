use std::{cmp::Ordering, marker::PhantomData, ops::RemAssign};

use bumpalo::Bump;
use num_integer::Integer;

use crate::{
    ops::{Bounded, Set, Subset, Union},
    range::Range,
    step_range::{RangeValue, StepRange},
};

#[derive(Clone, Debug)]
pub struct StripeSet<I: RangeValue> {
    ranges: Vec<StepRange<I>>,
}

impl<T2, I> PartialEq<T2> for StripeSet<I>
where
    I: RangeValue,
    for<'a> &'a StripeSet<I>: Subset<&'a T2>,
    for<'b> &'b T2: Subset<&'b StripeSet<I>>,
{
    fn eq(&self, other: &T2) -> bool {
        self.subset_of(other) && other.subset_of(self)
    }
}

pub struct StripeSetBoundedIter<'a, I, B, I2>
where
    I: RangeValue + PartialOrd<I2>,
    B: Bounded<I2>,
    I2: PartialOrd<I> + Eq,
{
    _i2: PhantomData<I2>,
    bounds: B,
    index: usize,
    set: &'a StripeSet<I>,

    /// true to return all overlapping ranges, even if not a subset of `bounds``
    partial: bool,
}

impl<'a, I, B, I2> Iterator for StripeSetBoundedIter<'a, I, B, I2>
where
    I: RangeValue + PartialOrd<I2>,
    B: Bounded<I2>,
    I2: PartialOrd<I> + Eq,
{
    type Item = &'a StepRange<I>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.set.ranges.len() {
            return None;
        }

        let curr = &self.set.ranges[self.index];
        if curr.hi() <= self.bounds.lo() {
            return None;
        }
        if curr.hi() < self.bounds.hi() && !self.partial {
            return None;
        }

        self.index += 1;
        // curr completely within bounds

        if curr.lo() >= self.bounds.lo() && curr.hi() >= self.bounds.hi() {
            return Some(curr);
        }

        // curr partially within bounds
        if self.partial && curr.lo() <= self.bounds.hi() {
            return Some(curr);
        }

        self.next()
    }
}

impl<I> StripeSet<I>
where
    I: RangeValue,
{
    pub fn new(ranges: Vec<StepRange<I>>) -> Self {
        let mut a = Self { ranges };
        a.normalize();
        a
    }

    pub fn iter_within<B, I2>(&self, bounds: B) -> StripeSetBoundedIter<'_, I, B, I2>
    where
        I: RangeValue + PartialOrd<I2>,
        B: Bounded<I2>,
        I2: PartialOrd<I> + Eq,
    {
        StripeSetBoundedIter {
            bounds,
            index: 0,
            set: self,
            partial: false,
            _i2: PhantomData,
        }
    }

    pub fn iter_within_partial<B, I2>(&self, bounds: B) -> StripeSetBoundedIter<'_, I, B, I2>
    where
        I: RangeValue + PartialOrd<I2>,
        B: Bounded<I2>,
        I2: PartialOrd<I> + Eq,
    {
        StripeSetBoundedIter {
            bounds,
            index: 0,
            set: self,
            partial: true,
            _i2: PhantomData,
        }
    }

    fn sort(&mut self) {
        self.ranges.sort_by(Self::cmp_range)
    }

    pub fn add_range(&mut self, r: StepRange<I>) {
        let mut remaining = Some(r);
        let mut insertion_point = 0;
        for (i, el) in self.ranges.iter().enumerate() {
            if let Some(new_el) = &remaining {
                if el.hi() < new_el.lo() {
                    // we assume the ranges are sorted, so non overlapping ranges are sorted by bounds
                    insertion_point = i + 1;
                    continue;
                } else if el.lo() > new_el.hi() {
                    insertion_point = i + 1;
                    if el.step() <= new_el.step() {
                        break;
                    }
                    continue;
                }
                if el.step() > new_el.step() {
                    insertion_point = i + 1;

                    continue;
                }

                insertion_point = i;
            } else {
                break;
            }
            remaining = remaining.and_then(|a| a.compactify(el));
        }
        if let Some(new_el) = remaining {
            self.ranges.insert(insertion_point, new_el);
        }
    }

    pub fn normalize(&mut self) {
        self.sort();
        // self.consolidate();
    }

    fn cmp_range(a: &StepRange<I>, b: &StepRange<I>) -> Ordering {
        // A |------------|
        // B                       |------------|
        if a.hi() < b.lo() {
            // non-overlapping ranges are sorted in terms of their bounds
            Ordering::Less
        } else if a.lo() > b.hi() {
            Ordering::Greater
        } else {
            // otherwise sort by step
            b.step().cmp(a.step())
        }
    }
}

impl<I> Set<I> for StripeSet<I>
where
    I: RangeValue,
{
    fn includes(&self, element: I) -> bool {
        for range in &self.ranges {
            if range.hi() < &element {
                break;
            }
            if range.includes(&element) {
                return true;
            }
        }

        false
    }
}

impl<'a, I> Subset<&'a StripeSet<I>> for &'a StripeSet<I>
where
    I: RangeValue + std::fmt::Debug,
{
    fn subset_of(self, rhs: Self) -> bool {
        for range in &self.ranges {
            let mut start = range.lo().clone();
            for other in rhs
                .iter_within_partial(range)
                .filter(|x| range.step().is_multiple_of(x.step()))
            {
                if other.lo() > &start {
                    // some part of our range was skipped, so this isn't a subset.
                    return false;
                }

                start = other.hi().clone() + range.step().clone();
            }
            if &start < range.hi() {
                return false;
            }
        }
        true
    }

    fn strict_subset_of(self, _: Self) -> bool {
        todo!()
    }
}

impl<I> Union<StripeSet<I>> for StripeSet<I>
where
    I: RangeValue,
{
    type Output = StripeSet<I>;

    fn union(self, rhs: StripeSet<I>) -> Self::Output {
        StripeSet::new([self.ranges, rhs.ranges].concat())
    }
}

#[test]
fn simple() {
    let a = StripeSet::new(vec![StepRange::new(0, 10, 2), StepRange::new(15, 20, 1)]);
    let b = StripeSet::new(vec![StepRange::new(0, 18, 6)]);
    assert!(b.subset_of(&a));
    assert!(!a.subset_of(&b));
}

#[test]
fn insert() {
    let mut a = StripeSet::new(vec![StepRange::new(0, 10, 2), StepRange::new(15, 20, 1)]);
    a.add_range(StepRange::new(10, 18, 2));
    assert_eq!(
        a.ranges,
        vec![
            StepRange::new(0, 10, 2),
            StepRange::new(12, 14, 2),
            StepRange::new(15, 20, 1)
        ]
    );

    let mut a = StripeSet::new(vec![]);

    a.add_range(StepRange::new(10, 18, 2));
    a.add_range(StepRange::new(0, 10, 2));
    a.add_range(StepRange::new(15, 20, 1));
    assert_eq!(
        a.ranges,
        vec![
            StepRange::new(0, 8, 2),
            StepRange::new(10, 18, 2),
            StepRange::new(15, 20, 1)
        ]
    );
}

#[test]
fn union() {
    let a = StripeSet::new(vec![StepRange::new(0, 5, 1), StepRange::new(10, 20, 2)]);
    let b = StripeSet::new(vec![StepRange::new(0, 18, 6)]);
    let c = a.clone().union(b.clone());
    dbg!(&c);
    assert!(a.subset_of(&c));
    assert!(b.subset_of(&c));
}
