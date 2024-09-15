use std::{cmp::Ordering, marker::PhantomData};

use num_integer::Integer;

use crate::{
    ops::{Bounded, Set, Subset},
    step_range::StepRange,
};

pub struct StripeSet<I: Integer + Clone> {
    ranges: Vec<StepRange<I>>,
}

pub struct StripeSetBoundedIter<'a, I, B, I2>
where
    I: Integer + Clone + PartialOrd<I2>,
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
    I: Integer + Clone + PartialOrd<I2>,
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

        return self.next();
    }
}

impl<I> StripeSet<I>
where
    I: Integer + Clone,
{
    pub fn new(ranges: Vec<StepRange<I>>) -> Self {
        let mut a = Self { ranges };
        a.normalize();
        a
    }

    pub fn iter_within<B, I2>(&self, bounds: B) -> StripeSetBoundedIter<'_, I, B, I2>
    where
        I: Integer + Clone + PartialOrd<I2>,
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
        I: Integer + Clone + PartialOrd<I2>,
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

    fn consolidate(&mut self) {
        let mut i = 0;
        while i < self.ranges.len() - 1 {
            if self.ranges[i].hi() > self.ranges[i + 1].lo() {
                if self.ranges[i]
                    .step()
                    .is_multiple_of(self.ranges[i + 1].step())
                {
                    self.ranges[i + 1] = self.ranges[i + 1]
                        .clone()
                        .with_lo(self.ranges[i].lo().clone());
                } else if self.ranges[i + 1]
                    .step()
                    .is_multiple_of(self.ranges[i].step())
                {
                    self.ranges[i] = self.ranges[i]
                        .clone()
                        .with_hi(self.ranges[i + 1].lo().clone());
                }
            } else if self.ranges[i].subset_of(&self.ranges[i + 1]) {
                self.ranges.remove(i);
            } else {
                i += 1;
            }
        }
    }

    pub fn normalize(&mut self) {
        self.sort();
        self.consolidate();
    }

    fn cmp_range(a: &StepRange<I>, b: &StepRange<I>) -> Ordering {
        match (
            a.lo().cmp(b.lo()),
            a.hi().cmp(b.hi()),
            a.step().cmp(b.step()),
        ) {
            // A |------------|
            // B        |------------|
            // OR
            // A |------------|
            // B                       |------------|
            (Ordering::Less, Ordering::Less, _) => Ordering::Less,

            // A         |------------|
            // B |------------|
            // OR
            // A                      |------------|
            // B |------------|
            (Ordering::Greater, Ordering::Greater, _) => Ordering::Greater,

            // SUPERSETS: supersets are always sorted before subsets
            //  this forms a tree like structure.
            //  during iteration, if  N - 1 does not contain N then N > N - 1
            // -----------------------

            // A |--------|
            // B |------------|
            // OR
            // A    |------|
            // B |------------|
            // OR
            // A      |-------|
            // B |------------|
            (Ordering::Equal, Ordering::Less, _) => Ordering::Greater,
            (Ordering::Greater, Ordering::Less, _) => Ordering::Greater,
            (Ordering::Greater, Ordering::Equal, _) => Ordering::Greater,

            // A |------------|
            // B |--------|
            // OR
            // A |------------|
            // B    |------|
            // OR
            // A |------------|
            // B      |-------|
            (Ordering::Equal, Ordering::Greater, _) => Ordering::Less,
            (Ordering::Less, Ordering::Greater, _) => Ordering::Less,
            (Ordering::Less, Ordering::Equal, _) => Ordering::Less,

            // if hi/lo match, sort by step
            // lower steps go AFTER higher steps, to maintain subsetset ordering mentioned above
            (Ordering::Equal, Ordering::Equal, Ordering::Less) => Ordering::Greater,
            (Ordering::Equal, Ordering::Equal, Ordering::Greater) => Ordering::Less,
            (Ordering::Equal, Ordering::Equal, Ordering::Equal) => Ordering::Equal,
        }
    }
}

impl<I> Set<I> for StripeSet<I>
where
    I: Integer + Clone,
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
    I: Integer + Clone + std::fmt::Debug,
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

#[test]
fn simple() {
    let a = StripeSet::new(vec![StepRange::new(0, 10, 2), StepRange::new(15, 20, 1)]);
    let b = StripeSet::new(vec![StepRange::new(0, 18, 6)]);
    assert!(b.subset_of(&a));
    assert!(!a.subset_of(&b));
}
