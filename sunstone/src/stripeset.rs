use std::{cmp::Ordering, marker::PhantomData};

use crate::{
    ops::{Bounded, Set, SetMut, Subset, Union},
    step_range::{RangeValue, StepRange},
    SetElement,
};

#[derive(Clone, Debug)]
pub struct StripeSet<I: SetElement> {
    ranges: Vec<StepRange<I>>,
}

impl<T2, I> PartialEq<T2> for StripeSet<I>
where
    I: SetElement,
    for<'a> &'a StripeSet<I>: Subset<&'a T2>,
    for<'b> &'b T2: Subset<&'b StripeSet<I>>,
{
    fn eq(&self, other: &T2) -> bool {
        self.subset_of(other) && other.subset_of(self)
    }
}

pub struct StripeSetBoundedIter<'a, I, B, I2>
where
    I: SetElement + PartialOrd<I2>,
    B: Bounded<I2>,
    I2: PartialOrd<I> + Ord,
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
    I: SetElement + PartialOrd<I2>,
    B: Bounded<I2>,
    I2: PartialOrd<I> + Ord,
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
    I: SetElement,
{
    pub fn new(ranges: Vec<StepRange<I>>) -> Self {
        let mut a = Self { ranges };
        a.normalize();
        a
    }

    pub fn stripes(&self) -> impl Iterator<Item = &'_ StepRange<I>> {
        self.ranges.iter()
    }
    pub fn arith_add(&self, other: &StripeSet<I>) -> Self {
        let mut new = StripeSet::new(vec![]);
        for a in &self.ranges {
            for other_range in &other.ranges {
                if &other_range.size() >= a.step() && other_range.step().is_one() {
                    new.add_range(StepRange::new(
                        a.lo().clone() + other_range.lo().clone(),
                        a.hi().clone() + other_range.hi().clone(),
                        I::one(),
                    ))
                } else {
                    // range  that we have to expand
                    let (explode_range, base_range) = if a.size() < other_range.size() {
                        (a, other_range)
                    } else {
                        (other_range, a)
                    };
                    for el in explode_range {
                        new.add_range(StepRange::new(
                            base_range.lo().clone() + el.clone(),
                            base_range.hi().clone() + el,
                            base_range.step().clone(),
                        ))
                    }
                }
            }
        }

        new
    }

    pub fn iter_within<B, I2>(&self, bounds: B) -> StripeSetBoundedIter<'_, I, B, I2>
    where
        I: RangeValue + PartialOrd<I2>,
        B: Bounded<I2>,
        I2: PartialOrd<I> + Ord,
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
        I2: PartialOrd<I> + Ord,
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

    pub fn compact_all(&mut self) {
        let mut i = 0;
        let mut j = 0;
        while i < self.ranges.len() {
            j += self
                .ranges
                .iter()
                .skip(j)
                .take_while(|r| {
                    r.step() <= self.ranges[i].step()
                        && !(r.lo() > self.ranges[i].hi() || r.hi() < self.ranges[i].lo())
                })
                .count();
            let mut index_to_compact = i;
            for n in (i + 1)..j {
                {
                    let (i_slice, j_slice) = self.ranges.split_at_mut(n);
                    i_slice[index_to_compact].compactify_mut(&j_slice[0]);
                }
                if self.ranges[index_to_compact].lo() > self.ranges[n].hi() {
                    self.ranges.swap(i, n);
                    index_to_compact = n;
                }
            }
            i += 1;
        }
    }

    pub fn merge_all(&mut self) {
        let mut i = 0;
        while i < self.ranges.len() {
            let mut should_incr = true;
            for j in (i + 1)..self.ranges.len() {
                // TODO: size == 1 too!
                if self.ranges[i].hi().clone() + self.ranges[i].step().clone()
                    == self.ranges[j].lo().clone()
                    && self.ranges[i].step() == self.ranges[j].step()
                {
                    let (_, new_hi, _) = self.ranges.remove(j).deconstruct();
                    self.ranges[i].set_hi(new_hi);
                    should_incr = false;
                    break;
                }

                if self.ranges[i] == self.ranges[j] {
                    self.ranges.remove(j);
                    should_incr = false;
                    break;
                }
            }
            if should_incr {
                i += 1;
            }
        }
    }

    pub fn add_range(&mut self, r: StepRange<I>) {
        self.ranges.push(r);
        self.normalize();
    }

    pub fn normalize(&mut self) {
        self.sort();
        self.compact_all();
        self.merge_all();
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
            match b.step().cmp(a.step()) {
                Ordering::Equal => a.lo().cmp(b.lo()),
                lt_gt => lt_gt,
            }
        }
    }

    fn get_containing_range_index(&self, element: &I) -> Option<usize> {
        self.ranges
            .iter()
            .enumerate()
            .find(|(_, r)| r.includes(element))
            .map(|(i, _)| i)
    }
}

impl<I> Set<I> for StripeSet<I>
where
    I: SetElement,
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
    I: SetElement + std::fmt::Debug,
{
    fn subset_of(self, rhs: Self) -> bool {
        for r in &self.ranges {
            let mut lo = r.lo().clone();
            let mut excl = vec![];
            for s in &rhs.ranges {
                if s.includes(&lo) {
                    if r.step().is_multiple_of(s.step()) {
                        lo = r.first_element_after(s.hi().clone());
                    } else if s.step().is_multiple_of(r.step()) {
                        excl.push(StepRange::new(lo.clone(), s.hi().clone(), s.step().clone()));
                    } else {
                        lo = lo + r.step().clone()
                    }
                    // dbg!(&r, &s, &lo);
                }
            }

            if &lo < r.hi() {
                if !excl.is_empty() {
                    'a: while &lo < r.hi() {
                        for r in &excl {
                            if r.includes(&lo) {
                                lo = lo + r.step().clone();
                                continue 'a;
                            }
                        }
                        dbg!("1");
                        return false;
                    }

                    dbg!("4");
                    continue;
                }

                dbg!("2", &lo, r.hi(), &lo < r.hi());
                return false;
            }
        }

        dbg!("3");
        true
    }

    fn strict_subset_of(self, _: Self) -> bool {
        todo!()
    }
}

impl<I> Union<StripeSet<I>> for StripeSet<I>
where
    I: SetElement,
{
    type Output = StripeSet<I>;

    fn union(self, rhs: StripeSet<I>) -> Self::Output {
        StripeSet::new([self.ranges, rhs.ranges].concat())
    }
}

impl<I> SetMut<I> for StripeSet<I>
where
    I: SetElement,
{
    type Output = StripeSet<I>;

    fn include(mut self, element: I) -> Self::Output {
        self.include_mut(element);
        self
    }

    fn include_mut(&mut self, element: I) {
        self.add_range(StepRange::new(element.clone(), element, I::one()));
    }

    fn exclude_mut(&mut self, element: &I) {
        if let Some(rem_ind) = self.get_containing_range_index(element) {
            let rem_result = self.ranges[rem_ind].clone().remove_and_split(element);
            match rem_result {
                None => {
                    self.ranges.remove(rem_ind);
                }
                Some((modified, None)) => {
                    self.ranges[rem_ind] = modified;
                }
                Some((l, Some(r))) => {
                    self.ranges[rem_ind] = l;
                    self.ranges.insert(rem_ind + 1, r);
                }
            };
        }
    }
}

#[test]
fn simple() {
    let a = StripeSet::new(vec![StepRange::new(0, 12, 2), StepRange::new(15, 20, 1)]);
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
        vec![StepRange::new(0, 14, 2), StepRange::new(15, 20, 1)]
    );

    let mut a = StripeSet::new(vec![]);

    a.add_range(StepRange::new(10, 18, 2));
    a.add_range(StepRange::new(0, 10, 2));
    a.add_range(StepRange::new(15, 20, 1));
    assert_eq!(
        a.ranges,
        vec![StepRange::new(0, 14, 2), StepRange::new(15, 20, 1)]
    );

    a.add_range(StepRange::new(20, 25, 1));

    assert_eq!(
        a.ranges,
        vec![StepRange::new(0, 14, 2), StepRange::new(15, 25, 1)]
    );
}

#[test]
fn subset() {
    let a = StripeSet::new(vec![StepRange::new(0, 6, 2), StepRange::new(10, 22, 6)]);
    let b = StripeSet::new(vec![StepRange::new(6, 14, 4)]);
    assert!(b.subset_of(&a));
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

#[test]
fn arith() {
    let a = StripeSet::new(vec![StepRange::new(0, 5, 1)]);
    let b = StripeSet::new(vec![StepRange::new(0, 100, 10)]);
    let c = a.arith_add(&b);
    assert_eq!(
        c.ranges,
        vec![
            StepRange::new(0, 100, 10),
            StepRange::new(1, 101, 10),
            StepRange::new(2, 102, 10),
            StepRange::new(3, 103, 10),
            StepRange::new(4, 104, 10),
            StepRange::new(5, 105, 10)
        ]
    );

    let d = c.arith_add(&a);
    assert_eq!(d, StripeSet::new(vec![StepRange::new(0, 110, 1)]))
}
