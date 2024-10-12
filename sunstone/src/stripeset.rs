use std::cmp::Ordering;

use crate::{
    ops::{Bounded, Set, SetOpIncludeExclude, SetOpIncludes, SetSubtract, Subset, Union},
    range::Range,
    step_range::StepRange,
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

    pub fn arith_sub_scalar(&mut self, rhs: &I) {
        for range in self.ranges.iter_mut() {
            let new_hi = range.hi().clone() - rhs;
            let new_lo = range.lo().clone() - rhs;
            if new_hi >= new_lo {
                *range = StepRange::new(new_lo, new_hi, range.step().clone());
            } else {
                *range = StepRange::new(new_hi, new_lo, range.step().clone());
            }
        }
    }

    pub fn arith_add_scalar(&mut self, rhs: &I) {
        for range in self.ranges.iter_mut() {
            let new_hi = range.hi().clone() + rhs;
            let new_lo = range.lo().clone() + rhs;
            if new_hi >= new_lo {
                *range = StepRange::new(new_lo, new_hi, range.step().clone());
            } else {
                *range = StepRange::new(new_hi, new_lo, range.step().clone());
            }
        }
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

    pub fn split_at(&mut self, num: &I) -> Self {
        let mut first_el_to_remove = None;

        // first, if some ranges are entirely above the split, get those
        for (i, range) in self.ranges.iter().enumerate().rev() {
            if range.lo() > num {
                first_el_to_remove = Some(i);
                break;
            }
        }

        let mut new = if let Some(first_el_to_remove) = first_el_to_remove {
            self.ranges.drain(first_el_to_remove..).collect()
        } else {
            Vec::new()
        };

        // second, start splitting ranges that span over `num`
        for range in self.ranges.iter_mut().rev() {
            if range.lo() < num && num < range.hi() {
                new.push(range.split_at_exclusive(num))
            }
        }

        Self::new(new)
    }

    pub fn modulo(&mut self, n: &I) {
        while self.get_range().is_some_and(|r| r.hi() >= n) {
            let mut hi = self.split_at(n);
            dbg!(&hi);
            hi.arith_sub_scalar(n);
            dbg!(&hi);
            self.union(hi);
            dbg!(&self);
        }
    }

    pub fn get_range(&self) -> Option<Range<I>> {
        if self.ranges.is_empty() {
            return None;
        }
        let mut lo = self.ranges[0].lo();
        let mut hi = self.ranges[0].hi();
        for r in self.ranges.iter().skip(1) {
            lo = r.lo().min(lo);
            hi = r.hi().max(hi);
        }

        Some(Range::new(lo.clone(), hi.clone()))
    }

    /// remove all elements above `new_maximum`. `new_maximum` will not be removed.
    pub fn exclude_above(&mut self, new_maximum: &I) {
        let mut first_el_to_remove = None;

        // first, remove all ranges that are entirely above `new_maximum`
        for (i, range) in self.ranges.iter().enumerate().rev() {
            if range.lo() > new_maximum {
                first_el_to_remove = Some(i);
                break;
            }
        }
        if let Some(new_len) = first_el_to_remove {
            self.ranges
                .resize(new_len, StepRange::new(I::zero(), I::zero(), I::one()));
        }

        // second, start truncating ranges that span over `new_maximum`
        for range in self.ranges.iter_mut().rev() {
            // note, due to sort order (see cmp_range), we can't actually guarentee we can stop iterating
            //  as soon as!(), `range.hi() < new_maximum`, even if they have equal step values.
            //  it's possible to have a set that looks like:
            //
            // ranges[1]    |-------|
            // ranges[0] |--------------------|
            //                          ^ new_maximum
            // this is because we only sort by lower bound if steps are equal and the ranges overlap.

            // don't use range.includes() here, since new_maximum doesn't need to actually be an element of the `range` set.`
            if range.lo() <= new_maximum && new_maximum < range.hi() {
                range.set_hi(new_maximum.clone());
            }
        }
    }

    /// remove all elements below `new_minimum`. `new_minimum` will not be removed.
    pub fn exclude_below(&mut self, new_minimum: &I) {
        let mut new_start_idx = None;

        // first, remove all ranges that are entirely below `new_minimum`
        for (i, range) in self.ranges.iter().enumerate() {
            if range.hi() < new_minimum {
                new_start_idx = Some(i);
                break;
            }
        }
        if let Some(new_start_idx) = new_start_idx {
            self.ranges.drain(0..new_start_idx);
        }

        // second, start truncating ranges that span over `new_minimum`
        for range in self.ranges.iter_mut().rev() {
            // note, we can't stop iterating as soon as new_minimum < lo, since we sort by step before lo.
            if range.lo() < new_minimum && new_minimum < range.hi() {
                range.set_lo(new_minimum.clone());
            }
        }
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

impl<I: SetElement> Set for StripeSet<I> {
    type ElementT = I;
}

impl<I> SetOpIncludes<I> for StripeSet<I>
where
    I: SetElement,
{
    fn includes(&self, element: I) -> bool {
        self.includes(&element)
    }
}

impl<'a, I> SetOpIncludes<&'a I> for StripeSet<I>
where
    I: SetElement,
{
    fn includes(&self, element: &'a I) -> bool {
        for range in &self.ranges {
            if range.hi() < element {
                break;
            }
            if range.includes(element) {
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

impl<'a, I> Union<StripeSet<I>> for &'a mut StripeSet<I>
where
    I: SetElement,
{
    type Output = Self;

    fn union(self, rhs: StripeSet<I>) -> Self::Output {
        self.ranges.extend_from_slice(&rhs.ranges);
        self.normalize();
        self
    }
}

impl<'a, I: SetElement> SetSubtract<&'a StripeSet<I>> for StripeSet<I> {
    fn set_subtract_mut(&mut self, rhs: &'a Self) {
        for rhs_stripe in rhs.stripes() {
            let mut i = 0;
            // here we go changing the array during iteration again :)
            'check_sub: while i < self.ranges.len() {
                let lhs_stripe = &self.ranges[i];
                if lhs_stripe.lo() > rhs_stripe.hi() || lhs_stripe.hi() < rhs_stripe.lo() {
                    // skip ranges that certainly have no intersect
                    i += 1;
                    continue;
                }

                // step 1) get the least common multiple of the two steps.
                //          This is the interval where they may intersect.
                //             e.g. step 2 & 3 may intersect every 6
                // step 2) determine first element of lhs is rhs.
                //         This could be anything <= lcm
                //         first_el = lcm
                let lcm_step = lhs_stripe.step().lcm(rhs_stripe.step());
                let mut lcm_range = StepRange::new(
                    rhs_stripe.lo().clone(),
                    (rhs_stripe.hi().clone() + I::one()).prev_multiple_of(&lcm_step)
                        - rhs_stripe.lo().mod_floor(&lcm_step),
                    lcm_step,
                );

                // LCM offset may be any N where:
                //  1) N mod step(RHS) = lo(RHS) mod step(RHS)    i.e. N is an element of RHS
                //  2) N < step(LSM)   since, past the step we'd just repeat
                while !lhs_stripe.includes(lcm_range.first_element_ge(lhs_stripe.lo().clone())) {
                    dbg!(lcm_range.first_element_ge(lhs_stripe.lo().clone()));
                    lcm_range.set_lo(lcm_range.lo().clone() + lcm_range.step());
                    if &(lcm_range.step().clone() + rhs_stripe.lo()) <= lcm_range.lo() {
                        // couldn't find any elements in lhs! continue on
                        i += 1;
                        continue 'check_sub;
                    }
                }

                // unfortunately, it looks like we need to modify the range.
                //  we're about to modify the array, so get an owned copy.
                //  to avoid shifting the array any more than we already will,
                //  reserve the space with a tombstone
                let lhs_stripe = std::mem::replace(
                    &mut self.ranges[i],
                    StepRange::new(I::zero(), I::zero(), I::one()),
                );

                let first_lhs_removal = lcm_range.first_element_ge(lhs_stripe.lo().clone());
                let last_lhs_removal = lcm_range.first_element_le(lhs_stripe.hi().clone());
                let lo_dist_to_first_removal = first_lhs_removal.clone() - lhs_stripe.lo();
                let hi_dist_to_last_removal = lhs_stripe.hi().clone() - &last_lhs_removal;

                let lhs_span_length = I::one() + lhs_stripe.hi() - lhs_stripe.lo();

                // if removal is below removed step, we can act like the lower part of the range is removed,
                // since the split from lhs <= the rest.
                // see the below loop for a better explaination
                let has_prefix =
                    &lo_dist_to_first_removal >= lcm_range.step().min(&lhs_span_length);

                let has_suffix = &hi_dist_to_last_removal >= lcm_range.step().min(&lhs_span_length);
                let removal_span_len = I::one() + &last_lhs_removal - &first_lhs_removal;

                let new_ranges_count = lcm_range.step().clone().min(removal_span_len.clone())
                    / lhs_stripe.step()
                    - I::one()
                    + if has_prefix { I::one() } else { I::zero() }
                    + if has_suffix { I::one() } else { I::zero() };

                if new_ranges_count.is_zero() {
                    // our whole range is removed
                    self.ranges.remove(i);
                    continue;
                } else if !new_ranges_count.is_one() {
                    // > 0, != 0, != 1, i.e. new_ranges_count > 1

                    // reserve space for the elements we'll be adding all at once
                    self.ranges.splice(
                        i..i,
                        (0..new_ranges_count.to_usize().unwrap())
                            .map(|_| StepRange::new(I::zero(), I::zero(), I::one())),
                    );
                }

                if has_prefix {
                    self.ranges[i] = StepRange::new(
                        lhs_stripe.lo().clone(),
                        first_lhs_removal.clone() - lhs_stripe.step(),
                        lhs_stripe.step().clone(),
                    );
                    i += 1;
                }

                if has_suffix {
                    self.ranges[i] = StepRange::new(
                        last_lhs_removal.clone() + lhs_stripe.step(),
                        lhs_stripe.hi().clone(),
                        lhs_stripe.step().clone(),
                    );
                    i += 1;
                }

                if first_lhs_removal == last_lhs_removal {
                    continue;
                }

                // let A = first removed element
                //     B = last removed element
                //     T = LCM step (elements removed)
                //     Sn = step(LHS)*n,  Sn < T
                // for all Sn, create a range: [ A + Sn, B - T + Sn, step = T ]
                for new_range_offset in num::range_step(
                    I::zero(),
                    lcm_range.step().clone().min(lhs_span_length),
                    lhs_stripe.step().clone(),
                ) {
                    let start = {
                        // a little trick not mentioned above:
                        // if the distance to the first element removed is < step, we can actually just start there,
                        // allowing us to not chop the original LHS range

                        let base = first_lhs_removal.clone() + &new_range_offset;
                        if !has_prefix && lo_dist_to_first_removal != new_range_offset {
                            base - &lo_dist_to_first_removal
                        } else {
                            base
                        }
                    };

                    let end = if &removal_span_len < lcm_range.step() {
                        start.clone()
                    } else {
                        let base = (start.clone() + &removal_span_len)
                            .prev_multiple_of(lcm_range.step())
                            - lcm_range.step()
                            + start.mod_floor(lcm_range.step());
                        if !has_suffix && hi_dist_to_last_removal != new_range_offset {
                            base + lcm_range.step()
                        } else {
                            base
                        }
                    };

                    self.ranges[i] = StepRange::new(start, end, lcm_range.step().clone());
                    i += 1;
                }
            }
        }
    }
}

impl<I> SetOpIncludeExclude<I> for StripeSet<I>
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
fn subtraction() {
    let mut a = StripeSet::new(vec![StepRange::new(0, 5, 1), StepRange::new(10, 20, 2)]);
    let b = StripeSet::new(vec![StepRange::new(0, 18, 6)]);
    dbg!(&a, &b);
    a.set_subtract_mut(&b);
    dbg!(&a);
    assert!(!a.includes(0));
    assert!(!a.includes(6));
    assert!(!a.includes(12));
    assert!(!a.includes(18));
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

#[test]
fn modulo() {
    let mut a = StripeSet::new(vec![StepRange::new(0, 15, 5)]);
    a.modulo(&4);
    assert_eq!(a, StripeSet::new(vec![StepRange::new(1, 3, 1)]));

    let mut b = StripeSet::new(vec![StepRange::new(1, 20, 1), StepRange::new(3, 39, 6)]);
    b.modulo(&7);
    assert_eq!(b, StripeSet::new(vec![StepRange::new(0, 6, 1)]));
}
