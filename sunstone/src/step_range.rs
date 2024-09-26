use std::fmt::Debug;

use num_integer::Integer;

use crate::ops::{Bounded, Intersect, Set, Subset};

#[cfg(test)]
pub trait RangeValue: Integer + Clone + Debug {}

#[cfg(not(test))]
pub trait RangeValue: Integer + Clone {}

#[cfg(test)]
impl<T> RangeValue for T where T: Integer + Clone + Debug {}

#[cfg(not(test))]
impl<T> RangeValue for T where T: Integer + Clone {}

pub struct StepRangeIter<'a, I: RangeValue> {
    range: &'a StepRange<I>,
    x: I,
}

impl<'a, I: RangeValue> Iterator for StepRangeIter<'a, I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        if self.x > self.range.hi {
            None
        } else {
            let val = self.x.clone();
            self.x = self.range.step.clone() + self.x.clone();
            Some(val)
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StepRange<I: RangeValue> {
    lo: I,
    hi: I,
    step: I,
}

impl<'a, I: RangeValue + 'a> IntoIterator for &'a StepRange<I> {
    type Item = I;

    type IntoIter = StepRangeIter<'a, I>;

    fn into_iter(self) -> Self::IntoIter {
        StepRangeIter {
            range: self,
            x: self.lo.clone(),
        }
    }
}

impl<I: RangeValue> StepRange<I> {
    #[inline]
    pub fn try_new(lo: I, hi: I, step: I) -> Option<StepRange<I>> {
        if step <= I::zero() || lo > hi || !(hi.clone() - lo.clone()).is_multiple_of(&step) {
            None
        } else {
            Some(StepRange { lo, hi, step })
        }
    }

    #[inline]
    pub fn new(lo: I, hi: I, step: I) -> StepRange<I> {
        Self::try_new(lo, hi, step).expect("invalid range parameters")
    }

    pub fn step(&self) -> &I {
        &self.step
    }

    pub fn set_hi(&mut self, hi: I) {
        self.hi = hi; // TODO: check invariants!
    }

    pub fn with_lo(self, lo: I) -> Self {
        assert!(lo <= self.hi);
        let l = (lo - self.hi.clone()).next_multiple_of(&self.step);
        Self::new(l + self.hi.clone(), self.hi, self.step)
    }

    pub fn with_hi(self, hi: I) -> Self {
        assert!(hi >= self.lo);
        let h = (hi - self.lo.clone()).prev_multiple_of(&self.step);
        Self::new(self.lo.clone(), h + self.lo, self.step)
    }

    /// deconstruct into (lo, hi, step)
    pub fn deconstruct(self) -> (I, I, I) {
        (self.lo, self.hi, self.step)
    }

    /// offset from an multiple of `step`. i.e. `lo % step``
    pub fn offset(&self) -> I {
        self.lo().mod_floor(self.step())
    }

    /// self.step - self.offset
    pub fn offset_inv(&self) -> I {
        self.step().clone() - self.offset()
    }

    /// return the first element in step_range below `n`
    pub fn first_element_before(&self, n: I) -> I {
        (n - I::one()).prev_multiple_of(&self.step) - self.offset()
    }

    /// return the first element in step_range below `n`
    pub fn first_element_after(&self, n: I) -> I {
        (n + I::one()).next_multiple_of(&self.step) + self.offset()
    }

    /// compactify, and return true if the range is completey contained in other
    pub fn compactify_mut(&mut self, other: &StepRange<I>) -> bool {
        if !self.step().is_multiple_of(&other.step) {
            return true;
        }

        let has_lo = other.includes(self.lo());
        let has_hi = other.includes(self.hi());
        if has_hi && has_lo {
            false
        } else if has_hi {
            self.hi = self.first_element_before(other.lo().clone());
            true
        } else if has_lo {
            self.lo = self.first_element_after(other.hi().clone());
            true
        } else {
            true
        }
    }

    pub fn compactify(mut self, other: &StepRange<I>) -> Option<Self> {
        if self.compactify_mut(other) {
            Some(self)
        } else {
            None
        }
    }

    pub fn arith_add(self, other: StepRange<I>) -> StepRange<I> {
        StepRange::new(
            self.lo + other.lo,
            self.hi + other.hi,
            self.step + other.step,
        )
    }

    pub fn arith_mul(self, other: StepRange<I>) -> StepRange<I> {
        StepRange::new(
            self.lo * other.lo,
            self.hi * other.hi,
            self.step * other.step,
        )
    }

    pub fn size(&self) -> I {
        (self.hi().clone() - self.lo().clone()) / self.step().clone()
    }

    pub fn is_size_one(&self) -> bool {
        self.hi() == self.lo()
    }

    /// Returns true if the step and offset of this range and `other` are compatible.
    /// If true then if these ranges lo/hi ranges intersect the two ranges have intersecting values.
    pub fn might_intersect(&self, other: &StepRange<I>) -> bool {
        // 1 - check that the steps are compatible
        //  i.e: there exists N such that every N elements in RHS there exists an element in SELF.
        if !self.step.is_multiple_of(&other.step) {
            return false;
        }

        // 2 - check that ranges have a compatible offset
        //  i.e. they aren't striped. e.g. step=5 & start=1 isn't compatible with step=5 & start = 0
        // TODO: can we avoid calls to clone here?
        let lo_offset = self.lo.clone() - other.lo.clone();
        lo_offset.is_zero() || lo_offset.is_multiple_of(&other.step)
    }
}

impl<I> Intersect for StepRange<I>
where
    I: RangeValue,
{
    type Output = Option<StepRange<I>>;

    fn intersect(self, rhs: Self) -> Self::Output {
        if self.might_intersect(&rhs) {
            StepRange::try_new(
                self.lo.max(rhs.lo),
                self.hi.min(rhs.hi),
                self.step.max(rhs.step),
            )
        } else {
            None
        }
    }
}

impl<I> Set<I> for StepRange<I>
where
    I: RangeValue,
{
    fn includes(&self, element: I) -> bool {
        element >= self.lo
            && element <= self.hi
            && (element - self.lo.clone()).is_multiple_of(&self.step)
    }
}

impl<'a, I> Set<&'a I> for StepRange<I>
where
    I: RangeValue,
{
    fn includes(&self, element: &'a I) -> bool {
        element >= self.lo()
            && element <= self.hi()
            && (element.clone() - self.lo.clone()).is_multiple_of(&self.step)
    }
}

impl<'a, I> Subset for &'a StepRange<I>
where
    I: RangeValue,
{
    #[inline]
    fn subset_of(self, rhs: Self) -> bool {
        self.might_intersect(rhs) && rhs.lo <= self.lo && rhs.hi >= self.hi
    }

    #[inline]
    fn strict_subset_of(self, rhs: Self) -> bool {
        self.lo != rhs.lo && self.hi != rhs.hi && self.subset_of(rhs)
    }
}

impl<I> Bounded<I> for StepRange<I>
where
    I: RangeValue,
{
    fn lo(&self) -> &I {
        &self.lo
    }

    fn hi(&self) -> &I {
        &self.hi
    }
}

impl<'a, I> Bounded<I> for &'a StepRange<I>
where
    I: RangeValue,
{
    fn lo(&self) -> &I {
        &self.lo
    }

    fn hi(&self) -> &I {
        &self.hi
    }
}

#[test]
fn simple() {
    let a = StepRange::new(0, 100, 1);
    let b = StepRange::new(0, 50, 5);
    assert!(!a.subset_of(&b));
    assert!(b.subset_of(&a));

    let c = StepRange::new(0, 60, 3);
    let d = StepRange::new(5, 32, 9);
    assert!(!d.subset_of(&c));

    let e = StepRange::new(3, 30, 9);
    assert!(e.subset_of(&c));
}

#[test]
fn bound_adjust() {
    assert_eq!(StepRange::new(1, 7, 3).with_lo(3), StepRange::new(4, 7, 3));
    assert_eq!(
        StepRange::new(0, 12, 3).with_lo(4),
        StepRange::new(6, 12, 3)
    );
    assert_eq!(
        StepRange::new(0, 25, 5).with_lo(12),
        StepRange::new(15, 25, 5)
    );
    assert_eq!(
        StepRange::new(10, 25, 5).with_lo(25),
        StepRange::new(25, 25, 5)
    );

    assert_eq!(StepRange::new(1, 7, 3).with_hi(5), StepRange::new(1, 4, 3));
    assert_eq!(StepRange::new(0, 12, 3).with_hi(8), StepRange::new(0, 6, 3));
    assert_eq!(
        StepRange::new(5, 25, 5).with_hi(12),
        StepRange::new(5, 10, 5)
    );
    assert_eq!(
        StepRange::new(10, 25, 5).with_hi(10),
        StepRange::new(10, 10, 5)
    );
}

#[test]
fn compactify() {
    let a = StepRange::new(2, 26, 4);
    let b = StepRange::new(10, 26, 2);
    assert_eq!(a.compactify(&b), Some(StepRange::new(2, 6, 4)))
}
