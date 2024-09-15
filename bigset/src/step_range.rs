use std::ops::{Add, Mul, Sub};

use num_integer::Integer;

use crate::ops::{Bounded, Intersect, Set, Subset};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StepRange<I: Integer + Clone> {
    lo: I,
    hi: I,
    step: I,
}

impl<I: Integer + Clone> StepRange<I> {
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

    pub fn with_lo(self, lo: I) -> Self {
        Self::new(lo, self.hi, self.step)
    }

    pub fn with_hi(self, hi: I) -> Self {
        Self::new(self.lo, hi, self.step)
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
    I: Integer + Clone,
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
    I: Integer + Clone,
{
    fn includes(&self, element: I) -> bool {
        element >= self.lo
            && element <= self.hi
            && (element - self.lo.clone()).is_multiple_of(&self.step)
    }
}

impl<'a, I> Set<&'a I> for StepRange<I>
where
    I: Integer + Clone,
{
    fn includes(&self, element: &'a I) -> bool {
        element >= self.lo()
            && element <= self.hi()
            && (element.clone() - self.lo.clone()).is_multiple_of(&self.step)
    }
}

impl<'a, I> Subset for &'a StepRange<I>
where
    I: Integer + Clone,
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
    I: Integer + Clone,
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
    I: Integer + Clone,
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
