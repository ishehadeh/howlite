use num_integer::Integer;
use tracing::warn;

use crate::{
    ops::{Bounded, Intersect, Set, SetOpIncludes, Subset},
    SetElement,
};

#[cfg(test)]
pub trait RangeValue: Integer + Clone + std::fmt::Debug {}

#[cfg(not(test))]
pub trait RangeValue: Integer + Clone {}

#[cfg(test)]
impl<T> RangeValue for T where T: Integer + Clone + std::fmt::Debug {}

#[cfg(not(test))]
impl<T> RangeValue for T where T: Integer + Clone {}

pub struct StepRangeIter<'a, I: SetElement> {
    range: &'a StepRange<I>,
    x: I,
}

impl<'a, I: SetElement> Iterator for StepRangeIter<'a, I> {
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
#[derive(Clone, PartialEq, Eq)]
pub struct StepRange<I: SetElement> {
    lo: I,
    hi: I,
    step: I,
}

impl<I: SetElement + std::fmt::Debug> std::fmt::Debug for StepRange<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{:?}, {:?}, step: {:?}]",
            &self.lo, &self.hi, &self.step
        )
    }
}

impl<'a, I: SetElement + 'a> IntoIterator for &'a StepRange<I> {
    type Item = I;

    type IntoIter = StepRangeIter<'a, I>;

    fn into_iter(self) -> Self::IntoIter {
        StepRangeIter {
            range: self,
            x: self.lo.clone(),
        }
    }
}

impl<I: SetElement> StepRange<I> {
    #[inline]
    pub fn try_new(lo: I, hi: I, step: I) -> Option<StepRange<I>> {
        if lo <= hi && step >= I::zero() {
            let dist = hi.clone() % &step - lo.clone() % &step;
            if dist.is_multiple_of(&step) {
                Some(StepRange { lo, hi, step })
            } else {
                warn!(lo = ?&lo, hi = ?&hi, step = ?&step, ?dist, "invalid range parameters, (dist % step == 0 is false)");
                None
            }
        } else {
            warn!(lo = ?&lo, hi = ?&hi, step = ?&step, "invalid range parameters, (lo <= hi is {}), (step >= 1 is {})", lo <= hi, step >= I::zero());
            None
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
        self.hi = hi;
        assert!(self.hi >= self.lo);
        assert!((self.hi.clone() - &self.lo).is_multiple_of(self.step()));
    }

    pub fn set_lo(&mut self, lo: I) {
        self.lo = lo;
        assert!(self.hi >= self.lo);
        assert!((self.hi.clone() - &self.lo).is_multiple_of(self.step()));
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

    /// return the first element in step_range after `n`
    pub fn first_element_after(&self, n: I) -> I {
        (n + I::one()).next_multiple_of(&self.step) + self.offset()
    }

    /// return the first element in step_range greater or equal `n`
    pub fn first_element_ge(&self, n: I) -> Option<I> {
        if &n <= self.lo() {
            Some(self.lo().clone())
        } else if &n >= self.hi() {
            None
        } else if self.step().is_one() {
            Some(n)
        } else {
            Some((n - self.offset()).next_multiple_of(&self.step) + self.offset())
        }
    }

    /// return the first element in step_range less or equal to `n`
    pub fn first_element_le(&self, n: I) -> Option<I> {
        if &n <= self.lo() {
            None
        } else if &n >= self.hi() {
            Some(self.hi().clone())
        } else if self.step().is_one() {
            Some(n)
        } else {
            Some(n.prev_multiple_of(&self.step) + self.offset())
        }
    }

    /// shorten this range if `other` covers part of it. If `other` covers all of `self`, return true
    pub fn compactify_mut(&mut self, other: &StepRange<I>) -> bool {
        let has_lo = other.includes(self.lo());
        let has_hi = other.includes(self.hi());
        if (has_lo || has_hi) && self.step().is_multiple_of(other.step()) {
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
        } else {
            false
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

    /// Returns true if this set contains a single element.
    /// Marginally more efficient than calling StepRange::size(), if arithmetic is costly.
    pub fn is_size_one(&self) -> bool {
        self.hi() == self.lo()
    }

    /// split into two, leaving self containing all elements  < el, and returning all elements >= el (if they exist)
    ///
    /// ## Panics
    /// - if self.lo() > el
    /// - if self.hi() < el
    pub fn split_at_exclusive(&mut self, el: &I) -> Self {
        assert!(self.lo() < el);
        assert!(self.hi() > el);

        let mut high_part = self.clone();
        let split_elem = self
            .first_element_ge(el.clone())
            .expect("StepRange::first_element_ge() return None, this should be unreachable.");
        high_part.set_lo(split_elem.clone());
        self.set_hi(split_elem - self.step());
        high_part
    }

    /// remove the given element and split the range around it
    pub fn remove_and_split(mut self, el: &I) -> Option<(Self, Option<Self>)> {
        // special case: remove either hi or lo endpoint
        if el == self.lo() {
            if self.lo() == self.hi() {
                None
            } else {
                self.lo = self.lo.clone() + self.step();
                Some((self, None))
            }
        } else if el == self.hi() {
            if self.lo() == self.hi() {
                None
            } else {
                self.hi = self.hi.clone() - self.step();
                Some((self, None))
            }
        } else if el >= self.lo() && (el.clone() - self.lo()).is_multiple_of(self.step()) {
            let set_lo = StepRange::new(self.lo, el.clone() - &self.step, self.step.clone());
            let set_hi = StepRange::new(el.clone() + &self.step, self.hi, self.step.clone());
            Some((set_lo, Some(set_hi)))
        } else {
            // not in set
            Some((self, None))
        }
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
    I: SetElement,
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

impl<I: SetElement> Set for StepRange<I> {
    type ElementT = I;
}

impl<I> SetOpIncludes<I> for StepRange<I>
where
    I: SetElement,
{
    fn includes(&self, element: I) -> bool {
        element >= self.lo
            && element <= self.hi
            && (element - self.lo.clone()).is_multiple_of(&self.step)
    }
}

impl<'a, I> SetOpIncludes<&'a I> for StepRange<I>
where
    I: SetElement,
{
    fn includes(&self, element: &'a I) -> bool {
        element >= self.lo()
            && element <= self.hi()
            && (element.clone() - self.lo.clone()).is_multiple_of(&self.step)
    }
}

impl<'a, I> Subset for &'a StepRange<I>
where
    I: SetElement,
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
    I: SetElement,
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
    I: SetElement,
{
    fn lo(&self) -> &I {
        &self.lo
    }

    fn hi(&self) -> &I {
        &self.hi
    }
}

impl<I: SetElement> From<crate::range::Range<I>> for StepRange<I> {
    fn from(value: crate::range::Range<I>) -> Self {
        let (lo, hi) = value.into_tuple();
        Self::new(lo, hi, I::one())
    }
}

#[cfg(test)]

mod test {
    use crate::{ops::Subset, step_range::StepRange};

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

    #[test]
    fn split_at_exclusive() {
        let mut lower = StepRange::new(2, 26, 4);
        let upper = lower.split_at_exclusive(&10);
        assert_eq!(lower, StepRange::new(2, 6, 4));
        assert_eq!(upper, StepRange::new(10, 26, 4));

        let mut lower = StepRange::new(2, 8, 3);
        let upper = lower.split_at_exclusive(&3);
        assert_eq!(lower, StepRange::new(2, 2, 3));
        assert_eq!(upper, StepRange::new(5, 8, 3));
    }
}
