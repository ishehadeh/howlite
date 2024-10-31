use std::{
    cmp::Ordering,
    ops::{Add, Mul, Sub},
};

use num::Integer;

use crate::ops::{self, Bounded};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Range<T: std::cmp::Ord> {
    lo: T,
    hi: T,
}

impl<T: std::cmp::Ord> Range<T> {
    pub fn new(lo: T, hi: T) -> Self {
        Self::try_new(lo, hi).expect("range lower bound must be <= upper bound")
    }

    pub fn try_new(lo: T, hi: T) -> Option<Self> {
        if hi < lo {
            None
        } else {
            Some(Range { lo, hi })
        }
    }

    pub fn into_tuple(self) -> (T, T) {
        (self.lo, self.hi)
    }

    /// Split self around the other
    /// Return values:
    ///     (None, None)       - other completely covers this range
    ///     (Some(a), None)    - other covers the upper bound of this range or exceeds the range entirely, a is [self.lo, min(other.lo, self.hi)]
    ///     (None, Some(b))    - other covers the lower bound of this range or is smaller than the range entirely, b is [max(other.hi, self.lo), self.hi]
    ///     (Some(a), Some(b)) - a is [self.lo, other.lo], and b is [other.hi, self.hi]
    pub fn split_around(self, other: Range<T>) -> (Option<Self>, Option<Self>) {
        let (abs_lo, abs_hi) = self.into_tuple();
        let (inner_lo, inner_hi) = other.into_tuple();

        // you could do this much easier with two `if` trees
        // This stupid match is to convince the compiler I wont use-after-move
        // there's likely an easier way, but this works ok.
        match (
            inner_lo.cmp(&abs_hi),
            inner_hi.cmp(&abs_lo),
            inner_hi.cmp(&abs_hi),
            inner_lo.cmp(&abs_lo),
        ) {
            // inner exceeds the self:
            //   > self
            (Ordering::Greater, _, _, _) => (Some(Self::new(abs_lo, abs_hi)), None),

            //   < self
            (_, Ordering::Less, _, _) => (None, Some(Self::new(abs_lo, abs_hi))),

            // inner is entirely within self
            (_, _, Ordering::Less, Ordering::Greater) => (
                Some(Self::new(abs_lo, inner_lo)),
                Some(Self::new(inner_hi, abs_hi)),
            ),

            // inner covers self completely
            (_, _, Ordering::Greater | Ordering::Equal, Ordering::Less | Ordering::Equal) => {
                (None, None)
            }

            // partial cover:
            //   self.lo covered by inner
            (_, _, Ordering::Greater | Ordering::Equal, Ordering::Greater) => {
                (None, Some(Self::new(inner_hi, abs_hi)))
            }

            (_, _, Ordering::Less, Ordering::Less | Ordering::Equal) => {
                (Some(Self::new(abs_lo, inner_hi)), None)
            }
        }
    }
}

impl<T: Integer> Range<T> {
    pub fn remove_range(self, other: Self) -> Option<(Self, Option<Self>)> {
        match (other.lo().cmp(self.lo()), other.hi().cmp(self.hi())) {
            (Ordering::Less | Ordering::Equal, Ordering::Less) => {
                Some((Range::new(other.hi + T::one(), self.hi), None))
            }
            (Ordering::Greater, Ordering::Greater | Ordering::Equal) => {
                Some((Range::new(self.lo, other.lo - T::one()), None))
            }
            (Ordering::Less | Ordering::Equal, Ordering::Equal | Ordering::Greater) => None,
            (Ordering::Greater, Ordering::Less) => Some((
                Range::new(self.lo, other.lo - T::one()),
                Some(Range::new(other.hi + T::one(), self.hi)),
            )),
        }
    }
}

impl<T: std::cmp::Ord> Range<T>
where
    for<'a> T: Clone + Sub<&'a T>,
{
    pub fn len_clone(&self) -> <T as Sub<&'_ T>>::Output {
        self.hi().clone() - self.lo()
    }
}

impl<'a, T: std::cmp::Ord> Range<&'a T>
where
    for<'b> &'a T: Sub<&'b T>,
{
    pub fn len(&self) -> <&'a T as Sub<&'_ T>>::Output {
        self.hi - self.lo
    }
}

impl<'a, T: std::cmp::Ord + Clone> Range<&'a T> {
    pub fn clone_endpoints(&self) -> Range<T> {
        Range::new(self.lo.clone(), self.hi.clone())
    }
}

impl<T: std::cmp::Ord> ops::Bounded<T> for Range<T> {
    fn lo(&self) -> &T {
        &self.lo
    }

    fn hi(&self) -> &T {
        &self.hi
    }
}

impl<'a, T: std::cmp::Ord> ops::Subset for &'a Range<T> {
    fn subset_of(self, rhs: Self) -> bool {
        rhs.lo() <= self.lo() && rhs.hi() >= self.hi()
    }

    fn strict_subset_of(self, rhs: Self) -> bool {
        rhs.lo() < self.lo() && rhs.hi() > self.hi()
    }
}

impl<T: Ord> ops::Intersect for Range<T> {
    type Output = Option<Range<T>>;

    fn intersect(self, rhs: Self) -> Self::Output {
        let (l_lo, l_hi) = self.into_tuple();
        let (r_lo, r_hi) = rhs.into_tuple();
        Range::try_new(T::max(l_lo, r_lo), T::min(l_hi, r_hi))
    }
}

impl<T: Ord> ops::Set for Range<T> {
    type ElementT = T;
}

impl<T: Ord> ops::SetOpIncludes<T> for Range<T> {
    fn includes(&self, element: T) -> bool {
        &element >= self.lo() && &element <= self.hi()
    }
}

impl<'a, T: Ord> ops::SetOpIncludes<&'a T> for Range<T> {
    fn includes(&self, element: &'a T) -> bool {
        element >= self.lo() && element <= self.hi()
    }
}

impl<T: Ord + Add<T, Output = OutputT>, OutputT: Ord> Add<Self> for Range<T> {
    type Output = Range<OutputT>;

    fn add(self, rhs: Self) -> Self::Output {
        Range::new(self.lo + rhs.lo, self.hi + rhs.hi)
    }
}

impl<T: Ord + Mul<T, Output = OutputT>, OutputT: Ord> Mul<Self> for Range<T> {
    type Output = Range<OutputT>;

    fn mul(self, rhs: Self) -> Self::Output {
        Range::new(self.lo * rhs.lo, self.hi * rhs.hi)
    }
}
