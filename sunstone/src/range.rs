use core::panic;
use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Sub},
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

    fn new_unchecked(lo: T, hi: T) -> Self {
        Self { lo, hi }
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
        // TODO: handle overflow
        self.hi().clone() - self.lo()
    }
}

impl<'a, T: std::cmp::Ord> Range<&'a T>
where
    for<'b> &'a T: Sub<&'b T>,
{
    pub fn len(&self) -> <&'a T as Sub<&'_ T>>::Output {
        // TODO: handle overflow
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
        Range::new_unchecked(self.lo + rhs.lo, self.hi + rhs.hi)
    }
}

impl<T: Ord + Mul<T, Output = OutputT> + Add<T, Output = T> + num::Zero + Clone, OutputT: Ord>
    Mul<Self> for Range<T>
{
    type Output = Range<OutputT>;

    fn mul(self, rhs: Self) -> Self::Output {
        let (min, max): (_, _) = match (
            self.lo >= T::zero(),
            self.hi >= T::zero(),
            rhs.lo >= T::zero(),
            rhs.hi >= T::zero(),
        ) {
            (true, true, true, true) => (self.lo * rhs.lo, self.hi * rhs.hi),
            (false, false, false, false) => (self.hi * rhs.hi, self.lo * rhs.lo),

            (true, true, false, true) => (self.hi.clone() * rhs.lo, self.hi * rhs.hi),

            (false, true, true, true) => (self.lo * rhs.hi.clone(), self.hi * rhs.hi),
            (false, true, false, true) => (self.lo * rhs.hi.clone(), self.hi * rhs.hi),
            (false, true, false, false) => (self.hi * rhs.hi.clone(), self.lo * rhs.hi),

            (false, false, true, true) => (self.lo * rhs.hi, self.hi * rhs.lo),
            (false, false, false, true) => (self.lo * rhs.hi, self.hi * rhs.lo),

            (true, true, false, false) => (self.hi * rhs.lo, self.lo * rhs.hi),

            // invalid cases
            (true, false, _, _) | (_, _, true, false) => {
                panic!("invalid range, lower endpoint is positive, but upper is not")
            }
        };

        Range::new_unchecked(min, max)
    }
}
impl<T: Ord + Sub<T, Output = OutputT> + Add<T, Output = T> + num::Zero, OutputT: Ord> Sub<Self>
    for Range<T>
{
    type Output = Range<OutputT>;

    fn sub(self, rhs: Self) -> Self::Output {
        Range::new_unchecked(self.lo - rhs.hi, self.hi - rhs.lo)
    }
}

impl<
        T: Ord + Div<T, Output = OutputT> + Sub<T, Output = T> + num::Zero + num::One + Clone,
        OutputT: Ord,
    > Div<Self> for Range<T>
{
    type Output = Range<OutputT>;

    fn div(self, rhs: Self) -> Self::Output {
        let Self {
            lo: rhs_lo_maybe_zero,
            hi: rhs_hi_maybe_zero,
        } = rhs;

        // Exclude zero if possible, since division by zero is undefined
        let rhs_lo = if rhs_lo_maybe_zero.is_zero() {
            if rhs_hi_maybe_zero < T::zero() {
                // hacky way to get negative one without explicitly requiring T to be signed
                // we know T is signed in this case because rhs_hi < 0,
                T::zero() - T::one()
            } else if rhs_hi_maybe_zero > T::zero() {
                T::one()
            } else {
                panic!("cannot divide by the range [0,0]");
            }
        } else {
            rhs_lo_maybe_zero
        };
        let rhs_hi = if rhs_hi_maybe_zero.is_zero() {
            T::zero() - T::one()
        } else {
            rhs_hi_maybe_zero
        };

        let (min, max): (_, _) = match (
            self.lo >= T::zero(),
            self.hi >= T::zero(),
            rhs_lo > T::zero(),
            rhs_hi > T::zero(),
        ) {
            // same sign for all endpoints => positive result
            (true, true, true, true) => (self.lo / rhs_hi, self.hi / rhs_lo),
            (false, false, false, false) => (self.hi / rhs_lo, self.lo / rhs_hi),

            // single element of rhs is negative => lower endpoint is negative, upper is positive
            (true, true, false, true) => (self.hi.clone() / rhs_lo, self.hi / rhs_hi),

            // lower bound of lhs is negative => keep polarity for both sides
            (false, true, true, true) => (self.lo / rhs_lo.clone(), self.hi / rhs_lo),
            (false, true, false, true) => (self.lo / rhs_hi.clone(), self.hi / rhs_hi),
            (false, true, false, false) => (self.hi / rhs_lo.clone(), self.lo / rhs_lo),

            // both bounds of lhs are negative => try to make upper positive, shift lower as little as possible
            (false, false, true, true) => (self.lo / rhs_lo, self.hi / rhs_hi),
            (false, false, false, true) => (self.lo / rhs_hi, self.hi / rhs_lo),

            // rhs is entirely negative => result is entirely negative
            (true, true, false, false) => (self.hi.clone() / rhs_hi, self.lo / rhs_lo),

            // invalid cases
            (true, false, _, _) | (_, _, true, false) => {
                panic!("invalid range, lower endpoint is positive, but upper is not")
            }
        };

        Range::new_unchecked(min, max)
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Debug;

    use crate::{ops::Bounded, range::Range};
    use num::{Integer, Zero};
    use proptest::prelude::*;

    fn range_strategy<
        I: Integer + Debug,
        StratA: Strategy<Value = I>,
        StratB: Strategy<Value = I>,
    >(
        a: StratA,
        b: StratB,
    ) -> impl Strategy<Value = Range<I>> {
        (a, b).prop_map(|(a, b)| {
            // using if-statement over a.min(b), ... here to make the borrow checker happy
            if a < b {
                Range::new(a, b)
            } else {
                Range::new(b, a)
            }
        })
    }

    #[allow(dead_code)]
    fn any_range<I: Integer + Debug + Arbitrary>() -> impl Strategy<Value = Range<I>> {
        range_strategy(any::<I>(), any::<I>())
    }

    fn arith_range() -> impl Strategy<Value = Range<i64>> {
        // give room so we don't overflow
        range_strategy(
            i32::MIN as i64..i32::MAX as i64,
            i32::MIN as i64..i32::MAX as i64,
        )
    }

    proptest!(

        #[test]
        fn range_div_returns_valid_range(numerator in arith_range(), denominator in arith_range()) {
            prop_assume!(!numerator.lo().is_zero() && !denominator.hi().is_zero());

            let result = numerator / denominator;
            prop_assert!(result.lo <= result.hi)

        }

        #[test]
        fn range_mul_returns_valid_range(a in arith_range(), b in arith_range()) {
            let result = a * b;
            prop_assert!(result.lo <= result.hi, "a * b = {result:?}");
        }

        #[test]
        fn range_add_returns_valid_range(a in arith_range(), b in arith_range()) {
            let result = a + b;
            prop_assert!(result.lo <= result.hi)

        }

        #[test]
        fn range_sub_returns_valid_range(a in arith_range(), b in arith_range()) {
            let result = a - b;
            prop_assert!(result.lo <= result.hi)

        }
    );
}
