use std::{cmp::Ordering, ops::Neg};

use tracing::{debug, instrument, Level};

use crate::{
    bitfield::BitField,
    ops::{
        ArithmeticSet, Bounded, IntersectMut, PartialBounded, Set, SetOpIncludeExclude,
        SetOpIncludes, SetSubtract, Subset, Union,
    },
    range::Range,
    step_range::StepRange,
    stripeset::StripeSet,
    SetElement,
};

const SMALL_SET_WORD_COUNT: usize = 128;
const SMALL_SET_MAX_RANGE: usize = SMALL_SET_WORD_COUNT * 64;

#[derive(Debug, Clone)]
/// A DynSet uses multiple representations to track its elements as effeciently as possible
pub struct DynSet<I: SetElement> {
    data: DynSetData<I>,
    range: Range<I>,
}

#[derive(Debug, Clone)]
pub enum DynSetData<I: SetElement> {
    Empty,
    Small(SmallSet<I>),
    Contiguous,
    Stripe(StripeSet<I>),
}

#[derive(Debug, Clone)]
pub struct SmallSet<I: SetElement> {
    elements: Box<BitField<SMALL_SET_WORD_COUNT>>,
    offset: I,
}

impl<I: SetElement> SmallSet<I> {
    pub fn stripes(&self) -> impl Iterator<Item = StepRange<I>> + '_ {
        self.elements.iter_step_ranges().map(move |s| {
            StepRange::new(
                I::from_usize(*s.lo()).unwrap() + &self.offset,
                I::from_usize(*s.hi()).unwrap() + &self.offset,
                I::from_usize(*s.step()).unwrap(),
            )
        })
    }
}

impl<I: SetElement> DynSet<I> {
    fn small_set_max_range() -> I {
        I::from_usize(SMALL_SET_MAX_RANGE).expect("I must be constructable from a usize")
    }

    pub fn new_from_range(lo: I, hi: I) -> DynSet<I> {
        DynSet {
            data: DynSetData::Contiguous,
            range: Range::new(lo, hi),
        }
    }

    pub fn new_from_tuples<IntLikeT: Into<I> + Clone>(
        ranges: &[(IntLikeT, IntLikeT)],
    ) -> DynSet<I> {
        let set: StripeSet<_> = StripeSet::new(
            ranges
                .iter()
                .map(|(lo, hi)| StepRange::new(lo.clone().into(), hi.clone().into(), I::one()))
                .collect(),
        );
        set.get_range()
            .map(|range| DynSet {
                data: DynSetData::Stripe(set),
                range,
            })
            .unwrap_or(Self::empty())
    }

    pub fn empty() -> DynSet<I> {
        DynSet {
            data: DynSetData::Empty,
            range: Range::new(I::zero(), I::zero()),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.data, DynSetData::Empty)
    }

    pub fn get_range(&self) -> &Range<I> {
        &self.range
    }

    // remove all elements above `n` and return them in a new set. (n is still in self)
    #[instrument]
    pub fn take_above(&mut self, n: &I) -> Self {
        let lower = self.take_below(&(I::one() + n));
        // I love std::mem::replace :)
        std::mem::replace(self, lower)
    }

    /// Remove elements below `n`, and return them in a new set
    #[instrument]
    pub fn take_below(&mut self, n: &I) -> Self {
        if self.is_empty() || self.range.lo() > n {
            debug!(?n, lo = ?self.range.lo(), is_empty = self.is_empty(), "set is empty, or entirely under limit. Returning Self::empty()");
            return Self::empty();
        }

        if n > self.range.hi() {
            debug!(?n, hi = ?self.range.hi(), "n > hi, swapping self with empty");
            return std::mem::replace(self, Self::empty());
        }

        match &mut self.data {
            DynSetData::Empty => unreachable!(),
            DynSetData::Small(small_set) => {
                // we checked above that we would actually have to split the set in the middle,
                // so the offset into the small set must be valid
                let real_offset = (n.clone() - &small_set.offset)
                    .to_usize()
                    .expect("bad offset into small set bit field - this should be unreachable");
                assert!(
                    real_offset < SMALL_SET_MAX_RANGE,
                    "n - small_set.offset > SMALL_SET_MAX_RANGE: {} >= {}",
                    real_offset,
                    SMALL_SET_MAX_RANGE
                );

                let higher = Box::new(small_set.elements.take_ge(real_offset));
                // we want self to be the higher end
                let lower_field = std::mem::replace(&mut small_set.elements, higher);
                let lower = SmallSet {
                    elements: lower_field,
                    offset: small_set.offset.clone(),
                };

                small_set.offset = small_set.offset.clone() + n;

                if let Some(field_range) = small_set.elements.range() {
                    self.range = Range::new(
                        I::from_usize(*field_range.lo()).unwrap() + &small_set.offset,
                        I::from_usize(*field_range.hi()).unwrap() + &small_set.offset,
                    )
                } else {
                    self.data = DynSetData::Empty
                };

                if let Some(lt_n_range) = lower.elements.range() {
                    Self {
                        range: Range::new(
                            I::from_usize(*lt_n_range.lo()).unwrap() + &lower.offset,
                            I::from_usize(*lt_n_range.hi()).unwrap() + &lower.offset,
                        ),
                        data: DynSetData::Small(lower),
                    }
                } else {
                    Self::empty()
                }
            }
            DynSetData::Contiguous => {
                let split_around_range = Range::new(n.clone() - I::one(), n.clone());
                let (lo, hi) = self
                    .range
                    .clone()
                    .split_around(Range::new(n.clone() - I::one(), n.clone()));

                debug!(?lo, ?hi, "splitting around range: {split_around_range:?}");

                if let Some(hi) = hi {
                    self.range = hi
                } else {
                    self.data = DynSetData::Empty
                }

                if let Some(lo) = lo {
                    let (lo, hi) = lo.into_tuple();
                    Self::new_from_range(lo, hi)
                } else {
                    Self::empty()
                }
            }
            DynSetData::Stripe(stripe_set) => {
                // this return/mutate behavior is the inverse of what we want.
                let elems_ge_n = stripe_set.split_at_exclusive(n);
                let elems_lt_n = std::mem::replace(stripe_set, elems_ge_n);
                if let Some(self_range) = stripe_set.get_range() {
                    self.range = self_range
                } else {
                    self.data = DynSetData::Empty
                };

                if let Some(lt_n_range) = elems_lt_n.get_range() {
                    Self {
                        data: DynSetData::Stripe(elems_lt_n),
                        range: lt_n_range,
                    }
                } else {
                    Self::empty()
                }
            }
        }
    }

    pub fn is_divisible_by(&self, n: &I) -> bool {
        assert!(n > &I::zero());
        match &self.data {
            DynSetData::Empty => true,
            DynSetData::Small(s) => {
                // stupid abs() impl that works even if I isn't signed
                let shift = s.offset.mod_floor(n);
                if let Some(n_usize) = n.to_usize() {
                    // this should never panic since `offset mod n < n`, and n can be converted to a usize
                    let shift_isize = shift.to_isize().unwrap();
                    s.elements.is_divisible_by_with_offset(n_usize, shift_isize)
                } else {
                    false
                }
            }
            DynSetData::Contiguous => {
                n.is_one() || (self.range.len_clone().is_one() && self.range.lo().is_multiple_of(n))
            }
            DynSetData::Stripe(s) => {
                for stripe in s.stripes() {
                    if !stripe.lo().is_multiple_of(n)
                        || !stripe.hi().is_multiple_of(n)
                        || !(stripe.hi() == stripe.lo() || stripe.step().is_multiple_of(n))
                    {
                        return false;
                    }
                }
                true
            }
        }
    }

    pub fn new_from_individual_generic<
        U: Into<I> + SetElement + num_traits::bounds::UpperBounded,
    >(
        slice: &[U],
    ) -> DynSet<I> {
        if slice.is_empty() {
            return Self::empty();
        }

        let (min, max) = slice.iter().fold((&slice[0], &slice[0]), |(min, max), el| {
            if el < min {
                (el, max)
            } else if el > max {
                (min, el)
            } else {
                (min, max)
            }
        });

        let u_small_set_max_range = U::try_from(SMALL_SET_MAX_RANGE);
        // condition here handles the case where SMALL_SET_MAX_RANGE > U::max_values()
        if u_small_set_max_range
            .map(|max_range| max.clone() - min < max_range)
            .unwrap_or_else(|_| {
                U::max_value()
                    .to_usize()
                    .map(|u_max| u_max < SMALL_SET_MAX_RANGE)
                    .unwrap_or(false)
            })
        {
            let mut usize_slice = Vec::with_capacity(slice.len());

            for x in slice.iter() {
                usize_slice.push(
                    (x.clone() - min)
                        .to_usize()
                        .expect("failed to convert element to usize"),
                )
            }
            return DynSet {
                data: DynSetData::Small(SmallSet {
                    elements: Box::new(BitField::from_slice(&usize_slice)),
                    offset: min.clone().into(),
                }),
                range: Range::new(min.clone().into(), max.clone().into()),
            };
        }

        todo!("non-small set from individual")
    }

    pub fn inner(&self) -> &DynSetData<I> {
        &self.data
    }

    pub fn new_from_individual(slice: &[I]) -> DynSet<I> {
        let (min, max) = slice.iter().fold((&slice[0], &slice[0]), |(min, max), el| {
            if el < min {
                (el, max)
            } else if el > max {
                (min, el)
            } else {
                (min, max)
            }
        });

        if let Ok(max_range) = I::try_from(SMALL_SET_MAX_RANGE) {
            if max.clone() - min < max_range {
                let mut usize_slice = Vec::with_capacity(slice.len());

                for x in slice.iter() {
                    usize_slice.push(
                        (x.clone() - min)
                            .to_usize()
                            .expect("failed to convert element to usize"),
                    )
                }
                return DynSet {
                    data: DynSetData::Small(SmallSet {
                        elements: Box::new(BitField::from_slice(&usize_slice)),
                        offset: min.clone(),
                    }),
                    range: Range::new(min.clone(), max.clone()),
                };
            }
        }

        todo!()
    }

    fn upgrade_from_contiguous(&mut self) {
        assert!(matches!(self.data, DynSetData::Contiguous));
        match self.range.len_clone().to_usize() {
            Some(len_usize) if len_usize < SMALL_SET_MAX_RANGE => {
                let offset = self.range.lo().clone();
                self.data = DynSetData::Small(SmallSet {
                    elements: {
                        let mut set = Box::new(BitField::new());
                        set.include_between(0, len_usize + 1);
                        set
                    },
                    offset,
                })
            }
            _ => self.data = DynSetData::Stripe(StripeSet::new(vec![self.range.clone().into()])),
        }

        assert!(!matches!(self.data, DynSetData::Contiguous));
    }

    fn upgrade_from_small(&mut self) {
        assert!(matches!(self.data, DynSetData::Small(_)));
        let new_data = if let DynSetData::Small(small_set) = &self.data {
            let mut small_stripe = StripeSet::new(Vec::with_capacity(16));
            small_set.elements.add_to_stripe(&mut small_stripe);
            StripeSet::new(
                small_stripe
                    .stripes()
                    .map(move |s| {
                        StepRange::new(
                            I::from_usize(*s.lo()).unwrap() + &small_set.offset,
                            I::from_usize(*s.hi()).unwrap() + &small_set.offset,
                            I::from_usize(*s.step()).unwrap(),
                        )
                    })
                    .collect(),
            )
        } else {
            unreachable!();
        };

        self.data = DynSetData::Stripe(new_data);
    }
}

impl<I: SetElement> Union<DynSet<I>> for DynSet<I> {
    type Output = Self;

    fn union(mut self, rhs: DynSet<I>) -> Self::Output {
        (&mut self).union(rhs);
        self
    }
}

impl<'a, I: SetElement> Union<DynSet<I>> for &'a mut DynSet<I> {
    type Output = Self;

    fn union(self, rhs: DynSet<I>) -> Self::Output {
        let DynSet {
            range: rhs_range,
            data: rhs_data,
        } = rhs;
        match (&mut self.data, rhs_data) {
            (d @ DynSetData::Empty, data) => {
                *d = data;
                self.range = rhs_range;
            }
            (_, DynSetData::Empty) => (),

            // one set completely contained within the other
            (data, DynSetData::Contiguous)
                if self.range.lo() >= rhs_range.lo() && self.range.hi() <= rhs_range.hi() =>
            {
                *data = DynSetData::Contiguous;
                self.range = rhs_range;
            }
            (DynSetData::Contiguous, _)
                if self.range.lo() < rhs_range.lo() && self.range.hi() > rhs_range.hi() => {}

            (DynSetData::Small(s1), DynSetData::Small(s2)) => {
                match s1.offset.cmp(&s2.offset) {
                    Ordering::Less => {
                        let offset = (s2.offset.clone() - &s1.offset).to_usize().expect(
                            "bitfield offset during union too large, this should be unreachable",
                        );
                        if offset + rhs_range.hi().to_usize().unwrap() < SMALL_SET_MAX_RANGE {
                            let offset_s2_elems = s2.elements.arith_add_scalar(offset);
                            s1.elements =
                                Box::new(std::mem::take(&mut s1.elements).union(offset_s2_elems));
                        } else {
                            self.upgrade_from_small();
                            return self.union(DynSet {
                                data: DynSetData::Small(s2),
                                range: rhs_range,
                            });
                        }
                    }

                    Ordering::Greater => {
                        let offset = (s1.offset.clone() - &s2.offset).to_usize().expect(
                            "bitfield offset during union too large, this should be unreachable",
                        );
                        if offset + self.range.hi().to_usize().unwrap() < SMALL_SET_MAX_RANGE {
                            s1.elements = Box::new(
                                std::mem::take(&mut s1.elements)
                                    .arith_add_scalar(offset)
                                    .union(*s2.elements),
                            );
                            s1.offset = s2.offset;
                        } else {
                            self.upgrade_from_small();
                            return self.union(DynSet {
                                data: DynSetData::Small(s2),
                                range: rhs_range,
                            });
                        }
                    }

                    Ordering::Equal => {
                        s1.elements =
                            Box::new(std::mem::take(&mut s1.elements).union(*s2.elements));
                    }
                };

                self.range = Range::new(
                    self.range.lo().min(rhs_range.lo()).clone(),
                    self.range.hi().max(rhs_range.hi()).clone(),
                );
            }
            (DynSetData::Contiguous, DynSetData::Contiguous) => {
                let s1 = self.range.clone();
                let s2 = rhs_range;
                self.range = Range::new(s1.lo().min(s2.lo()).clone(), s1.hi().max(s2.hi()).clone());
                // first check if the two ranges overlap - if so we can create a new contiguous range.
                if (s1.lo() < s2.hi() && s2.hi() < s1.hi())
                    || (s2.lo() < s1.hi() && s1.hi() < s2.hi())
                {

                    // nothing to do - we already updated the range
                } else {
                    // if they don't overlap convert into a stripe set.
                    self.data = DynSetData::Stripe(StripeSet::new(vec![s1.into(), s2.into()]));
                }
            }
            (DynSetData::Stripe(s1), DynSetData::Stripe(s2)) => {
                self.range = Range::new(
                    self.range.lo().min(rhs_range.lo()).clone(),
                    self.range.hi().max(rhs_range.hi()).clone(),
                );
                s1.union(s2);
            }

            (DynSetData::Stripe(s1), DynSetData::Contiguous) => {
                self.range = Range::new(
                    self.range.lo().min(rhs_range.lo()).clone(),
                    self.range.hi().max(rhs_range.hi()).clone(),
                );
                s1.add_range(rhs_range.into());
            }

            (DynSetData::Stripe(s1), DynSetData::Small(s2)) => {
                let mut little_stripe: StripeSet<usize> = StripeSet::new(vec![]);

                s2.elements.add_to_stripe(&mut little_stripe);
                for stripe in little_stripe.stripes() {
                    s1.add_range(StepRange::new(
                        s2.offset.clone() + I::from_usize(*stripe.lo()).unwrap(),
                        s2.offset.clone() + I::from_usize(*stripe.hi()).unwrap(),
                        I::from_usize(*stripe.step()).unwrap(),
                    ));
                }
                self.range = Range::new(
                    self.range.lo().min(rhs_range.lo()).clone(),
                    self.range.hi().max(rhs_range.hi()).clone(),
                );
            }
            (DynSetData::Small(small), DynSetData::Contiguous) => {
                if rhs_range.hi().clone() - &small.offset < DynSet::small_set_max_range()
                    && rhs_range.lo() >= &small.offset
                {
                    let small_range = Range::new(
                        (rhs_range.lo().clone() - &small.offset).to_usize().unwrap(),
                        (I::one() + rhs_range.hi() - &small.offset)
                            .to_usize()
                            .unwrap(),
                    );
                    small
                        .elements
                        .include_between(*small_range.lo(), *small_range.hi());
                } else if rhs_range.len_clone() < DynSet::small_set_max_range() {
                    if let Some(shift_needed) = (small.offset.clone() - rhs_range.lo()).to_isize() {
                        if shift_needed < 0
                            && small
                                .elements
                                .hi()
                                .map(|h| SMALL_SET_MAX_RANGE - h > (-shift_needed) as usize)
                                .unwrap_or(false)
                        {
                            small.elements.add_scalar(-shift_needed as usize);
                            small.offset = rhs_range.lo().clone();
                        } else if small
                            .elements
                            .lo()
                            .map(|l| l > shift_needed as usize)
                            .unwrap_or(false)
                        {
                            small.elements = Box::new(
                                std::mem::take(&mut small.elements)
                                    .arith_sub_scalar(shift_needed as usize),
                            );
                            small.offset = rhs_range.lo().clone();
                        } else {
                            self.upgrade_from_small();
                            return self.union(DynSet {
                                data: DynSetData::Contiguous,
                                range: rhs_range,
                            });
                        }
                    } else {
                        self.upgrade_from_small();
                        return self.union(DynSet {
                            data: DynSetData::Contiguous,
                            range: rhs_range,
                        });
                    }
                } else {
                    self.upgrade_from_small();
                    return self.union(DynSet {
                        data: DynSetData::Contiguous,
                        range: rhs_range,
                    });
                }
                self.range = Range::new(
                    self.range.lo().min(rhs_range.lo()).clone(),
                    self.range.hi().max(rhs_range.hi()).clone(),
                );
            }
            (DynSetData::Contiguous, data) => {
                let old_self = std::mem::replace(
                    self,
                    DynSet {
                        data,
                        range: rhs_range,
                    },
                );
                self.union(old_self);
            }
            a => todo!("impl union for {:?}", a),
        };
        self
    }
}

impl<'a, I: SetElement> ArithmeticSet<&'a Self, &'a I> for DynSet<I> {
    fn add_all(&mut self, rhs: &Self) {
        match (&mut self.data, &rhs.data) {
            // TODO: how do we define arithmetic with empty???
            (_, DynSetData::Empty) => (),
            (DynSetData::Empty, _data) => *self = rhs.clone(),

            (DynSetData::Small(s1), DynSetData::Small(s2)) => {
                if let Some(new_max_in_field) = ((self.range.hi().clone() - &s1.offset)
                    + (rhs.range.hi().clone() - &s2.offset))
                    .to_usize()
                {
                    if new_max_in_field < SMALL_SET_MAX_RANGE {
                        s1.offset = s2.offset.clone() + &s1.offset;
                        s1.elements =
                            Box::new(s1.elements.arith_add::<SMALL_SET_WORD_COUNT>(&s2.elements));
                        self.range = Range::new(
                            rhs.range.lo().clone() + self.range.lo(),
                            rhs.range.hi().clone() + self.range.hi(),
                        );
                        return;
                    }
                }

                self.upgrade_from_small();
                self.add_all(rhs);
            }
            (DynSetData::Small(_), DynSetData::Contiguous) => {
                let mut new_rhs = rhs.clone();
                new_rhs.upgrade_from_contiguous();
                self.add_all(&new_rhs);
            }
            (DynSetData::Small(_), DynSetData::Stripe(_stripe_set)) => {
                //    TODO: performance, we could effiecently add stripe here, if its small enough
                self.upgrade_from_small();
                self.add_all(rhs);
            }
            (DynSetData::Contiguous, DynSetData::Contiguous) => {
                self.range = rhs.range.clone() + self.range.clone();
            }
            (DynSetData::Contiguous, _) => {
                self.upgrade_from_contiguous();
                self.add_all(rhs);
            }
            (DynSetData::Stripe(s1), DynSetData::Contiguous) => {
                let rhs_stripe_set = StripeSet::new(vec![rhs.range.clone().into()]);
                *s1 = s1.arith_add(&rhs_stripe_set);
                self.range = s1.get_range().unwrap();
            }
            (DynSetData::Stripe(s1), DynSetData::Stripe(s2)) => {
                *s1 = s1.arith_add(&s2);
                self.range = s1.get_range().unwrap();
            }
            (DynSetData::Stripe(_), data) => {
                assert!(matches!(data, DynSetData::Small(_)));
                let mut new_rhs = rhs.clone();
                new_rhs.upgrade_from_small();
                self.add_all(&new_rhs);
            }
        };
    }

    fn mul_all(&mut self, rhs: &Self) {
        if self.is_empty() {
            return;
        }
        self.range = self.range.clone() * rhs.range.clone();
        self.data = DynSetData::Contiguous;
        // TODO: implement mul_all
    }

    fn add_scalar(&mut self, rhs: &I) {
        // TODO: we can implement this in a fairly performant way, actually.
        self.add_all(&DynSet::new_from_individual(&[rhs.clone().into()]));
    }

    fn mul_scalar(&mut self, rhs: &I) {
        // TODO: implement mul_scalar
        self.mul_all(&DynSet::new_from_individual(&[rhs.clone().into()]));
    }

    fn div_scalar(&mut self, rhs: &I) {
        if self.is_empty() {
            return;
        }

        let (lo, hi) = self.range.clone().into_tuple();
        self.range = Range::new(lo / rhs, hi / rhs);
        self.data = DynSetData::Contiguous;
    }

    fn mod_scalar(&mut self, rhs: &'a I) {
        match &mut self.data {
            DynSetData::Empty => (),
            DynSetData::Small(small_set) => {
                if &(Self::small_set_max_range() + &small_set.offset) < rhs {
                    // we don't have to change the set at all, its already entirely in the remainer
                } else {
                    // shift the set to the right offset:
                    // Baby proof:
                    //    let A = small_set.elements
                    //        O = small_set.offset
                    //        R = rhs
                    //    forall i : (Ai + O) mod rhs = Bi
                    //    modulo distributs, so: Bi = (Ai mod rhs) + (O mod rhs)

                    if let Some(shift_needed) = small_set.offset.mod_floor(rhs).to_usize() {
                        let bitmap_hi = small_set.elements.hi().expect(
                            "small bitmap was empty, this should be converted to the empty variant of a multiset",
                        );

                        if bitmap_hi + shift_needed < SMALL_SET_MAX_RANGE {
                            small_set.elements.add_scalar(shift_needed);
                            small_set
                                .elements
                                .mod_scalar(rhs.to_usize().unwrap_or_else(|| unreachable!()));
                            small_set.offset = I::zero();
                            return;
                        }
                    }

                    // if we were able to mod the bitfield, then we would have returned by this point
                    // now, try upgrading and re-doing the operation
                    self.upgrade_from_small();
                    self.mod_scalar(rhs);
                }
            }
            DynSetData::Contiguous => {
                if self.range.hi() < rhs {
                    // do nothing, we're already in the remainer
                } else if self.range.lo().is_zero() {
                    self.range =
                        Range::new(self.range.lo().clone(), self.range.hi().min(rhs).clone());
                } else if self.range.lo().mod_floor(rhs) < self.range.hi().mod_floor(rhs) {
                    // after taking mod rhs, we're still continuous!
                    self.range = Range::new(
                        self.range.lo().mod_floor(rhs),
                        self.range.hi().mod_floor(rhs),
                    );

                    // at this point we have to upgrade...
                } else if rhs < &Self::small_set_max_range() {
                    let mut new_field = Box::new(BitField::new());
                    new_field.include_between(
                        0,
                        self.range
                            .hi()
                            .mod_floor(rhs)
                            .to_usize()
                            .unwrap_or_else(|| unreachable!())
                            + 1,
                    );
                    new_field.include_between(
                        self.range
                            .lo()
                            .mod_floor(rhs)
                            .to_usize()
                            .unwrap_or_else(|| unreachable!())
                            + 1,
                        rhs.to_usize().unwrap_or_else(|| unreachable!()) - 1,
                    );
                    self.range = Range::new(I::zero(), rhs.clone() - I::one());
                    self.data = DynSetData::Small(SmallSet {
                        elements: new_field,
                        offset: I::zero(),
                    })
                } else {
                    let mut stripe = StripeSet::new(vec![self.range.clone().into()]);
                    stripe.modulo(rhs);
                    self.range = stripe.get_range().expect(
                        "modulo should never result in an empty set, this is a stripe set bug.",
                    );
                    self.data = DynSetData::Stripe(stripe);
                }
            }
            DynSetData::Stripe(stripe_set) => {
                stripe_set.modulo(rhs);
                self.range = stripe_set.get_range().expect(
                    "modulo should never result in an empty set, this is a stripe set bug.",
                );
            }
        }
    }

    fn sub_all(&mut self, rhs: &'a Self) {
        match (&mut self.data, &rhs.data) {
            // TODO: how do we define arithmetic with empty???
            // TODO: quickly adapted this from add_all, needs tests
            (_, DynSetData::Empty) => (),
            (DynSetData::Empty, _data) => *self = rhs.clone(),

            (DynSetData::Small(s1), DynSetData::Small(s2)) => {
                if let Some(new_max_in_field) = ((self.range.hi().clone() - &s1.offset)
                    - (rhs.range.hi().clone() - &s2.offset))
                    .to_usize()
                {
                    if new_max_in_field < SMALL_SET_MAX_RANGE {
                        s1.offset = s2.offset.clone() - &s1.offset;
                        s1.elements =
                            Box::new(s1.elements.arith_sub::<SMALL_SET_WORD_COUNT>(&s2.elements));
                        self.range = Range::new(
                            rhs.range.lo().clone() + self.range.lo(),
                            rhs.range.hi().clone() + self.range.hi(),
                        );
                        return;
                    }
                }

                self.upgrade_from_small();
                self.sub_all(rhs);
            }
            (DynSetData::Small(_), DynSetData::Contiguous) => {
                let mut new_rhs = rhs.clone();
                new_rhs.upgrade_from_contiguous();
                self.sub_all(&new_rhs);
            }
            (DynSetData::Small(_), DynSetData::Stripe(_stripe_set)) => {
                //    TODO: performance, we could effiecently add stripe here, if its small enough
                self.upgrade_from_small();
                self.sub_all(rhs);
            }
            (DynSetData::Contiguous, DynSetData::Contiguous) => {
                self.range = self.range.clone() - rhs.range.clone();
            }
            (DynSetData::Contiguous, _) => {
                self.upgrade_from_contiguous();
                self.sub_all(rhs);
            }
            (DynSetData::Stripe(s1), DynSetData::Contiguous) => {
                let rhs_stripe_set = StripeSet::new(vec![rhs.range.clone().into()]);
                *s1 = s1.arith_sub(&rhs_stripe_set);
                self.range = s1.get_range().unwrap();
            }
            (DynSetData::Stripe(s1), DynSetData::Stripe(s2)) => {
                *s1 = s1.arith_sub(&s2);
                self.range = s1.get_range().unwrap();
            }
            (DynSetData::Stripe(_), data) => {
                assert!(matches!(data, DynSetData::Small(_)));
                let mut new_rhs = rhs.clone();
                new_rhs.upgrade_from_small();
                self.sub_all(&new_rhs);
            }
        };
    }

    fn div_all(&mut self, rhs: &'a Self) {
        if self.is_empty() {
            return;
        }
        self.range = self.range.clone() / rhs.range.clone();
        self.data = DynSetData::Contiguous;
        // TODO: implement div_all
    }
}

impl<I: SetElement> DynSet<I> {
    pub fn exclude_below(&mut self, n: &I) {
        if n > self.range.hi() {
            self.data = DynSetData::Empty;
        } else {
            match &mut self.data {
                DynSetData::Empty => (),
                DynSetData::Small(small) => {
                    if let Some(first_el_to_remove) =
                        (n.clone() - &small.offset - I::one()).to_usize()
                    {
                        if first_el_to_remove >= SMALL_SET_MAX_RANGE {
                            // this should be unreachable due to the above case
                            // it may be worth warning here, since something is definitely wrong...
                            self.data = DynSetData::Empty;
                        } else {
                            small.elements.exclude_range(0, first_el_to_remove);
                            if let Some(r) = small.elements.range() {
                                self.range = Range::new(
                                    small.offset.clone() + I::from_usize(*r.lo()).unwrap(),
                                    small.offset.clone() + I::from_usize(*r.hi()).unwrap(),
                                );
                            } else {
                                self.data = DynSetData::Empty;
                            }
                        }
                    }
                }
                DynSetData::Contiguous => {
                    self.range =
                        Range::new(self.range.lo().max(n).clone(), self.range.hi().clone());
                }
                DynSetData::Stripe(stripe) => {
                    stripe.exclude_below(n);
                    self.range = stripe.get_range().unwrap()
                }
            }
        }
    }

    pub fn exclude_above(&mut self, n: &I) {
        if n < self.range.lo() {
            self.data = DynSetData::Empty;
        } else {
            match &mut self.data {
                DynSetData::Empty => (),
                DynSetData::Small(small) => {
                    if let Some(field_n) = (n.clone() - &small.offset + I::one()).to_usize() {
                        if field_n < SMALL_SET_MAX_RANGE {
                            small.elements.exclude_range(field_n, SMALL_SET_MAX_RANGE);
                            if let Some(r) = small.elements.range() {
                                self.range = Range::new(
                                    small.offset.clone() + I::from_usize(*r.lo()).unwrap(),
                                    small.offset.clone() + I::from_usize(*r.hi()).unwrap(),
                                );
                            } else {
                                self.data = DynSetData::Empty;
                            }
                        }
                    }
                }
                DynSetData::Contiguous => {
                    self.range =
                        Range::new(self.range.lo().clone(), self.range.hi().min(n).clone());
                }
                DynSetData::Stripe(stripe) => {
                    stripe.exclude_above(n);
                    if let Some(r) = stripe.get_range() {
                        self.range = r;
                    } else {
                        self.data = DynSetData::Empty;
                    }
                }
            }
        }
    }
}

impl<I: SetElement + Neg<Output = I>> DynSet<I> {
    pub fn neg_mut(&mut self) {
        match &mut self.data {
            DynSetData::Empty => (),
            DynSetData::Small(s) => {
                s.offset = -s.offset.clone() - I::from_usize(SMALL_SET_MAX_RANGE).unwrap();
                s.elements.reverse();
            }
            DynSetData::Contiguous => {
                let (lo, hi) = self.range.clone().into_tuple();
                self.range = Range::new(-hi, -lo);
            }
            DynSetData::Stripe(s) => s.neg_mut(),
        }
    }
}

impl<I: SetElement> PartialBounded<I> for DynSet<I> {
    fn partial_lo(&self) -> Option<&I> {
        match self.data {
            DynSetData::Empty => None,
            _ => Some(self.range.lo()),
        }
    }

    fn partial_hi(&self) -> Option<&I> {
        match self.data {
            DynSetData::Empty => None,
            _ => Some(self.range.hi()),
        }
    }
}

impl<I: SetElement> Set for DynSet<I> {
    type ElementT = I;
}

impl<I: SetElement> SetOpIncludes<I> for DynSet<I> {
    fn includes(&self, element: I) -> bool {
        match &self.data {
            DynSetData::Empty => false,
            DynSetData::Small(s) => s.elements.includes(match (element - &s.offset).to_usize() {
                Some(v) => v,
                None => return false,
            }),
            DynSetData::Contiguous => self.range.includes(element),
            DynSetData::Stripe(s) => s.includes(element),
        }
    }
}

impl<'a, I: SetElement> SetOpIncludes<&'a I> for DynSet<I> {
    fn includes(&self, element: &'a I) -> bool {
        match &self.data {
            DynSetData::Empty => false,
            DynSetData::Small(s) => {
                s.elements
                    .includes(match (element.clone() - &s.offset).to_usize() {
                        Some(v) => v,
                        None => return false,
                    })
            }
            DynSetData::Contiguous => self.range.includes(element),
            DynSetData::Stripe(s) => s.includes(element),
        }
    }
}

impl<'a, I: SetElement> Subset<&'a DynSet<I>> for &'a DynSet<I> {
    fn subset_of(self, rhs: Self) -> bool {
        if !matches!(self.data, DynSetData::Empty) && !self.range.subset_of(&rhs.range) {
            return false;
        }

        match (&self.data, &rhs.data) {
            (_, DynSetData::Empty) => true,
            (DynSetData::Empty, _) => false,
            // we already checked range subset
            (_, DynSetData::Contiguous) => true,
            (DynSetData::Small(s1), DynSetData::Small(s2)) => {
                // since we know they have the similar ranges, if we can access the underlying
                // bit field we can probably do this way, way, faster
                match s1.offset.cmp(&s2.offset) {
                    Ordering::Less => s1
                        .elements
                        .clone()
                        .arith_add_scalar((s2.offset.clone() - &s1.offset).to_usize().unwrap())
                        .subset_of(&s2.elements),
                    Ordering::Equal => s1.elements.subset_of(&s2.elements),
                    Ordering::Greater => s1
                        .elements
                        .clone()
                        .arith_sub_scalar((s1.offset.clone() - &s2.offset).to_usize().unwrap())
                        .subset_of(&s2.elements),
                }
            }
            (DynSetData::Contiguous, DynSetData::Small(small_set)) => {
                small_set.elements.includes_range(Range::new(
                    (rhs.range.lo().clone() - &small_set.offset)
                        .to_usize()
                        .unwrap(),
                    (rhs.range.hi().clone() - &small_set.offset)
                        .to_usize()
                        .unwrap(),
                ))
            }
            (DynSetData::Small(small_set), DynSetData::Stripe(stripe_set)) => {
                for n in small_set.elements.iter_values_from(0, 0) {
                    let val = I::from_usize(n).unwrap() + &small_set.offset;
                    if !stripe_set.includes(val) {
                        return false;
                    }
                }

                true
            }
            (DynSetData::Stripe(stripe_set), DynSetData::Small(small_set)) => {
                stripe_set.stripes().all(|a| {
                    match (
                        (a.lo().clone() - &small_set.offset).to_usize(),
                        (a.hi().clone() - &small_set.offset).to_usize(),
                        a.step().to_usize(),
                    ) {
                        (Some(lo), Some(hi), Some(step)) => small_set
                            .elements
                            .includes_step_range(StepRange::new(lo, hi, step)),
                        _ => false,
                    }
                })
            }
            (DynSetData::Stripe(s1), DynSetData::Stripe(s2)) => s1.subset_of(s2),
            (DynSetData::Contiguous, DynSetData::Stripe(s2)) => {
                // I have a deadline.
                StripeSet::new(vec![self.range.clone().into()]).subset_of(s2)
            }
        }
    }

    fn strict_subset_of(self, _rhs: Self) -> bool {
        todo!()
    }
}

impl<I: SetElement> PartialOrd for DynSet<I> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.partial_bounds(), other.partial_bounds()) {
            (Some(l), Some(r)) if l.hi() < r.lo() => Some(Ordering::Less),
            (Some(l), Some(r)) if l.lo() > r.hi() => Some(Ordering::Greater),
            (Some(l), Some(r)) if l.lo() == r.lo() && l.hi() == r.hi() => {
                if self.eq(other) {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl<I: SetElement> Eq for DynSet<I> {}
impl<I: SetElement> PartialEq for DynSet<I> {
    fn eq(&self, other: &Self) -> bool {
        self.subset_of(other) && other.subset_of(self)
    }
}

impl<I: SetElement> SetOpIncludeExclude<I> for DynSet<I> {
    type Output = Self;

    fn include(mut self, element: I) -> Self::Output {
        self.include_mut(element);
        self
    }

    #[instrument(level=Level::TRACE)]
    fn include_mut(&mut self, element: I) {
        match &mut self.data {
            DynSetData::Empty => {
                self.data = DynSetData::Contiguous;
                self.range = Range::new(element.clone(), element);
            }
            DynSetData::Small(small_set) => {
                let max_range = I::from_usize(SMALL_SET_MAX_RANGE).unwrap();

                if element < small_set.offset || (element.clone() - &small_set.offset) > max_range {
                    if let Some(r) = small_set.elements.range() {
                        let local_lo = I::from_usize(*r.lo()).unwrap();
                        let local_hi = I::from_usize(*r.hi()).unwrap();

                        let abs_hi = local_hi.clone() + &small_set.offset;

                        let can_shift_up = local_lo.clone();
                        let can_shift_down = max_range - &local_hi - I::one();

                        if element > abs_hi && element <= abs_hi + &can_shift_up {
                            small_set.elements = Box::new(
                                small_set
                                    .elements
                                    .clone()
                                    .arith_sub_scalar(can_shift_up.to_usize().unwrap()),
                            );
                            small_set.offset = small_set.offset.clone() + can_shift_up;
                            small_set
                                .elements
                                .include_mut((element - &small_set.offset).to_usize().unwrap());
                            return;
                        } else if element < small_set.offset
                            && element >= small_set.offset.clone() - &can_shift_down
                        {
                            let min_shift_down = small_set.offset.clone() - &element;
                            small_set.elements = Box::new(
                                small_set
                                    .elements
                                    .clone()
                                    .arith_add_scalar(min_shift_down.to_usize().unwrap()),
                            );
                            small_set.offset = small_set.offset.clone() - &min_shift_down;

                            small_set.elements = Box::new(
                                small_set
                                    .elements
                                    .clone()
                                    .include((element - &small_set.offset).to_usize().unwrap()),
                            );
                            return;
                        }
                    } else {
                        // set is empty, just include the new value.
                        // typically this case shouldn't happen, since these sets are basically always non-empty.
                        // But, it doesn't hurt to handle in a sane way...
                        small_set.offset = element;
                        small_set.elements.include_mut(0);
                        return;
                    }

                    self.upgrade_from_small();
                    self.include_mut(element);
                } else {
                    small_set.elements = Box::new(
                        small_set
                            .elements
                            .clone()
                            .include((element - &small_set.offset).to_usize().unwrap()),
                    );
                }
            }
            DynSetData::Contiguous => {
                if !self.range.includes(element.clone()) {
                    self.upgrade_from_contiguous();
                    // make really, really, sure we won't recurse forever...
                    assert!(!matches!(self.data, DynSetData::Contiguous));
                    self.include_mut(element);
                }
            }
            DynSetData::Stripe(stripe_set) => {
                stripe_set.include_mut(element);
            }
        }
    }

    fn exclude_mut(&mut self, element: &I) {
        match &mut self.data {
            DynSetData::Empty => (),
            DynSetData::Small(small) => {
                let offset_element = element.clone() - &small.offset;
                if let Some(offset_element_usize) = offset_element.to_usize() {
                    if offset_element_usize < SMALL_SET_MAX_RANGE {
                        small.elements.exclude_mut(&offset_element_usize);
                        if let Some(bounds) = small.elements.range() {
                            self.range = Range::new(
                                I::from_usize(*bounds.lo()).unwrap() + &small.offset,
                                I::from_usize(*bounds.hi()).unwrap() + &small.offset,
                            );
                        } else {
                            self.data = DynSetData::Empty
                        }
                    }
                }
            }
            DynSetData::Contiguous => todo!("DynSet::exclude_mut(): when contiguous"),
            DynSetData::Stripe(stripe) => {
                stripe.exclude_mut(element);
            }
        }
    }
}

impl<'a, I: SetElement> IntersectMut<&'a Self> for DynSet<I> {
    fn intersect_mut(&mut self, rhs: &'a Self) {
        // all lhs elements not in rhs
        let mut excl_l = self.clone();
        excl_l.set_subtract_mut(rhs);

        // all rhs elements that are not in lhs
        let mut excl_r = rhs.clone();
        excl_r.set_subtract_mut(self);

        self.set_subtract_mut(&excl_l);
        self.set_subtract_mut(&excl_r);
    }
}

impl<'a, I: SetElement> SetSubtract<&'a Self> for DynSet<I> {
    fn set_subtract_mut(&mut self, rhs: &'a Self) {
        match (&mut self.data, &rhs.data) {
            (_, DynSetData::Empty) => (),
            (DynSetData::Empty, _) => (),
            (DynSetData::Small(s1), DynSetData::Small(s2)) => {
                let dist = s2.offset.clone() - &s1.offset;
                if dist.is_zero() {
                    s1.elements.set_subtract_mut(&s2.elements)
                } else if let Some(isize_dist) = dist.to_isize() {
                    if isize_dist.unsigned_abs() < SMALL_SET_MAX_RANGE {
                        s1.elements
                            .set_subtract_mut_with_offset(&s2.elements, isize_dist);
                    }
                };
            }
            (DynSetData::Small(s1), DynSetData::Contiguous) => {
                let start = (s1.offset.clone() - rhs.range.lo()).to_usize();
                let end = (s1.offset.clone() - rhs.range.hi()).to_usize();
                if let (Some(start), Some(end)) = (start, end) {
                    if start <= SMALL_SET_MAX_RANGE {
                        s1.elements
                            .exclude_range(start, end.min(SMALL_SET_MAX_RANGE));
                    }
                }
            }
            (DynSetData::Small(s1), DynSetData::Stripe(s2)) => {
                for range in s2.stripes() {
                    if self.range.includes(range.lo())
                        || self.range.includes(range.hi())
                        || (self.range.lo() >= range.lo() && self.range.hi() <= range.hi())
                    {
                        // TODO: this won't work if there's a HUGE step that happens to land within s1, because `range` is very, very large.
                        //       I don't know if this case is practically possible though, so ignoring for now.
                        if let Some(s) = range.step().to_usize() {
                            let usize_range = StepRange::new(
                                (range.lo().clone() - &s1.offset)
                                    .max(I::zero())
                                    .to_usize()
                                    .unwrap(),
                                (range.hi().clone() - &s1.offset)
                                    .min(I::from_usize(SMALL_SET_MAX_RANGE).unwrap())
                                    .to_usize()
                                    .unwrap(),
                                s,
                            );
                            s1.elements.set_subtract_step_range(usize_range);
                        }
                    }
                }
            }
            (DynSetData::Contiguous, DynSetData::Small(_) | DynSetData::Stripe(_)) => {
                self.upgrade_from_contiguous();
                self.set_subtract_mut(rhs);
            }
            (DynSetData::Contiguous, DynSetData::Contiguous) => {
                let split = self.range.clone().remove_range(rhs.range.clone());
                match split {
                    Some((l, Some(r))) => {
                        self.data = DynSetData::Stripe(StripeSet::new(vec![l.into(), r.into()]))
                    }
                    Some((l, None)) => self.range = l,
                    None => self.data = DynSetData::Empty,
                }
            }
            (DynSetData::Stripe(_s1), DynSetData::Small(_s2)) => todo!(),
            (DynSetData::Stripe(s1), DynSetData::Contiguous) => {
                // TODO: impl StripeSet.substract_range()
                s1.set_subtract_mut(&StripeSet::new(vec![rhs.range.clone().into()]));
                match s1.get_range() {
                    Some(r) => self.range = r,
                    None => self.data = DynSetData::Empty,
                }
            }
            (DynSetData::Stripe(s1), DynSetData::Stripe(s2)) => {
                s1.set_subtract_mut(s2);
                match s1.get_range() {
                    Some(r) => self.range = r,
                    None => self.data = DynSetData::Empty,
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    mod subset {
        use crate::{multi::DynSet, ops::Subset};

        #[test]
        fn cont_stripe() {
            let a = DynSet::new_from_range(-10i64, 25i64);
            let b = DynSet::new_from_tuples(&[(0, 5), (20, 25)]);
            assert!(!a.subset_of(&b));
            assert!(b.subset_of(&a));
        }
    }
}
