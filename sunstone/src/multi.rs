use std::{cell::RefCell, cmp::Ordering};

use num_prime::buffer::NaiveBuffer;

use crate::{
    bitfield::BitField,
    ops::{
        ArithmeticSet, Bounded, PartialBounded, Set, SetOpIncludeExclude, SetOpIncludes, Subset,
        Union,
    },
    range::Range,
    step_range::StepRange,
    stripeset::StripeSet,
    SetElement,
};

const SMALL_SET_WORD_COUNT: usize = 128;
const SMALL_SET_MAX_RANGE: usize = SMALL_SET_WORD_COUNT * 64;

thread_local! {
    static LOCAL_PRIME_BUFFER: RefCell<NaiveBuffer> = RefCell::new(NaiveBuffer::new());
}

#[derive(Debug, Clone)]
/// A DynSet uses multiple representations to track its elements as effeciently as possible
pub struct DynSet<I: SetElement> {
    data: DynSetData<I>,
    range: Range<I>,
}

#[derive(Debug, Clone)]
enum DynSetData<I: SetElement> {
    Empty,
    Small(SmallSet<I>),
    Contiguous,
    Stripe(StripeSet<I>),
}

#[derive(Debug, Clone)]
struct SmallSet<I: SetElement> {
    elements: Box<BitField<SMALL_SET_MAX_RANGE>>,
    offset: I,
}

impl<I: SetElement> DynSet<I> {
    pub fn new_from_range(lo: I, hi: I) -> DynSet<I> {
        DynSet {
            data: DynSetData::Contiguous,
            range: Range::new(lo, hi),
        }
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
        if let Some(len_usize) = self.range.len().to_usize() {
            if len_usize < SMALL_SET_MAX_RANGE {
                let offset = self.range.lo().clone();
                self.data = DynSetData::Small(SmallSet {
                    elements: {
                        let mut set = Box::new(BitField::new());
                        set.include_between(0, len_usize);
                        set
                    },
                    offset,
                })
            }
        } else {
            self.data = DynSetData::Stripe(StripeSet::new(vec![self.range.clone().into()]));
        }
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
                            I::from_usize(*s.step()).unwrap() + &small_set.offset,
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

impl<I: SetElement> Union<DynSet<I>> for DynSet<I>
where
    for<'a> &'a I: std::ops::Sub<&'a I, Output = I>,
{
    type Output = DynSet<I>;

    fn union(self, rhs: DynSet<I>) -> Self::Output {
        let data = match (self.data, rhs.data) {
            // one set completely contained within the other
            (_, DynSetData::Contiguous)
                if self.range.lo() >= rhs.range.lo() && self.range.hi() <= rhs.range.hi() =>
            {
                DynSet {
                    data: DynSetData::Contiguous,
                    range: rhs.range,
                }
            }
            (DynSetData::Contiguous, _)
                if self.range.lo() < rhs.range.lo() && self.range.hi() > rhs.range.hi() =>
            {
                DynSet {
                    data: DynSetData::Contiguous,
                    range: self.range,
                }
            }

            (DynSetData::Small(s1), DynSetData::Small(s2)) => {
                let data = match s1.offset.cmp(&s2.offset) {
                    Ordering::Less => {
                        let offset = (&s2.offset - &s1.offset).to_usize().expect(
                            "bitfield offset during union too large, this should be unreachable",
                        );
                        if offset + rhs.range.hi().to_usize().unwrap() < SMALL_SET_MAX_RANGE {
                            DynSetData::Small(SmallSet {
                                elements: Box::new(
                                    s2.elements.arith_add_scalar(offset).union(*s1.elements),
                                ),
                                offset: s1.offset,
                            })
                        } else {
                            todo!()
                        }
                    }

                    Ordering::Greater => {
                        let offset = (&s1.offset - &s2.offset).to_usize().expect(
                            "bitfield offset during union too large, this should be unreachable",
                        );
                        if offset + self.range.hi().to_usize().unwrap() < SMALL_SET_MAX_RANGE {
                            DynSetData::Small(SmallSet {
                                elements: Box::new(
                                    s1.elements.arith_add_scalar(offset).union(*s2.elements),
                                ),
                                offset: s2.offset,
                            })
                        } else {
                            todo!()
                        }
                    }

                    Ordering::Equal => DynSetData::Small(SmallSet {
                        elements: Box::new(s2.elements.union(*s1.elements)),
                        offset: s2.offset,
                    }),
                };

                DynSet {
                    data,
                    range: Range::new(
                        self.range.lo().min(rhs.range.lo()).clone(),
                        self.range.hi().max(rhs.range.hi()).clone(),
                    ),
                }
            }
            (DynSetData::Contiguous, DynSetData::Contiguous) => {
                let s1 = self.range;
                let s2 = rhs.range;
                // first check if the two ranges overlap - if so we can create a new contiguous range.
                if s1.lo() < s2.hi() && s1.hi() > s2.hi() {
                    Self {
                        data: DynSetData::Contiguous,
                        range: Range::new(s2.into_tuple().0, s1.into_tuple().1),
                    }
                } else if s2.lo() < s1.hi() && s2.hi() > s1.hi() {
                    Self {
                        data: DynSetData::Contiguous,
                        range: Range::new(s1.into_tuple().0, s2.into_tuple().1),
                    }
                } else {
                    // if they don't overlap convert into a stripe set.
                    Self {
                        range: Range::new(
                            s1.lo().min(s2.lo()).clone(),
                            s1.hi().max(s2.hi()).clone(),
                        ),
                        data: DynSetData::Stripe(StripeSet::new(vec![s1.into(), s2.into()])),
                    }
                }
            }
            (DynSetData::Stripe(s1), DynSetData::Stripe(s2)) => Self {
                range: Range::new(
                    self.range.lo().min(rhs.range.lo()).clone(),
                    self.range.hi().max(rhs.range.hi()).clone(),
                ),
                data: DynSetData::Stripe(s1.union(s2)),
            },

            (DynSetData::Stripe(mut s1), DynSetData::Contiguous) => Self {
                range: Range::new(
                    self.range.lo().min(rhs.range.lo()).clone(),
                    self.range.hi().max(rhs.range.hi()).clone(),
                ),
                data: DynSetData::Stripe({
                    s1.add_range(rhs.range.into());
                    s1
                }),
            },

            (DynSetData::Stripe(mut s1), DynSetData::Small(s2)) => {
                let mut little_stripe: StripeSet<usize> = StripeSet::new(vec![]);

                s2.elements.add_to_stripe(&mut little_stripe);
                for stripe in little_stripe.stripes() {
                    s1.add_range(StepRange::new(
                        s2.offset.clone() + I::from_usize(*stripe.lo()).unwrap(),
                        s2.offset.clone() + I::from_usize(*stripe.hi()).unwrap(),
                        I::from_usize(*stripe.step()).unwrap(),
                    ));
                    dbg!(&s1);
                }
                Self {
                    range: Range::new(
                        self.range.lo().min(rhs.range.lo()).clone(),
                        self.range.hi().max(rhs.range.hi()).clone(),
                    ),
                    data: DynSetData::Stripe(s1),
                }
            }

            _ => todo!(),
        };
        data
    }
}

impl<I: SetElement> ArithmeticSet for DynSet<I> {
    fn add_all(&mut self, rhs: Self) {
        self.range = self.range.clone() + rhs.range;
    }

    fn mul_all(&mut self, rhs: Self) {
        self.range = self.range.clone() * rhs.range;
    }

    fn add_scalar(&mut self, rhs: <Self as Set>::ElementT) {
        todo!()
    }

    fn mul_scalar(&mut self, rhs: <Self as Set>::ElementT) {
        todo!()
    }
}

impl<I: SetElement> DynSet<I> {
    pub fn exclude_below(&mut self, n: &I) {
        if n > self.range.hi() {
            self.data = DynSetData::Empty
        } else if n > self.range.lo() {
            self.range = Range::new(n.clone(), self.range.hi().clone());
        }
    }

    pub fn exclude_above(&mut self, n: &I) {
        if n < self.range.lo() {
            self.data = DynSetData::Empty
        } else if n < self.range.hi() {
            self.range = Range::new(self.range.lo().clone(), n.clone());
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
            (DynSetData::Contiguous, _) => true,
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
            (DynSetData::Small(small_set), DynSetData::Contiguous) => {
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
                for range in stripe_set.stripes() {
                    let has_range = small_set.elements.includes_step_range(StepRange::new(
                        (range.lo().clone() - &small_set.offset).to_usize().unwrap(),
                        (range.hi().clone() - &small_set.offset).to_usize().unwrap(),
                        (range.step().clone() - &small_set.offset)
                            .to_usize()
                            .unwrap(),
                    ));
                    if !has_range {
                        return false;
                    }
                }

                true
            }
            (DynSetData::Stripe(stripe_set), DynSetData::Small(small_set)) => todo!(),
            (DynSetData::Stripe(stripe_set), DynSetData::Contiguous) => todo!(),
            (DynSetData::Stripe(s1), DynSetData::Stripe(s2)) => s1.subset_of(s2),
        }
    }

    fn strict_subset_of(self, rhs: Self) -> bool {
        todo!()
    }
}

impl<I: SetElement> SetOpIncludeExclude<I> for DynSet<I> {
    type Output = Self;

    fn include(self, element: I) -> Self::Output {
        todo!()
    }

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
                        let lo = I::from_usize(*r.lo()).unwrap();
                        let hi = I::from_usize(*r.hi()).unwrap();
                        if lo.clone() + &small_set.offset + &max_range > element
                            && element > lo.clone() + &small_set.offset
                        {
                            small_set.elements =
                                Box::new(small_set.elements.clone().arith_sub_scalar(*r.lo()));
                            small_set.offset = small_set.offset.clone() + lo;
                            small_set
                                .elements
                                .include_mut((element - &small_set.offset).to_usize().unwrap());
                            return;
                        } else if small_set.offset.clone() - &max_range - &hi <= element
                            && element < small_set.offset.clone() + &max_range - &hi
                        {
                            small_set.elements =
                                Box::new(small_set.elements.clone().arith_add_scalar(
                                    (max_range.clone() - &hi).to_usize().unwrap(),
                                ));
                            small_set.offset = small_set.offset.clone() - (max_range - hi);

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
        todo!()
    }
}

#[test]
fn dyn_union() {
    let a = DynSet::new_from_individual(&[1, 5, 11, 1000, 1024]);
    dbg!(&a);
    let b = DynSet::new_from_individual(&[3, 8, 13, 100, 200]);
    dbg!(&b);

    let c: DynSet<i32> = DynSet::new_from_range(0, 100);
    let d: DynSet<i32> = DynSet::new_from_range(200, 400);
    dbg!(&c);
    let e = dbg!(c.union(d));
    panic!("{:?}", e.union(a));
}
