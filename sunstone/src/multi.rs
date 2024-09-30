use std::{cell::RefCell, cmp::Ordering};

use num_prime::buffer::NaiveBuffer;

use crate::{
    bitfield::BitField,
    ops::{Bounded, RingOps, Union},
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
    Small(SmallSet<I>),
    Contiguous,
    Stripe(StripeSet<I>),
}

#[derive(Debug, Clone)]
struct SmallSet<I: SetElement> {
    elements: Box<BitField<SMALL_SET_MAX_RANGE>>,
    offset: I,
}

impl<I: SetElement> DynSet<I>
where
    for<'a> &'a I: std::ops::Sub<&'a I, Output = I>,
{
    pub fn new_from_range(lo: I, hi: I) -> DynSet<I> {
        DynSet {
            data: DynSetData::Contiguous,
            range: Range::new(lo, hi),
        }
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
            if max - min < max_range {
                let mut usize_slice = Vec::with_capacity(slice.len());

                for x in slice.iter() {
                    usize_slice.push(
                        (x - min)
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

                s2.elements.add_to_stripe::<usize>(&mut little_stripe);
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

impl<I: SetElement> RingOps for DynSet<I> {
    fn ring_add(self, rhs: Self) -> Self {
        Self {
            data: rhs.data, // TODO!!
            range: self.range + rhs.range,
        }
    }

    fn ring_mul(self, rhs: Self) -> Self {
        Self {
            data: rhs.data, // TODO!!
            range: self.range * rhs.range,
        }
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
