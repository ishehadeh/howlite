use std::{cell::RefCell, cmp::Ordering};

use num_prime::buffer::NaiveBuffer;

use crate::{
    bitfield::BitField,
    ops::{Bounded, Union},
    range::Range,
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
}

#[derive(Debug, Clone)]
enum DynSetData<I: SetElement> {
    Small(SmallSet<I>),
    Contiguous(Contiguous<I>),
    Stripe(StripeSet<I>),
}

#[derive(Debug, Clone)]
struct Contiguous<I: SetElement> {
    range: Range<I>,
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
            data: DynSetData::Contiguous(Contiguous {
                range: Range::new(lo, hi),
            }),
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
            (DynSetData::Small(s1), DynSetData::Small(s2)) => {
                match (s1.elements.range(), s2.elements.range()) {
                    (None, _) => DynSetData::Small(s2),
                    (_, None) => DynSetData::Small(s1),
                    (Some(s1_range), Some(s2_range)) => match s1.offset.cmp(&s2.offset) {
                        Ordering::Less => {
                            let offset = (&s2.offset - &s1.offset).to_usize().expect("bitfield offset during union too large, this should be unreachable");
                            if offset + s2_range.hi() < SMALL_SET_MAX_RANGE {
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
                            let offset = (&s1.offset - &s2.offset).to_usize().expect("bitfield offset during union too large, this should be unreachable");
                            if offset + s1_range.hi() < SMALL_SET_MAX_RANGE {
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
                    },
                }
            }
            (DynSetData::Contiguous(s1), DynSetData::Contiguous(s2)) => {
                // first check if the two ranges overlap - if so we can create a new contiguous range.
                if s1.range.lo() >= s2.range.hi() {
                    DynSetData::Contiguous(Contiguous {
                        range: Range::new(s1.range.into_tuple().0, s2.range.into_tuple().1),
                    })
                } else if s2.range.lo() >= s1.range.hi() {
                    DynSetData::Contiguous(Contiguous {
                        range: Range::new(s2.range.into_tuple().0, s1.range.into_tuple().1),
                    })
                } else {
                    // if they don't overlap convert into a stripe set.
                    DynSetData::Stripe(StripeSet::new(vec![s1.range.into(), s2.range.into()]))
                }
            }
            (DynSetData::Stripe(s1), DynSetData::Stripe(s2)) => DynSetData::Stripe(s1.union(s2)),
            _ => todo!(),
        };

        Self { data }
    }
}

#[test]
fn dyn_union() {
    let a = DynSet::new_from_individual(&[1, 5, 11, 1000, 1024]);
    dbg!(&a);
    let b = DynSet::new_from_individual(&[3, 8, 13, 100, 200]);
    dbg!(&b);
    panic!("{:?}", a.union(b));
}
