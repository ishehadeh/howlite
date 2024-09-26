use std::cell::RefCell;

use num_prime::{buffer::NaiveBuffer, detail::PrimalityRefBase};

use crate::{bitfield::BitField, range::Range, stripeset::StripeSet, FactorSet, SetElement};

const SMALL_SET_WORD_COUNT: usize = 128;
const SMALL_SET_MAX_RANGE: usize = SMALL_SET_WORD_COUNT * 64;

thread_local! {
    static LOCAL_PRIME_BUFFER: RefCell<NaiveBuffer> = RefCell::new(NaiveBuffer::new());
}

#[derive(Debug, Clone)]
/// A DynSet uses multiple representations to track its elements as effeciently as possible
pub struct DynSet<I: SetElement>
where
    for<'r> &'r I: PrimalityRefBase<I>,
{
    data: DynSetData<I>,
}

#[derive(Debug, Clone)]
enum DynSetData<I: SetElement>
where
    for<'r> &'r I: PrimalityRefBase<I>,
{
    Small(SmallSet<I>),
    Contiguous(Contiguous<I>),
    Stripe(StripeSet<I>),
    Factor(FactorSet<'static, I, NaiveBuffer>),
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
    for<'r> &'r I: PrimalityRefBase<I>,
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
                for (i, x) in slice.iter().enumerate() {
                    usize_slice[i] = (x - min)
                        .to_usize()
                        .expect("failed to convert element to usize");
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
