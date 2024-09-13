use std::ops::{Add, Mul, Sub};

use crate::ops::Subset;

pub struct StepRange<I: num_integer::Integer> {
    start: I,
    repetitions: I,
    step: I,
}

impl<I: num_integer::Integer> StepRange<I> {
    pub fn try_new(start: I, repetitions: I, step: I) -> Option<StepRange<I>> {
        if step <= I::zero() || repetitions <= I::zero() {
            None
        } else {
            Some(StepRange {
                start,
                repetitions,
                step,
            })
        }
    }

    pub fn new(start: I, repetitions: I, step: I) -> StepRange<I> {
        Self::try_new(start, repetitions, step).expect("invalid range parameters")
    }
}

impl<'a, I> Subset for &'a StepRange<I>
where
    I: num_integer::Integer,
    &'a I: Add<&'a I, Output = I> + Sub<&'a I, Output = I> + Mul<&'a I, Output = I>,
{
    #[inline]
    fn subset_of(self, rhs: Self) -> bool {
        // 1 - first make sure the superset spans over at least the start of this range
        if rhs.start > self.start {
            return false;
        }

        // 2 - check that the steps are compatible
        //  i.e: there exists N such that every N elements in RHS there exists an element in SELF.
        if !self.step.is_multiple_of(&rhs.step) {
            return false;
        }

        // 3 - make sure this superset spans over the end of this range
        if &rhs.repetitions * &rhs.step < &self.repetitions * &self.step {
            return false;
        }

        // 4 - check that ranges have a compatible offset
        //  i.e. they aren't striped. e.g. step=5 & start=1 isn't compatible with step=5 & start = 0
        let lo_offset = &self.start - &rhs.start;
        lo_offset.is_zero() || lo_offset.is_multiple_of(&rhs.step)
    }

    #[inline]
    fn strict_subset_of(self, rhs: Self) -> bool {
        self.start != rhs.start && self.repetitions != rhs.repetitions && self.subset_of(rhs)
    }
}

#[test]
fn simple() {
    let a = StepRange::new(0, 100, 1);
    let b = StepRange::new(0, 10, 5);
    assert!(!a.subset_of(&b));
    assert!(b.subset_of(&a));

    let c = StepRange::new(0, 20, 3);
    let d = StepRange::new(5, 3, 9);
    assert!(!d.subset_of(&c));

    let e = StepRange::new(3, 3, 9);
    assert!(e.subset_of(&c));
}
