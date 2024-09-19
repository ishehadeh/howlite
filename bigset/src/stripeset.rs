use std::{cmp::Ordering, marker::PhantomData, ops::RemAssign};

use bumpalo::Bump;
use num_integer::Integer;

use crate::{
    ops::{Bounded, Set, Subset, Union},
    range::Range,
    step_range::StepRange,
};

#[derive(Clone, Debug)]
pub struct StripeSet<I: Integer + Clone> {
    ranges: Vec<StepRange<I>>,
}

pub struct StripeSetBoundedIter<'a, I, B, I2>
where
    I: Integer + Clone + PartialOrd<I2>,
    B: Bounded<I2>,
    I2: PartialOrd<I> + Eq,
{
    _i2: PhantomData<I2>,
    bounds: B,
    index: usize,
    set: &'a StripeSet<I>,

    /// true to return all overlapping ranges, even if not a subset of `bounds``
    partial: bool,
}

impl<'a, I, B, I2> Iterator for StripeSetBoundedIter<'a, I, B, I2>
where
    I: Integer + Clone + PartialOrd<I2> + std::fmt::Debug,
    B: Bounded<I2>,
    I2: PartialOrd<I> + Eq,
{
    type Item = &'a StepRange<I>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.set.ranges.len() {
            return None;
        }

        let curr = &self.set.ranges[self.index];
        if curr.hi() <= self.bounds.lo() {
            return None;
        }
        if curr.hi() < self.bounds.hi() && !self.partial {
            return None;
        }

        self.index += 1;
        // curr completely within bounds

        if curr.lo() >= self.bounds.lo() && curr.hi() >= self.bounds.hi() {
            return Some(curr);
        }

        // curr partially within bounds
        if self.partial && curr.lo() <= self.bounds.hi() {
            return Some(curr);
        }

        self.next()
    }
}

impl<I> StripeSet<I>
where
    I: Integer + Clone + std::fmt::Debug,
{
    pub fn new(ranges: Vec<StepRange<I>>) -> Self {
        let mut a = Self { ranges };
        a.normalize();
        a
    }

    pub fn iter_within<B, I2>(&self, bounds: B) -> StripeSetBoundedIter<'_, I, B, I2>
    where
        I: Integer + Clone + PartialOrd<I2>,
        B: Bounded<I2>,
        I2: PartialOrd<I> + Eq,
    {
        StripeSetBoundedIter {
            bounds,
            index: 0,
            set: self,
            partial: false,
            _i2: PhantomData,
        }
    }

    pub fn iter_within_partial<B, I2>(&self, bounds: B) -> StripeSetBoundedIter<'_, I, B, I2>
    where
        I: Integer + Clone + PartialOrd<I2>,
        B: Bounded<I2>,
        I2: PartialOrd<I> + Eq,
    {
        StripeSetBoundedIter {
            bounds,
            index: 0,
            set: self,
            partial: true,
            _i2: PhantomData,
        }
    }

    fn sort(&mut self) {
        self.ranges.sort_by(Self::cmp_range)
    }

    pub fn add_range(&mut self, r: StepRange<I>) {
        let mut remaining = Some(r);
        let mut insertion_point = 0;
        for (i, el) in self.ranges.iter().enumerate() {
            if let Some(new_el) = &remaining {
                if el.hi() > new_el.lo() {
                    // we assume the ranges are sorted, so non overlapping ranges are sorted by bounds
                    insertion_point = i;
                    continue;
                } else if el.lo() > new_el.hi() {
                    break;
                }
                if el.step() < new_el.step() {
                    insertion_point = i;
                    continue;
                }
            } else {
                break;
            }
            remaining = remaining.and_then(|a| a.compactify(el));
        }
        if let Some(new_el) = remaining {
            self.ranges.insert(insertion_point, new_el);
        }
    }

    pub fn normalize(&mut self) {
        self.sort();
        // self.consolidate();
    }

    fn cmp_range(a: &StepRange<I>, b: &StepRange<I>) -> Ordering {
        // A |------------|
        // B                       |------------|
        if a.hi() < b.lo() {
            // non-overlapping ranges are sorted in terms of their bounds
            Ordering::Less
        } else if a.lo() > b.hi() {
            Ordering::Greater
        } else {
            // otherwise sort by step
            a.step().cmp(b.step())
        }
    }
}

impl<I> Set<I> for StripeSet<I>
where
    I: Integer + Clone,
{
    fn includes(&self, element: I) -> bool {
        for range in &self.ranges {
            if range.hi() < &element {
                break;
            }
            if range.includes(&element) {
                return true;
            }
        }

        false
    }
}

impl<'a, I> Subset<&'a StripeSet<I>> for &'a StripeSet<I>
where
    I: Integer + Clone + std::fmt::Debug,
{
    fn subset_of(self, rhs: Self) -> bool {
        for range in &self.ranges {
            let mut start = range.lo().clone();
            for other in rhs
                .iter_within_partial(range)
                .map(|x| dbg!(x))
                .filter(|x| range.step().is_multiple_of(x.step()))
                .map(|y| dbg!(y))
            {
                dbg!(&other, &range);
                if other.lo() > &start {
                    // some part of our range was skipped, so this isn't a subset.
                    return false;
                }

                start = other.hi().clone() + range.step().clone();
            }
            dbg!(&start);
            if &start < range.hi() {
                return false;
            }
        }
        true
    }

    fn strict_subset_of(self, _: Self) -> bool {
        todo!()
    }
}

impl<I> Union<StripeSet<I>> for StripeSet<I>
where
    I: Integer + Clone + std::fmt::Debug,
{
    type Output = StripeSet<I>;

    fn union(self, rhs: StripeSet<I>) -> Self::Output {
        StripeSet::new([self.ranges, rhs.ranges].concat())
    }
}

#[test]
fn simple() {
    let a = StripeSet::new(vec![StepRange::new(0, 10, 2), StepRange::new(15, 20, 1)]);
    let b = StripeSet::new(vec![StepRange::new(0, 18, 6)]);
    assert!(b.subset_of(&a));
    assert!(!a.subset_of(&b));
}

#[test]
fn union() {
    let a = StripeSet::new(vec![StepRange::new(0, 5, 1), StepRange::new(10, 20, 2)]);
    let b = StripeSet::new(vec![StepRange::new(0, 18, 6)]);
    let c = a.clone().union(b.clone());
    dbg!(&c);
    assert!(a.subset_of(&c));
    assert!(b.subset_of(&c));
}

pub struct Stripe<I: Integer> {
    pub offset: I,
    pub start: I,
    pub end: I,
    pub step: I,
}

impl<I: Integer + Clone + std::fmt::Debug> Stripe<I> {
    fn subset_of(&self, other: &Stripe<I>) -> bool {
        let (step_div, step_mod) = self.step.div_mod_floor(&other.step);
        let (offset_div, offset_mod) = if other.offset.is_zero() {
            self.offset.div_mod_floor(&other.step)
        } else {
            self.offset.div_mod_floor(&other.offset)
        };
        dbg!(&step_div, &step_mod, &offset_div, &offset_mod);
        if !step_mod.is_zero() || !offset_mod.is_zero() {
            return false;
        }
        let adjusted_start_b = step_div.clone() * other.start.clone()
            - (self.offset.clone() - other.offset.clone() * offset_div.clone());
        let adjusted_end_b = step_div.clone() * other.end.clone()
            - (self.offset.clone() - other.offset.clone() * offset_div.clone());
        dbg!(&adjusted_start_b, &adjusted_end_b);
        self.start >= adjusted_start_b && self.end <= adjusted_end_b
    }
}

pub struct StripeTreeNode<'bump, I: Integer> {
    pub step: I,
    pub offset: I,
    pub ranges: bumpalo::collections::Vec<'bump, Range<I>>,
    pub children: bumpalo::collections::Vec<'bump, StripeTreeNode<'bump, I>>,
}

pub struct StripeTree<'bump, I: Integer> {
    bumpalo: &'bump bumpalo::Bump,
    root: StripeTreeNode<'bump, I>,
}

impl<'bump, I: Integer> StripeTree<'bump, I> {
    pub fn new_in(alloc: &'bump bumpalo::Bump) -> StripeTree<'bump, I> {
        StripeTree {
            bumpalo: alloc,
            root: StripeTreeNode {
                step: I::one(),
                offset: I::zero(),
                ranges: bumpalo::collections::Vec::new_in(alloc),
                children: bumpalo::collections::Vec::new_in(alloc),
            },
        }
    }

    pub fn coverage(&mut self, step: I, range: Range<I>) {}

    pub fn add_node(&mut self, step: I, offset: I, range: Range<I>) {
        Self::add_node_below(self.bumpalo, step, offset, range, &mut self.root)
    }

    fn add_node_below(
        bump: &'bump bumpalo::Bump,
        step: I,
        offset: I,
        range: Range<I>,
        node: &mut StripeTreeNode<'bump, I>,
    ) {
        if node.step == step && offset == node.offset {
            node.ranges.push(range);
            return;
        }
        for (i, current) in node.children.iter_mut().enumerate() {
            if current.step == step && offset == current.offset {
                current.ranges.push(range);
                return;
            }
            if current.step.is_multiple_of(&step)
                && offset.mod_floor(&current.step) == current.offset
            {
                if current.step >= step {
                    return Self::add_node_below(bump, step, offset, range, current);
                } else {
                    // TODO: add other children below current now.
                    let old_current = std::mem::replace(
                        current,
                        StripeTreeNode {
                            step,
                            offset,
                            ranges: bumpalo::vec![in &bump],
                            children: bumpalo::vec![in &bump],
                        },
                    );
                    current.children.push(old_current);
                    return;
                }
            }
        }
    }
}

#[test]
fn a() {
    let a = Stripe {
        offset: 1,
        start: 1,
        end: 2,
        step: 10,
    };
    let b = Stripe {
        offset: 1,
        start: 1,
        end: 2,
        step: 5,
    };

    assert!(!a.subset_of(&b));

    let c = Stripe {
        offset: 0,
        start: 0,
        end: 3,
        step: 6,
    };
    let d = Stripe {
        offset: 0,
        start: 0,
        end: 10,
        step: 3,
    };

    assert!(c.subset_of(&d));
}
