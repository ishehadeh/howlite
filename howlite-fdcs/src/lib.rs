use std::{
    borrow::BorrowMut,
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
};

use num_bigint::BigInt;

#[cfg(test)]
mod test;

mod integer_range;
pub use integer_range::IntegerRange;

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct IntegerSet {
    ranges: BTreeSet<IntegerRange>,
}

impl IntegerSet {
    pub fn new<RangeT: Into<BigInt> + Clone>(ranges: &[(RangeT, RangeT)]) -> IntegerSet {
        IntegerSet {
            ranges: ranges.iter().map(IntegerRange::from).collect(),
        }
    }

    pub fn intersect(&self, other: &IntegerSet) -> IntegerSet {
        let mut intersect = IntegerSet::default();
        for r0 in other.ranges.iter() {
            for r1 in self.ranges.iter() {
                if let Some(r_intersect) = r0.intersect(&r1) {
                    intersect.ranges.insert(r_intersect);
                }
            }
        }

        intersect
    }
}

// IMPL: min/max related functions
impl IntegerSet {
    /// Get the minimum and maximum values of the set.
    pub fn range(&self) -> Option<IntegerRange> {
        match (self.min(), self.max()) {
            (Some(min), Some(max)) => Some(IntegerRange::new(min, max)),
            _ => None,
        }
    }

    pub fn min(&self) -> Option<BigInt> {
        // btree set maintains sort order.
        // for details on how ranges are sorted see: [IntegerRange] impl of Ord
        self.ranges.first().map(|lo_range| lo_range.lo.clone())
    }

    pub fn max(&self) -> Option<BigInt> {
        // btree set maintains sort order, mostly with respect to the lower value,
        // so we need to iterate to find the high.
        // TODO: find a better sort mechanism for ranges.
        self.ranges
            .iter()
            .rev()
            .map(|hi_range| hi_range.hi.clone())
            .max()
    }
}

// IMPL: exclude values
impl IntegerSet {
    pub fn exclude(&mut self, other: &IntegerSet) -> bool {
        let mut has_mutated = false;
        for other_range in &other.ranges {
            has_mutated = self.exclude_range(other_range) || has_mutated
        }

        has_mutated
    }

    pub fn exclude_range(&mut self, other: &IntegerRange) -> bool {
        let mut additions: Vec<IntegerRange> = Vec::new();
        let mut remove: Vec<IntegerRange> = Vec::new();

        for range in &self.ranges {
            if range.lo < other.lo && range.hi > other.hi {
                // other is inside this range, we need to split the range.
                let lo_range = IntegerRange::new(range.lo.clone(), other.lo.clone() - 1);
                let hi_range = IntegerRange::new(other.hi.clone() + 1, range.hi.clone());
                remove.push(range.clone());
                additions.push(lo_range);
                additions.push(hi_range);
            } else if range.lo >= other.lo && range.hi <= other.hi {
                // range completely contained in other
                remove.push(range.clone());
            } else if range.lo >= other.lo && range.hi > other.hi {
                // partial - high
                additions.push(IntegerRange::new(other.hi.clone() + 1, range.hi.clone()));
                remove.push(range.clone())
            } else if range.lo < other.lo && range.hi <= other.hi {
                // partial - low
                additions.push(IntegerRange::new(range.lo.clone(), other.lo.clone() - 1));
                remove.push(range.clone())
            }
        }

        let has_mutated = !additions.is_empty() || !remove.is_empty();
        for r in remove {
            self.ranges.remove(&r);
        }
        for a in additions {
            self.ranges.insert(a);
        }

        has_mutated
    }
}

// IMPL: mutate bounds
impl IntegerSet {
    pub fn exclude_below(&mut self, lo: &BigInt) {
        let too_low: Vec<_> = self
            .ranges
            .iter()
            .take_while(|range| range.lo < *lo)
            .cloned()
            .collect();
        for mut range in too_low {
            self.ranges.remove(&range);
            range.lo.clone_from(lo);
            self.ranges.insert(range);
        }
    }

    pub fn exclude_above(&mut self, hi: &BigInt) {
        // NOTE: do to sort order mostly being by low,
        //  we can't use .rev().take_while() here.
        let too_high: Vec<_> = self
            .ranges
            .iter()
            .filter(|range| range.hi > *hi)
            .cloned()
            .collect();
        for mut range in too_high {
            self.ranges.remove(&range);
            range.hi.clone_from(hi);
            self.ranges.insert(range);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable {
    id: usize,
}

/// An Domain Constraint is an expression in the form X in I, where I is non-empty set of integers
#[derive(Debug, Clone)]
pub struct DomainConstraint {
    variable: Variable,
    /// I, a non-empty set of integers
    domain: IntegerSet,
}

/// A Store is a non-empty set of [DomainConstraint]s
#[derive(Clone, Debug, Default)]
pub struct Store {
    next_var_id: usize,
    constraints: Vec<DomainConstraint>,
}

impl Store {
    pub fn var(&mut self) -> Variable {
        let var = Variable {
            id: self.next_var_id,
        };

        self.next_var_id += 1;
        var
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Variable(Variable),
    Value(BigInt),
}

#[derive(Clone, Debug)]
pub enum Event {
    /// Instantiate `variable` to be equal to `binding`
    Instantiate { variable: Variable, binding: Term },

    /// Update the lower or upper bound of `variable`
    Bound {
        variable: Variable,
        range: IntegerRange,
    },

    /// Exclude a set of integers from the variable's domain, which do NOT update the bounds.
    Exclude {
        variable: Variable,
        excluded: IntegerSet,
    },
}

pub trait Constraint: Debug {
    fn propogate(&mut self, event: Event) -> Vec<Event>
    where
        Self: Sized;
}

#[derive(Debug)]
pub struct Enviornmnet<ConstraintT>
where
    ConstraintT: Constraint,
{
    constraints: Vec<ConstraintT>,
    domains: BTreeMap<Variable, IntegerSet>,
    next_var_id: usize,
}

impl<ConstraintT> Default for Enviornmnet<ConstraintT>
where
    ConstraintT: Constraint,
{
    fn default() -> Self {
        Self {
            constraints: Default::default(),
            domains: Default::default(),
            next_var_id: Default::default(),
        }
    }
}

impl<ConstraintT> Enviornmnet<ConstraintT>
where
    ConstraintT: Constraint,
{
    pub fn new() -> Enviornmnet<ConstraintT> {
        Enviornmnet::default()
    }

    pub fn var_create(&mut self, domain: IntegerSet) -> Variable {
        let var = Variable {
            id: self.next_var_id,
        };
        self.next_var_id = match self.next_var_id.checked_add(1) {
            Some(v) => v,
            None => panic!("ran out of variable IDs!"),
        };

        self.domains.insert(var, domain);
        var
    }

    pub fn var_get(&self, var: Variable) -> &IntegerSet {
        self.domains
            .get(&var)
            .expect("variable was interned but does not appear in domain map")
    }

    pub fn var_set(&mut self, var: Variable, domain: IntegerSet) {
        self.domains.insert(var, domain);
    }

    fn mutate_var<F: Fn(IntegerSet) -> IntegerSet>(&mut self, var: Variable, mutation: F) {
        let domain = self.var_get(var);
        self.var_set(var, mutation(domain.clone()))
    }

    fn do_event(&mut self, event: Event) {
        match event {
            Event::Instantiate {
                variable: _,
                binding: _,
            } => todo!("instatiate impl"),
            Event::Bound { variable, range } => self.mutate_var(variable, |mut domain| {
                domain.exclude_above(&range.hi);
                domain.exclude_below(&range.lo);
                domain
            }),
            Event::Exclude { variable, excluded } => {
                let range_before = self.var_get(variable).clone();
                self.mutate_var(variable, |mut domain| {
                    domain.exclude(&excluded);
                    domain
                });
                let range_after = self.var_get(variable).clone();
                assert_eq!(range_before, range_after);
            }
        }
    }
}

impl<ConstraintT> Enviornmnet<ConstraintT>
where
    ConstraintT: Constraint,
{
    pub fn constrain(&mut self, constraint: ConstraintT) {
        self.constraints.push(constraint)
    }
}
