use std::{
    any::TypeId,
    collections::{BTreeMap, BTreeSet, HashMap, VecDeque},
    f32::consts::E,
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

    pub fn range(&self) -> Option<IntegerRange> {
        if let Some(lo_range) = self.ranges.first() {
            if let Some(hi_range) = self.ranges.last() {
                return Some((lo_range.lo.clone(), hi_range.hi.clone()).into());
            }
        }

        None
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

    /// Exclude a set of integers from the variable's domain
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

#[derive(Default, Debug)]
pub struct Enviornmnet {
    constraints: Vec<Box<dyn Constraint>>,
    domains: BTreeMap<Variable, IntegerSet>,
    next_var_id: usize,
}

impl Enviornmnet {
    pub fn new() -> Enviornmnet {
        Enviornmnet::default()
    }

    pub fn var(&mut self, domain: IntegerSet) -> Variable {
        let var = Variable {
            id: self.next_var_id,
        };
        self.next_var_id = match self.next_var_id.checked_add(1) {
            Some(v) => v,
            None => panic!("ran out of variable IDs!"),
        };

        self.domains.insert(var.clone(), domain);
        var
    }

    pub fn get(&self, var: Variable) -> &IntegerSet {
        self.domains
            .get(&var)
            .expect("variable was interned but does not appear in domain map")
    }

    pub fn set(&mut self, var: Variable, domain: IntegerSet) {
        self.domains.insert(var, domain);
    }
}
