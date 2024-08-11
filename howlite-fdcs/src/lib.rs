use std::{
    collections::{BTreeMap, VecDeque},
    fmt::Debug,
};

use num_bigint::BigInt;

#[cfg(test)]
mod test;

pub mod integer;
use integer::{IntegerRange, IntegerSet};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable {
    id: usize,
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
    fn propogate(&mut self, vars: &VariableSet, event: Event) -> Vec<Event>
    where
        Self: Sized;

    fn initialize(&mut self, vars: &VariableSet) -> Vec<Event>
    where
        Self: Sized;
}
#[derive(Debug, Default, Clone)]
pub struct VariableSet {
    next_var_id: usize,
    domains: BTreeMap<Variable, IntegerSet>,
}

impl VariableSet {
    pub fn create(&mut self, domain: IntegerSet) -> Variable {
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

    pub fn get(&self, var: Variable) -> &IntegerSet {
        self.domains
            .get(&var)
            .expect("variable was interned but does not appear in domain map")
    }

    pub fn set(&mut self, var: Variable, domain: IntegerSet) {
        self.domains.insert(var, domain);
    }
}

#[derive(Debug)]
pub struct Enviornmnet<ConstraintT>
where
    ConstraintT: Constraint,
{
    constraints: Vec<ConstraintT>,
    pub variables: VariableSet,
}

impl<ConstraintT> Default for Enviornmnet<ConstraintT>
where
    ConstraintT: Constraint,
{
    fn default() -> Self {
        Self {
            constraints: Default::default(),
            variables: Default::default(),
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

    fn mutate_var<F: Fn(IntegerSet) -> IntegerSet>(&mut self, var: Variable, mutation: F) {
        let domain = self.variables.get(var);
        self.variables.set(var, mutation(domain.clone()))
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
                let range_before = self.variables.get(variable).clone();
                self.mutate_var(variable, |mut domain| {
                    domain.exclude(&excluded);
                    domain
                });
                let range_after = self.variables.get(variable).clone();
                assert_eq!(range_before, range_after);
            }
        }
    }
}

impl<ConstraintT> Enviornmnet<ConstraintT>
where
    ConstraintT: Constraint,
{
    pub fn constrain(&mut self, mut constraint: ConstraintT) {
        let mut events: VecDeque<Event> = Default::default();
        events.extend(constraint.initialize(&self.variables));
        self.constraints.push(constraint);

        while let Some(event) = events.pop_front() {
            self.do_event(event.clone());
            for constraint in &mut self.constraints {
                events.extend(constraint.propogate(&self.variables, event.clone()))
            }
        }
    }
}
