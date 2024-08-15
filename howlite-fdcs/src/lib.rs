use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    fmt::Debug,
};

use num_bigint::BigInt;

pub mod constraints;
#[cfg(test)]
mod test;

pub mod integer;
use integer::{IntegerRange, IntegerSet};

macro_rules! id_type {
    ($name:ident, $gen_name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name {
            id: usize,
        }

        #[derive(Clone, Debug, Default)]
        struct $gen_name {
            next_id: usize,
        }

        impl $gen_name {
            #[allow(dead_code)]
            pub fn new() -> $gen_name {
                Default::default()
            }

            pub fn make_id(&mut self) -> $name {
                let id_int = self.next_id;
                self.next_id += 1;
                $name { id: id_int }
            }
        }
    };
}

id_type!(Variable, VariableIdGenerator);
id_type!(EventId, EventIdGenerator);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Variable(Variable),
    Value(BigInt),
}

#[derive(Clone, Debug)]
pub enum Mutation {
    /// Instantiate `variable` to be equal to `binding`
    Instantiate { binding: Term },

    /// Update the lower or upper bound of `variable`
    Bound { range: IntegerRange },

    /// Exclude a set of integers from the variable's domain, which do NOT update the bounds.
    Exclude { excluded: IntegerSet },
}

#[derive(Debug, Clone)]
pub struct Event {
    pub id: EventId,
    pub subject: Variable,
    pub mutation: Mutation,
}

pub trait Constraint: Debug {
    fn propogate(&mut self, ctx: ConstraintContext, event: Event);
    fn initialize(&mut self, ctx: ConstraintContext);
}

#[derive(Debug, Default, Clone)]
pub struct VariableSet {
    variable_id_gen: VariableIdGenerator,
    domains: BTreeMap<Variable, IntegerSet>,
}

impl VariableSet {
    pub fn create(&mut self, domain: IntegerSet) -> Variable {
        let var = self.variable_id_gen.make_id();
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
pub struct Environment<ConstraintT>
where
    ConstraintT: Constraint,
{
    constraints: Vec<ConstraintT>,
    events: EventQueue,
    pub variables: VariableSet,
}

impl<ConstraintT> Default for Environment<ConstraintT>
where
    ConstraintT: Constraint,
{
    fn default() -> Self {
        Self {
            constraints: Default::default(),
            variables: Default::default(),
            events: Default::default(),
        }
    }
}

impl<ConstraintT> Environment<ConstraintT>
where
    ConstraintT: Constraint,
{
    pub fn new() -> Environment<ConstraintT> {
        Environment::default()
    }

    fn mutate_var<F: Fn(IntegerSet) -> IntegerSet>(&mut self, var: Variable, mutation: F) {
        let domain = self.variables.get(var);
        self.variables.set(var, mutation(domain.clone()))
    }

    fn do_event(&mut self, variable: Variable, event: Mutation) {
        match event {
            Mutation::Instantiate { binding: _ } => todo!("instatiate impl"),
            Mutation::Bound { range } => self.mutate_var(variable, |mut domain| {
                domain.exclude_above(&range.hi);
                domain.exclude_below(&range.lo);
                domain
            }),
            Mutation::Exclude { excluded } => {
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

impl<ConstraintT> Environment<ConstraintT>
where
    ConstraintT: Constraint,
{
    pub fn constrain(&mut self, mut constraint: ConstraintT) {
        let ctx = ConstraintContext {
            variables: &self.variables,
            queue: &mut self.events,
        };
        constraint.initialize(ctx);
        self.constraints.push(constraint);

        while let Some(event_data) = self.events.take() {
            self.do_event(event_data.subject, event_data.mutation.clone());
            for constraint in &mut self.constraints {
                let ctx = ConstraintContext {
                    variables: &self.variables,
                    queue: &mut self.events,
                };
                constraint.propogate(ctx, event_data.clone())
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct EventQueue {
    events: VecDeque<Event>,
    event_id_gen: EventIdGenerator,
}

pub struct ConstraintContext<'a> {
    pub variables: &'a VariableSet,
    queue: &'a mut EventQueue,
}

impl<'a> ConstraintContext<'a> {
    pub fn submit(&mut self, variable: Variable, event: Mutation) -> EventId {
        let id = self.queue.event_id_gen.make_id();
        self.queue.events.push_back(Event {
            subject: variable,
            id,
            mutation: event,
        });
        id
    }
}

impl EventQueue {
    pub fn take(&mut self) -> Option<Event> {
        self.events.pop_front()
    }
}
