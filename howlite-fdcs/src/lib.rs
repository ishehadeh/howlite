use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    fmt::Debug,
};

use num_bigint::BigInt;

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
id_type!(ConstraintId, ConstraintIdGenerator);
id_type!(EventId, EventIdGenerator);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Variable(Variable),
    Value(BigInt),
}

#[derive(Clone, Debug)]
pub enum Mutation {
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

#[derive(Debug, Clone)]
pub struct Event {
    pub id: EventId,
    pub source: ConstraintId,
    pub mutation: Mutation,
}

pub trait Constraint: Debug {
    fn propogate(&mut self, vars: &VariableSet, events: EventSink<'_>, event: Event);
    fn initialize(&mut self, vars: &VariableSet, events: EventSink<'_>);
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
    constraint_id_gen: ConstraintIdGenerator,
    constraints: HashMap<ConstraintId, ConstraintT>,
    events: EventQueue,
    pub variables: VariableSet,
}

impl<ConstraintT> Default for Environment<ConstraintT>
where
    ConstraintT: Constraint,
{
    fn default() -> Self {
        Self {
            constraint_id_gen: Default::default(),
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

    fn do_event(&mut self, event: Mutation) {
        match event {
            Mutation::Instantiate {
                variable: _,
                binding: _,
            } => todo!("instatiate impl"),
            Mutation::Bound { variable, range } => self.mutate_var(variable, |mut domain| {
                domain.exclude_above(&range.hi);
                domain.exclude_below(&range.lo);
                domain
            }),
            Mutation::Exclude { variable, excluded } => {
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
        let new_id = self.constraint_id_gen.make_id();

        constraint.initialize(&self.variables, self.events.sink(new_id));

        self.constraints.insert(new_id, constraint);

        while let Some(event_data) = self.events.take() {
            self.do_event(event_data.mutation.clone());
            for (constraint_id, constraint) in &mut self.constraints {
                constraint.propogate(
                    &self.variables,
                    self.events.sink(*constraint_id),
                    event_data.clone(),
                )
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct EventQueue {
    events: VecDeque<Event>,
    event_id_gen: EventIdGenerator,
}

pub struct EventSink<'a> {
    queue: &'a mut EventQueue,
    constraint: ConstraintId,
}

impl<'a> EventSink<'a> {
    pub fn submit(&mut self, event: Mutation) -> EventId {
        let id = self.queue.event_id_gen.make_id();
        self.queue.events.push_back(Event {
            source: self.constraint,
            id,
            mutation: event,
        });
        id
    }
}

impl EventQueue {
    pub fn sink(&mut self, constraint: ConstraintId) -> EventSink<'_> {
        EventSink {
            queue: self,
            constraint,
        }
    }

    pub fn take(&mut self) -> Option<Event> {
        self.events.pop_front()
    }
}
