use crate::integer::{
    num_bigint::{BigInt, BigUint},
    IntegerSet,
};
use slotmap::{new_key_type, Key, SecondaryMap, SlotMap};

pub enum Mutation {
    AdjustLo(BigUint),
    AdjustHi(BigUint),
    Instantiate,
    Exclude(IntegerSet),
}

pub enum ConstraintResult {
    Narrow(Mutation),
    Satisfied,
    Violation,
}

impl<const N: usize> VariableSet for [VariableId; N] {
    fn contains(&self, var: VariableId) -> bool {
        for x in 0..N {
            if self[x] == var {
                return true;
            }
        }

        false
    }
}

pub trait Constraint {
    type Vars: VariableSet
    where
        Self: Sized;
    fn vars(&self) -> &Self::Vars
    where
        Self: Sized;
    fn narrow(&self, event: &Event) -> ConstraintResult;
}

new_key_type! { pub struct EventId; }
new_key_type! { pub struct VariableId; }
new_key_type! { pub struct ConstraintId; }

pub enum EventSource {
    Constraint(ConstraintId),
    SourcePosition {
        file: &'static str,
        line: usize,
        column: usize,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Variable {
  value: IntegerSet,
  instantiated: bool
}


pub struct Event {
    id: EventId,
    variable: VariableId,
    constraint: ConstraintId,
    parent: EventId,
    result: ConstraintResult,
}

pub struct EventStore {
    events: SlotMap<EventId, Event>,
    root_event: EventId,
}

struct EnvironmentState {
    variables: SecondaryMap<VariableId, Variable>,
}

impl EnvironmentState {
  pub fn apply_mutation(&mut self, variable_id: VariableId, mutation: Mutation) -> EnvironmentState {    
    if self.variables[variable_id].instantiated {
      todo!("this should be a soft error")
    }
    match mutation {
      Mutation::Instantiate => self.variables[variable_id].instantiated = true,
      Mutation::Exclude(exclude) => {
        if !exclude.is_subset_of(&self.variables[variable_id].value) {
            todo!("this should be a soft error");
        }
        self.variables[variable_id].value.subtract();
      }
      Mutation::AdjustHi(offset) => {
        self.variables[variable_id].value.exclude_above(&self.hi - &offset);
      }
    }
  }
}

impl std::hash::Hash for EnvironmentState {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
      hasher.write_usize(self.variables.len());
      for (var_id, var_value) in &self.variables {
        var_id.hash(hasher);
        var_value.hash(hasher);
      }
    }
}

pub struct Solver {
  state: EnvironmentState,
  
}
