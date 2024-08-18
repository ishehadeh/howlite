use std::{
    collections::{HashSet, VecDeque},
    fmt::Debug,
};

use slotmap::{new_key_type, Key, SecondaryMap, SlotMap};

use crate::{
    integer::{IntegerRange, IntegerSet},
    variables::{InvalidMutationError, Mutation, Variable, VariableId},
};

pub trait Constraint: Debug {
    fn propogate(&mut self, env: &mut PropogationEnvironment<'_>) -> bool;
}

pub type AnyConstraint = Box<dyn Constraint>;

impl Constraint for AnyConstraint {
    fn propogate(&mut self, env: &mut PropogationEnvironment<'_>) -> bool {
        self.as_mut().propogate(env)
    }
}

new_key_type! { pub struct ConstraintId; }
new_key_type! { pub struct GenerationId; }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EventNodeState {
    Waiting,
    Consistent,
    Inconsistent,
    Rolledback,
}

#[derive(Clone, Default, Debug)]
pub struct VariableGeneration {
    variables: rpds::HashTrieMap<VariableId, Variable>,
}

impl VariableGeneration {
    pub fn create(&self, domain: IntegerSet) -> (VariableId, VariableGeneration) {
        let id = VariableId::new();
        (
            id,
            VariableGeneration {
                variables: self.variables.insert(id, Variable::Domain(domain)),
            },
        )
    }

    pub fn mutate(
        &self,
        variable: VariableId,
        mutation: Mutation,
    ) -> Result<VariableGeneration, InvalidMutationError> {
        let new_val = self
            .variables
            .get(&variable)
            .expect("invalid variable id")
            .clone()
            .apply(mutation)?;
        Ok(VariableGeneration {
            variables: self.variables.insert(variable, new_val),
        })
    }

    pub fn clear(&self, variable: VariableId) -> VariableGeneration {
        VariableGeneration {
            variables: self
                .variables
                .insert(variable, Variable::Domain(IntegerSet::empty())),
        }
    }

    pub fn get(&self, variable: VariableId) -> &Variable {
        self.variables.get(&variable).expect("invalid variable id")
    }
}

#[derive(Debug, Clone)]
pub struct Generation {
    parent: GenerationId,
    variables: VariableGeneration,
    action: GenerationAction,
}

#[derive(Clone, Debug)]
enum GenerationAction {
    NoAction,
    CreateVariable {
        variable: VariableId,
        value: IntegerSet,
    },
    MutateVariable {
        constraint: ConstraintId,
        mutation: Mutation,
        variable: VariableId,
    },
}

#[derive(Debug)]
pub struct Environment {
    constraints: SlotMap<ConstraintId, AnyConstraint>,
    generations: SlotMap<GenerationId, Generation>,
    satisfies: SecondaryMap<GenerationId, HashSet<ConstraintId>>,
    current_generation: GenerationId,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Environment {
        let mut generations: SlotMap<GenerationId, Generation> = Default::default();
        let root_gen_id = generations.insert(Generation {
            parent: GenerationId::null(),
            variables: Default::default(),
            action: GenerationAction::NoAction,
        });
        Environment {
            constraints: Default::default(),
            current_generation: root_gen_id,
            generations,
            satisfies: Default::default(),
        }
    }

    pub fn generation(&self, generation_id: GenerationId) -> &Generation {
        self.generations
            .get(generation_id)
            .expect("invalid generation id")
    }

    pub fn current_generation(&self) -> GenerationId {
        self.current_generation
    }

    pub fn create_variable(&mut self, domain: IntegerSet) -> VariableId {
        let current_generation = self.generation(self.current_generation);

        let (id, variables) = current_generation.variables.create(domain.clone());
        let generation = Generation {
            parent: self.current_generation,
            variables,
            action: GenerationAction::CreateVariable {
                variable: id,
                value: domain,
            },
        };
        self.current_generation = self.generations.insert(generation);
        id
    }

    pub fn domain(&self, variable: VariableId) -> IntegerSet {
        match self
            .generation(self.current_generation)
            .variables
            .get(variable)
        {
            Variable::Domain(d) => d.clone(),
            Variable::Instantiated(x) => IntegerSet::new(&[(x.clone(), x.clone())]),
        }
    }
}

impl Environment {
    fn do_constraint(
        &mut self,
        constraint_id: ConstraintId,
        initial_generation: GenerationId,
    ) -> (GenerationId, bool) {
        let mut environment = PropogationEnvironment {
            constraint_id,
            generations: &mut self.generations,
            parent_current_generation: initial_generation,
            local_current_generation: initial_generation,
        };
        let constraint = self
            .constraints
            .get_mut(constraint_id)
            .expect("invalid constraint id");

        println!("PROPOGATE constraint={constraint_id:?}, generation={initial_generation:?}");
        let success = constraint.propogate(&mut environment);
        println!(
            "PROPOGATE END [generation={:?}, success={success}]",
            environment.local_generation_id()
        );
        (environment.local_generation_id(), success)
    }

    fn show_gen(&self, generation: GenerationId) {
        {
            let current_gen = self.generation(generation);
            println!(
                "GENERATION: {:?}, action={:?}",
                self.current_generation, current_gen.action
            );
            for (var_id, var) in current_gen.variables.variables.iter() {
                println!("  {var_id:?} = {var:?}")
            }
        }
    }

    pub fn satisfied_constraints(&self, generation: GenerationId) -> HashSet<ConstraintId> {
        self.satisfies.get(generation).cloned().unwrap_or_default()
    }

    pub fn satisfies_all(&self, generation: GenerationId) -> bool {
        self.satisfies
            .get(generation)
            .map(|x| x.len() == self.constraints.len())
            .unwrap_or(false)
    }

    pub fn run_constraints(&mut self, mut generation: GenerationId) -> GenerationId {
        let constraints: HashSet<_> = self.constraints.keys().collect();
        let satisfied_constraints = self.satisfied_constraints(generation);
        let unsatisfied_constraints = constraints.difference(&satisfied_constraints);
        for &constraint_id in unsatisfied_constraints {
            self.show_gen(generation);
            let (next_generation, success) = self.do_constraint(constraint_id, generation);
            if success {
                if let Some(satisfied_constraints) = self.satisfies.get_mut(next_generation) {
                    satisfied_constraints.insert(constraint_id);
                } else {
                    self.satisfies
                        .insert(next_generation, HashSet::from([constraint_id]));
                }
                if self.satisfies[next_generation].len() == self.constraints.len() {
                    return next_generation;
                }
            }
            if generation != next_generation && success {
                println!("DESCEND");
                generation = self.run_constraints(next_generation)
            } else {
                println!(
                    "SKIP success={}, changed={}",
                    success,
                    generation != next_generation
                );
            }
        }

        generation
    }

    pub fn constrain<T: Constraint + 'static>(&mut self, constraint: T) -> ConstraintId {
        let new_constraint_id = self.constraints.insert(Box::new(constraint));
        self.current_generation = self.run_constraints(self.current_generation);
        assert!(self.satisfies_all(self.current_generation));
        new_constraint_id
    }
}

#[derive(Debug)]
/// A frozen environment where mutations from a single constraint are applied in isolation.
pub struct PropogationEnvironment<'env> {
    constraint_id: ConstraintId,
    generations: &'env mut SlotMap<GenerationId, Generation>,
    parent_current_generation: GenerationId,
    local_current_generation: GenerationId,
}

impl<'env> PropogationEnvironment<'env> {
    pub fn local_generation_id(&self) -> GenerationId {
        self.local_current_generation
    }

    pub fn environment_generation_id(&self) -> GenerationId {
        self.local_current_generation
    }

    pub fn generation(&self, generation_id: GenerationId) -> &Generation {
        self.generations
            .get(generation_id)
            .expect("invalid generation id")
    }

    pub fn last_mutation(&self) -> Option<(VariableId, Mutation)> {
        let mut generation_id = self.local_current_generation;
        while generation_id != GenerationId::null() {
            let generation = self.generation(generation_id);
            match &generation.action {
                GenerationAction::MutateVariable {
                    constraint: _,
                    mutation,
                    variable,
                } => return Some((*variable, mutation.clone())),
                _ => generation_id = generation.parent,
            }
        }

        None
    }

    pub fn variable(&self, variable: VariableId) -> &Variable {
        self.generation(self.local_current_generation)
            .variables
            .get(variable)
    }

    pub fn mutate(
        &mut self,
        variable: VariableId,
        mutation: Mutation,
    ) -> Result<(), InvalidMutationError> {
        println!("  MUTATE {variable:?} {mutation:?}");
        let new_variable_generation = self
            .generation(self.local_current_generation)
            .variables
            .mutate(variable, mutation.clone())?;
        let new_generation = Generation {
            parent: self.local_current_generation,
            variables: new_variable_generation,
            action: GenerationAction::MutateVariable {
                constraint: self.constraint_id,
                mutation,
                variable,
            },
        };

        self.local_current_generation = self.generations.insert(new_generation);
        Ok(())
    }

    pub fn variable_range(&self, variable: VariableId) -> Option<IntegerRange> {
        match self.variable(variable) {
            Variable::Domain(domain) => domain.range(),
            Variable::Instantiated(val) => Some(IntegerRange::new(val.clone(), val.clone())),
        }
    }
}
