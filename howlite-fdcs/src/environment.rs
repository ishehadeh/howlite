use std::{
    collections::VecDeque,
    fmt::Debug,
};

use slotmap::{new_key_type, Key, SlotMap};

use crate::{
    integer::{IntegerRange, IntegerSet},
    variables::{InvalidMutationError, Mutation, Variable, VariableId},
};

pub trait Constraint: Debug {
    fn propogate(&mut self, env: &mut PropogationEnvironment<'_>);
}

pub type AnyConstraint = Box<dyn Constraint>;

impl Constraint for AnyConstraint {
    fn propogate(&mut self, env: &mut PropogationEnvironment<'_>) {
        self.as_mut().propogate(env)
    }
}

new_key_type! { struct ConstraintId; }
new_key_type! { struct GenerationId; }

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
struct Generation {
    parent: GenerationId,
    variables: VariableGeneration,
    action: GenerationAction,
}

#[derive(Clone, Debug)]
enum GenerationAction {
    NoAction,
    Inconsistency {
        variables: Vec<VariableId>,
    },
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
        }
    }

    pub fn generation(&self, generation_id: GenerationId) -> &Generation {
        self.generations
            .get(generation_id)
            .expect("invalid generation id")
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

    pub fn is_consistent(&self, generation: GenerationId) -> bool {
        !matches!(
            self.generation(generation).action,
            GenerationAction::Inconsistency { .. }
        )
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
    ) -> GenerationId {
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
        constraint.propogate(&mut environment);
        println!(
            "PROPOGATE END [generation={:?}]",
            environment.local_generation_id()
        );
        environment.local_generation_id()
    }

    pub fn constrain<T: Constraint + 'static>(&mut self, constraint: T) {
        let new_constraint_id = self.constraints.insert(Box::new(constraint));
        let new_constraint_generation =
            self.do_constraint(new_constraint_id, self.current_generation);

        let mut generation_queue = VecDeque::new();
        if self.current_generation != new_constraint_generation {
            generation_queue.push_back(new_constraint_generation);
        }

        let mut consistent = true;
        while let Some(generation) = generation_queue.pop_front() {
            let mut mutation_needed = false;
            if !self.is_consistent(generation) {
                consistent = false;
                continue;
            }
            self.current_generation = generation;
            // assert!(self.is_consistent(self.current_generation));

            println!("GENERATION: {:?}", self.current_generation);
            for (var_id, var) in self
                .generation(self.current_generation)
                .variables
                .variables
                .iter()
            {
                println!("  var[{var_id:?}] = {var:?}")
            }
            let constraints: Vec<_> = self.constraints.keys().collect();
            for constraint_id in constraints {
                let new_generation = self.do_constraint(constraint_id, self.current_generation);
                if self.current_generation != new_generation {
                    generation_queue.push_back(new_generation);
                    mutation_needed = true;
                }
            }
            println!(
                "GENERATION END: {:?}, mutation={mutation_needed}",
                self.current_generation
            );
            if !mutation_needed {
                break;
            }
        }
        assert!(consistent);
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

    pub fn inconsistent(&mut self, variables: impl Into<Vec<VariableId>>) {
        let inconsisten_variables = variables.into();
        let generation = self.generation(self.local_current_generation);

        let new_variable_generation = inconsisten_variables
            .iter()
            .fold(generation.variables.clone(), |variables, &variable_id| {
                variables.clear(variable_id)
            });

        let new_generation = Generation {
            parent: self.local_current_generation,
            variables: new_variable_generation,
            action: GenerationAction::Inconsistency {
                variables: inconsisten_variables,
            },
        };

        self.local_current_generation = self.generations.insert(new_generation);
    }

    pub fn mutate(
        &mut self,
        variable: VariableId,
        mutation: Mutation,
    ) -> Result<(), InvalidMutationError> {
        println!("  MUTATE var[{variable:?}] {mutation:?}");
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
