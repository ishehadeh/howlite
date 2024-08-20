use std::{collections::HashSet, fmt::Debug};

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
    root: GenerationId,
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
            root: root_gen_id,
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
            Variable::Instantiated(x) => IntegerSet::new_from_tuples(&[(x.clone(), x.clone())]),
        }
    }
}

impl Environment {
    fn do_constraint(
        &mut self,
        constraint_id: ConstraintId,
        initial_generation: GenerationId,
    ) -> (GenerationId, bool) {
        self.show_gen(initial_generation);
        let constraint = self
            .constraints
            .get_mut(constraint_id)
            .expect("invalid constraint id");

        println!("PROPOGATE constraint={constraint_id:?}, generation={initial_generation:?}");
        let (result_gen, success) = {
            let mut environment = PropogationEnvironment {
                constraint_id,
                generations: &mut self.generations,
                parent_current_generation: initial_generation,
                local_current_generation: initial_generation,
            };
            let success = constraint.propogate(&mut environment);
            (environment.local_generation_id(), success)
        };
        println!("PROPOGATE END [generation={result_gen:?}, success={success}]",);
        self.show_gen(result_gen);
        (result_gen, success)
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

    fn mark_satisfied(&mut self, generation: GenerationId, constraint: ConstraintId) {
        if let Some(satisfied) = self.satisfies.get_mut(generation) {
            satisfied.insert(constraint);
        } else {
            self.satisfies
                .insert(generation, HashSet::from([constraint]));
        }
    }

    pub fn satisfies_all(&self, generation: GenerationId) -> bool {
        self.satisfies
            .get(generation)
            .map(|x| x.len() == self.constraints.len())
            .unwrap_or(false)
    }

    pub fn run_constraints(&mut self, generation: GenerationId) -> GenerationId {
        println!("\nrun_constraints(): generation={generation:?}");
        let keys: HashSet<_> = self.constraints.keys().collect();
        let satisfied_set = self.satisfied_constraints(generation);
        let mut unsatisfied_iter = keys.difference(&satisfied_set).collect::<Vec<_>>();
        unsatisfied_iter.sort(); // keep sorted so behavior is determenistic. Order matters here!
        for &constraint_id in unsatisfied_iter {
            let (constraint_gen, consistent) = self.do_constraint(constraint_id, generation);
            if consistent {
                self.mark_satisfied(constraint_gen, constraint_id);

                if constraint_gen != generation {
                    dbg!(constraint_gen);
                    if self.satisfies_all(constraint_gen) {
                        println!("found");
                        return constraint_gen;
                    }
                    let descent_gen = self.run_constraints(constraint_gen);
                    if self.satisfies_all(descent_gen) {
                        return descent_gen;
                    }
                } else {
                    self.mark_satisfied(generation, constraint_id);
                }
            }
        }

        println!("exit run_constraints() generation={generation:?}");
        generation
    }

    pub fn constrain<T: Constraint + 'static>(&mut self, constraint: T) -> ConstraintId {
        let new_constraint_id = self.constraints.insert(Box::new(constraint));
        let var_gen = self.last_variable_introduction();
        self.current_generation = self.run_constraints(var_gen.unwrap_or(self.root));
        new_constraint_id
    }

    fn last_variable_introduction(&self) -> Option<GenerationId> {
        let mut generation_id = self.current_generation;
        while generation_id != GenerationId::null() {
            let generation = self.generation(generation_id);
            match &generation.action {
                GenerationAction::CreateVariable {
                    variable: _,
                    value: _,
                } => return Some(generation_id),
                _ => generation_id = generation.parent,
            }
        }

        None
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
