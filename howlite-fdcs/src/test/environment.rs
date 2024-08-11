use num_bigint::BigInt;

use crate::{
    Constraint, Environment, Event, EventSink, IntegerRange, IntegerSet, Mutation, Variable,
    VariableSet,
};

#[derive(Debug, Clone)]
pub enum TestConstraint {
    Less { lhs: Variable, rhs: Variable },
}

impl Constraint for TestConstraint {
    fn propogate(&mut self, vars: &VariableSet, mut events: EventSink<'_>, event: Event)
    where
        Self: Sized,
    {
        match self {
            TestConstraint::Less { lhs, rhs } => match event.mutation {
                crate::Mutation::Instantiate { variable, binding } => todo!(),
                crate::Mutation::Bound {
                    variable: e_var,
                    range,
                } if e_var == *lhs => {
                    let rhs_set = vars.get(*rhs);
                    if range.hi < rhs_set.min().unwrap() {
                        events.submit(Mutation::Bound {
                            variable: rhs.clone(),
                            range: IntegerRange::new(range.hi, rhs_set.max().unwrap()),
                        });
                    }
                }
                crate::Mutation::Bound {
                    variable: e_var,
                    range,
                } if e_var == *rhs => {
                    let lhs_set = vars.get(*lhs);
                    if range.lo < lhs_set.max().unwrap() {
                        events.submit(Mutation::Bound {
                            variable: lhs.clone(),
                            range: IntegerRange::new(range.lo, lhs_set.min().unwrap()),
                        });
                    }
                }
                _ => (),
            },
        }
    }

    fn initialize(&mut self, vars: &VariableSet, mut events: EventSink<'_>)
    where
        Self: Sized,
    {
        match self {
            TestConstraint::Less { lhs, rhs } => {
                let rhs_range = vars.get(*rhs).range().unwrap();
                let lhs_range = vars.get(*lhs).range().unwrap();
                events.submit(Mutation::Bound {
                    variable: *lhs,
                    range: IntegerRange::new(lhs_range.lo, rhs_range.lo - 1),
                });
                events.submit(Mutation::Bound {
                    variable: *rhs,
                    range: IntegerRange::new(lhs_range.hi + 1, rhs_range.hi),
                });
            }
        }
    }
}

#[test]
fn compare() {
    let mut env = Environment::new();
    let x = env.variables.create(IntegerSet::new(&[(0, 5)]));
    let y = env.variables.create(IntegerSet::new(&[(4, 6)]));
    let z = env.variables.create(IntegerSet::new(&[(-5, 2)]));
    env.constrain(TestConstraint::Less { lhs: x, rhs: y });
    env.constrain(TestConstraint::Less { lhs: z, rhs: x });
    assert_eq!(env.variables.get(x).clone(), IntegerSet::new(&[(3, 3)]));
    assert_eq!(env.variables.get(y).clone(), IntegerSet::new(&[(6, 6)]));
    assert_eq!(env.variables.get(z).clone(), IntegerSet::new(&[(-5, -1)]));
}
