use num_bigint::BigInt;

use crate::{Constraint, Enviornmnet, Event, IntegerRange, IntegerSet, Variable, VariableSet};

#[derive(Debug, Clone)]
pub enum TestConstraint {
    Less { lhs: Variable, rhs: Variable },
}

impl Constraint for TestConstraint {
    fn propogate(&mut self, vars: &VariableSet, event: crate::Event) -> Vec<crate::Event>
    where
        Self: Sized,
    {
        match self {
            TestConstraint::Less { lhs, rhs } => match event {
                crate::Event::Instantiate { variable, binding } => todo!(),
                crate::Event::Bound {
                    variable: e_var,
                    range,
                } if e_var == *lhs => {
                    let rhs_set = vars.get(*rhs);
                    if range.hi < rhs_set.min().unwrap() {
                        vec![Event::Bound {
                            variable: rhs.clone(),
                            range: IntegerRange::new(range.hi, rhs_set.max().unwrap()),
                        }]
                    } else {
                        vec![]
                    }
                }
                crate::Event::Bound {
                    variable: e_var,
                    range,
                } if e_var == *rhs => {
                    let lhs_set = vars.get(*lhs);
                    if range.lo < lhs_set.max().unwrap() {
                        vec![Event::Bound {
                            variable: lhs.clone(),
                            range: IntegerRange::new(range.lo, lhs_set.min().unwrap()),
                        }]
                    } else {
                        vec![]
                    }
                }
                _ => vec![],
            },
        }
    }

    fn initialize(&mut self, vars: &VariableSet) -> Vec<crate::Event>
    where
        Self: Sized,
    {
        match self {
            TestConstraint::Less { lhs, rhs } => {
                let rhs_range = vars.get(*rhs).range().unwrap();
                let lhs_range = vars.get(*lhs).range().unwrap();
                vec![
                    Event::Bound {
                        variable: *lhs,
                        range: IntegerRange::new(lhs_range.lo, rhs_range.lo - 1),
                    },
                    Event::Bound {
                        variable: *rhs,
                        range: IntegerRange::new(lhs_range.hi + 1, rhs_range.hi),
                    },
                ]
            }
        }
    }
}

#[test]
fn compare() {
    let mut env = Enviornmnet::new();
    let x = env.variables.create(IntegerSet::new(&[(0, 5)]));
    let y = env.variables.create(IntegerSet::new(&[(4, 6)]));
    let z = env.variables.create(IntegerSet::new(&[(-5, 2)]));
    env.constrain(TestConstraint::Less { lhs: x, rhs: y });
    env.constrain(TestConstraint::Less { lhs: z, rhs: x });
    assert_eq!(env.variables.get(x).clone(), IntegerSet::new(&[(3, 3)]));
    assert_eq!(env.variables.get(y).clone(), IntegerSet::new(&[(6, 6)]));
    assert_eq!(env.variables.get(z).clone(), IntegerSet::new(&[(-5, -1)]));
}
