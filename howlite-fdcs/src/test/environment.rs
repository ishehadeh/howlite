use crate::{
    constraints::OffsetCompare, Constraint, ConstraintContext, Environment, Event, IntegerRange,
    IntegerSet, Mutation, Variable,
};

#[test]
fn compare() {
    let mut env = Environment::new();
    let x = env.variables.create(IntegerSet::new(&[(0, 5)]));
    let y = env.variables.create(IntegerSet::new(&[(4, 6)]));
    let z = env.variables.create(IntegerSet::new(&[(-5, 2)]));
    env.constrain(OffsetCompare::less(x, 0, y));
    env.constrain(OffsetCompare::less(z, 0, x));
    assert_eq!(env.variables.get(x).clone(), IntegerSet::new(&[(3, 3)]));
    assert_eq!(env.variables.get(y).clone(), IntegerSet::new(&[(6, 6)]));
    assert_eq!(env.variables.get(z).clone(), IntegerSet::new(&[(-5, -1)]));
}
