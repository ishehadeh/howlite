use crate::{
    constraints::{BinaryAddConstraint, OffsetLtConstraint},
    environment::Environment,
    IntegerSet,
};

#[test]
fn compare() {
    let mut env = Environment::new();
    let x = env.create_variable(IntegerSet::new_from_tuples(&[(0, 5)]));
    let y = env.create_variable(IntegerSet::new_from_tuples(&[(4, 6)]));
    let z = env.create_variable(IntegerSet::new_from_tuples(&[(-5, 2)]));
    env.constrain(OffsetLtConstraint::new(x, 0, y));
    env.constrain(OffsetLtConstraint::new(z, 0, x));
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(0, 3)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(4, 6)]));
    assert_eq!(env.domain(z), IntegerSet::new_from_tuples(&[(-5, -1)]));
}

#[test]
fn add() {
    let mut env = Environment::new();
    let x = env.create_variable(IntegerSet::new_from_tuples(&[(0, 5)]));
    let y = env.create_variable(IntegerSet::new_from_tuples(&[(4, 6)]));
    let z = env.create_variable(IntegerSet::new_from_tuples(&[(7, 10)]));
    env.constrain(BinaryAddConstraint::new(x, y, z));
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(3, 4)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(4, 6)]));
    assert_eq!(env.domain(z), IntegerSet::new_from_tuples(&[(7, 10)]));
}

#[test]
fn add_coefficient() {
    let mut env = Environment::new();
    let x = env.create_variable(IntegerSet::new_from_tuples(&[(0, 5)]));
    let y = env.create_variable(IntegerSet::new_from_tuples(&[(4, 6)]));
    let z = env.create_variable(IntegerSet::new_from_tuples(&[(4, 7)]));
    env.constrain(BinaryAddConstraint::new_with_coefficient(x, 3.into(), y, z));
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(0, 0)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(4, 6)]));
    assert_eq!(env.domain(z), IntegerSet::new_from_tuples(&[(4, 6)]));
}

#[test]
fn add_chain() {
    let mut env = Environment::new();
    let a = env.create_variable(IntegerSet::new_from_tuples(&[(0, 5)]));
    let b = env.create_variable(IntegerSet::new_from_tuples(&[(4, 6)]));
    let c = env.create_variable(IntegerSet::new_from_tuples(&[(7, 10)]));
    let d = env.create_variable(IntegerSet::new_from_tuples(&[(-5, 10)]));
    let e = env.create_variable(IntegerSet::new_from_tuples(&[(0, 3)]));
    env.constrain(BinaryAddConstraint::new(a, b, c));
    env.constrain(BinaryAddConstraint::new(c, d, e));
    assert_eq!(env.domain(a), IntegerSet::new_from_tuples(&[(3, 3)]));
    assert_eq!(env.domain(b), IntegerSet::new_from_tuples(&[(4, 4)]));
    assert_eq!(env.domain(c), IntegerSet::new_from_tuples(&[(7, 7)]));
    assert_eq!(env.domain(d), IntegerSet::new_from_tuples(&[(-5, -4)]));
    assert_eq!(env.domain(e), IntegerSet::new_from_tuples(&[(0, 3)]));
}

#[test]
fn add_compare() {
    let mut env = Environment::new();
    let x = env.create_variable(IntegerSet::new_from_tuples(&[(0, 5)]));
    let y = env.create_variable(IntegerSet::new_from_tuples(&[(4, 6)]));
    let z = env.create_variable(IntegerSet::new_from_tuples(&[(0, 100)]));
    env.constrain(BinaryAddConstraint::new(x, y, z));
    env.constrain(OffsetLtConstraint::new(z, -5, y));
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(0, 2)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(4, 6)]));
    assert_eq!(env.domain(z), IntegerSet::new_from_tuples(&[(4, 8)]));
}
