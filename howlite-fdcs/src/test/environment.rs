use crate::{
    constraints::{OffsetLtConstraint, MultiplyConstEqConstraint},
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
    let gen = env.run_constraints(env.current_generation()).expect("constraints failed");
    env.set_current_generation(gen);
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(0, 3)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(4, 6)]));
    assert_eq!(env.domain(z), IntegerSet::new_from_tuples(&[(-5, -1)]));
}

#[test]
fn compare_mul_eq() {
    let mut env = Environment::new();
    let x = env.create_variable(IntegerSet::new_from_tuples(&[(0, 5)]));
    let y = env.create_variable(IntegerSet::new_from_tuples(&[(4, 6)]));
    let t0 = env.create_variable(IntegerSet::new_from_tuples(&[(0, 15)]));
    env.constrain(OffsetLtConstraint::new(x, 0, t0));
    env.constrain(MultiplyConstEqConstraint::new(y, 3.into(), t0));
    let gen = env.run_constraints(env.current_generation()).expect("constraints failed");
    env.set_current_generation(gen);
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(0, 0)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(4, 5)]));
    assert_eq!(env.domain(t0), IntegerSet::new_from_tuples(&[(12, 15)]));
}


#[test]
fn compare_mul_eq2() {
    let mut env = Environment::new();
    let x = env.create_variable(IntegerSet::new_from_tuples(&[(1, 2), (7, 9)]));
    let y = env.create_variable(IntegerSet::new_from_tuples(&[(4, 6), (8, 11)]));
    let t0 = env.create_variable(IntegerSet::new_from_tuples(&[(0, 100)]));
    let t1 = env.create_variable(IntegerSet::new_from_tuples(&[(0, 100)]));
    env.constrain(MultiplyConstEqConstraint::new(x, 8.into(), t1));
    env.constrain(MultiplyConstEqConstraint::new(y, 3.into(), t0));
    env.constrain(OffsetLtConstraint::new(t1, 0, t0));
    env.constrain(OffsetLtConstraint::new(t1, 0, t0));
    let gen = env.run_constraints(env.current_generation()).expect("constraints failed");
    env.set_current_generation(gen);
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(2, 2)]));
    assert_eq!(env.domain(t1), IntegerSet::new_from_tuples(&[(16, 16)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(6, 6)]));
    assert_eq!(env.domain(t0), IntegerSet::new_from_tuples(&[(18, 18)]));
}

/*


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
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(0, 1)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(4, 4)]));
    assert_eq!(env.domain(z), IntegerSet::new_from_tuples(&[(4, 7)])); // TODO: this should *only* be 4 or 7 not 4..7
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
    assert_eq!(env.domain(e), IntegerSet::new_from_tuples(&[(2, 3)]));
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

#[test]
fn add_system_simple() {
    let mut env = Environment::new();
    let x = env.create_variable(IntegerSet::new_from_tuples(&[(-20, 20)]));
    let y = env.create_variable(IntegerSet::new_from_tuples(&[(-20, 20)]));

    // TODO: bindings
    let r0 = env.create_variable(IntegerSet::new_from_tuples(&[(5, 5)]));
    let r1 = env.create_variable(IntegerSet::new_from_tuples(&[(7, 7)]));

    let c1 = env.constrain(BinaryAddConstraint::new(x, y, r0));
    let c2 = env.constrain(BinaryAddConstraint::new_with_coefficient(
        x,
        2.into(),
        y,
        r1,
    ));
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(2, 2)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(3, 3)]));
}

#[test]
fn linear_system() {
    let mut env = Environment::new();
    let x = env.create_variable(IntegerSet::new_from_tuples(&[(-20, 20)]));
    let y = env.create_variable(IntegerSet::new_from_tuples(&[(-20, 20)]));

    // TODO: bindings
    let r0 = env.create_variable(IntegerSet::new_from_tuples(&[(-11, -11)]));
    let r1 = env.create_variable(IntegerSet::new_from_tuples(&[(-18, -18)]));

    let t0 = env.create_variable(IntegerSet::new_from_tuples(&[(-50, 50)]));
    let zero = env.create_variable(IntegerSet::new_from_tuples(&[(0, 0)]));

    let c1 = env.constrain(BinaryAddConstraint::new_with_coefficient(
        y,
        (-7).into(),
        x,
        r0,
    ));
    let c2 = env.constrain(BinaryAddConstraint::new_with_coefficient(
        y,
        2.into(),
        zero,
        t0,
    ));
    let c3 = env.constrain(BinaryAddConstraint::new_with_coefficient(
        x,
        5.into(),
        t0,
        r1,
    ));
    dbg!(
        c1,
        c2,
        c3,
        env.satisfied_constraints(env.current_generation())
    );
    assert_eq!(env.domain(x), IntegerSet::new_from_tuples(&[(-4, -4)]));
    assert_eq!(env.domain(y), IntegerSet::new_from_tuples(&[(1, 1)]));
}
*/
