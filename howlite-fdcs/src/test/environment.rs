use crate::{
    constraints::{BinaryAddConstraint, OffsetCompare},
    Constraint, Environment, IntegerSet,
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

impl Constraint for Box<dyn Constraint> {
    fn propogate(&mut self, ctx: crate::ConstraintContext, event: crate::Event) {
        self.as_mut().propogate(ctx, event)
    }

    fn initialize(&mut self, ctx: crate::ConstraintContext) {
        self.as_mut().initialize(ctx)
    }
}

#[test]
fn add() {
    let mut env: Environment<Box<dyn Constraint>> = Environment::new();
    let x = env.variables.create(IntegerSet::new(&[(0, 5)]));
    let y = env.variables.create(IntegerSet::new(&[(4, 6)]));
    let z = env.variables.create(IntegerSet::new(&[(7, 10)]));
    env.constrain(Box::new(BinaryAddConstraint::new(x, y, z)));
    assert_eq!(env.variables.get(x).clone(), IntegerSet::new(&[(3, 4)]));
    assert_eq!(env.variables.get(y).clone(), IntegerSet::new(&[(4, 6)]));
    assert_eq!(env.variables.get(z).clone(), IntegerSet::new(&[(7, 10)]));
}
