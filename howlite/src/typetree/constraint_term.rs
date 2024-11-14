use preseli::IntegerSet;
use sunstone::{
    multi::DynSet,
    ops::{ArithmeticSet, Bounded, PartialBounded, SetSubtract, Union},
};

use crate::symtab::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstraintOp {
    Mul,
    Add,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryConstraintRelation {
    Lt,
    Eq,
    Gt,
    Ne,
}

impl BinaryConstraintRelation {
    fn to_set(&self, rhs: preseli::IntegerSet) -> preseli::IntegerSet {
        match self {
            BinaryConstraintRelation::Lt => rhs
                .partial_bounds()
                .map(|b| IntegerSet::new_from_range(i128::MIN, *b.lo() - 1))
                .unwrap_or(IntegerSet::empty()),
            BinaryConstraintRelation::Eq => rhs,
            BinaryConstraintRelation::Gt => rhs
                .partial_bounds()
                .map(|b| IntegerSet::new_from_range(*b.hi() + 1, i128::MAX))
                .unwrap_or(IntegerSet::empty()),
            BinaryConstraintRelation::Ne => {
                let mut complement = IntegerSet::new_from_range(i128::MIN, i128::MAX);
                complement.set_subtract_mut(&rhs);
                complement
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstraintTerm {
    Literal(preseli::IntegerSet),
    Var(Symbol),
    UnaryConstraint {
        var: Symbol,
        superset: preseli::IntegerSet,
    },
    BinaryConstraint {
        lhs: Symbol,
        lhs_offset: preseli::IntegerSet,
        relation: BinaryConstraintRelation,
        rhs: Symbol,
    },
    UnaryOperation {
        var: Symbol,
        op: ConstraintOp,
        value: preseli::IntegerSet,
    },
    BinaryOperation {
        lhs: Symbol,
        lhs_offset: preseli::IntegerSet,
        op: ConstraintOp,
        rhs: Symbol,
    },
}

impl ConstraintTerm {
    pub fn unary_constraint_from_relation(
        var: Symbol,
        relation: BinaryConstraintRelation,
        rhs: IntegerSet,
    ) -> Self {
        ConstraintTerm::UnaryConstraint {
            var,
            superset: relation.to_set(rhs),
        }
    }
    pub fn compare_literal(
        &mut self,
        relation: BinaryConstraintRelation,
        rhs: preseli::IntegerSet,
    ) {
        match self {
            ConstraintTerm::Literal(_) => todo!(),
            ConstraintTerm::Var(symbol) => {
                *self = Self::unary_constraint_from_relation(*symbol, relation, rhs)
            }
            ConstraintTerm::UnaryConstraint { .. } => todo!(),
            ConstraintTerm::BinaryConstraint { .. } => todo!(),
            ConstraintTerm::UnaryOperation {
                var,
                op: ConstraintOp::Add,
                value,
            } => {
                // move literals to the same side, then make this a unary constraint
                let mut new_rhs_set = rhs;
                new_rhs_set.sub_all(value);
                *self = Self::unary_constraint_from_relation(*var, relation, new_rhs_set)
            }
            ConstraintTerm::UnaryOperation {
                var,
                op: ConstraintOp::Mul,
                value,
            } => {
                // move literals to the same side, then make this a unary constraint

                if matches!(
                    relation,
                    BinaryConstraintRelation::Eq | BinaryConstraintRelation::Ne
                ) {
                    let mut new_rhs_set = rhs;
                    new_rhs_set.div_all(value);
                    *self = Self::unary_constraint_from_relation(*var, relation, new_rhs_set)
                } else {
                    let inverse_relation = match relation {
                        BinaryConstraintRelation::Lt => BinaryConstraintRelation::Gt,
                        BinaryConstraintRelation::Gt => BinaryConstraintRelation::Lt,
                        BinaryConstraintRelation::Eq | BinaryConstraintRelation::Ne => {
                            unreachable!()
                        }
                    };
                    let mut ge_zero = rhs;
                    let mut lt_zero = ge_zero.take_below(&0);

                    let ge_zero_constraint = if !ge_zero.is_empty() {
                        ge_zero.div_all(value);

                        Some(inverse_relation.to_set(ge_zero))
                    } else {
                        None
                    };
                    let lt_zero_constraint = if !lt_zero.is_empty() {
                        lt_zero.div_all(value);
                        Some(inverse_relation.to_set(lt_zero))
                    } else {
                        None
                    };
                    let set = ge_zero_constraint
                        .into_iter()
                        .chain(lt_zero_constraint.into_iter())
                        .reduce(|last, next| last.union(next))
                        .unwrap_or(DynSet::empty());
                    *self = ConstraintTerm::UnaryConstraint {
                        var: *var,
                        superset: set,
                    }
                }
            }
            ConstraintTerm::BinaryOperation { .. } => {
                todo!("compare_literal: ConstraintTerm::BinaryOperation")
            }
        }
    }
    pub fn apply_literal(&mut self, op: ConstraintOp, lit: preseli::IntegerSet) {
        match self {
            ConstraintTerm::Var(s) => {
                *self = ConstraintTerm::UnaryOperation {
                    var: *s,
                    op: op,
                    value: lit,
                }
            }
            ConstraintTerm::Literal(s) => match op {
                ConstraintOp::Mul => s.mul_all(&lit),
                ConstraintOp::Add => s.add_all(&lit),
            },
            // TODO: mark as dependent on the value of var
            ConstraintTerm::UnaryConstraint { .. } => todo!(),
            ConstraintTerm::BinaryConstraint { .. } => todo!(),

            ConstraintTerm::UnaryOperation {
                var: _,
                op: existing_op,
                value,
            } if *existing_op == op => {
                value.mul_all(&lit);
            }
            ConstraintTerm::UnaryOperation { op: _, .. } => {
                todo!()
            }
            ConstraintTerm::BinaryOperation { .. } => todo!(),
        }
    }

    pub fn apply_var(&mut self, op: ConstraintOp, var: Symbol) {
        match self {
            ConstraintTerm::Var(s) => {
                *self = ConstraintTerm::BinaryOperation {
                    lhs: *s,
                    lhs_offset: IntegerSet::new_from_range(1, 1),
                    op,
                    rhs: var,
                }
            }
            ConstraintTerm::Literal(s) => {
                *self = ConstraintTerm::UnaryOperation {
                    var,
                    op,
                    value: s.clone(),
                }
            }
            // TODO: mark as dependent on the value of var
            ConstraintTerm::UnaryConstraint { .. } => todo!(),
            ConstraintTerm::BinaryConstraint { .. } => todo!(),

            ConstraintTerm::UnaryOperation {
                var: lhs,
                op: ConstraintOp::Add,
                value,
            } => {
                *self = ConstraintTerm::BinaryOperation {
                    lhs: *lhs,
                    lhs_offset: value.clone(),
                    op,
                    rhs: var,
                }
            }
            ConstraintTerm::UnaryOperation { op: _, .. } => {
                todo!()
            }
            ConstraintTerm::BinaryOperation { .. } => todo!(),
        }
    }
}
