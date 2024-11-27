use preseli::{environment::Constraint, IntegerSet};
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
    fn to_set(&self, rhs: &preseli::IntegerSet) -> preseli::IntegerSet {
        match self {
            BinaryConstraintRelation::Lt => rhs
                .partial_bounds()
                .map(|b| IntegerSet::new_from_range(i64::MIN as i128, *b.lo() - 1))
                .unwrap_or(IntegerSet::empty()),
            BinaryConstraintRelation::Eq => rhs.clone(),
            BinaryConstraintRelation::Gt => rhs
                .partial_bounds()
                .map(|b| IntegerSet::new_from_range(*b.hi() + 1, u64::MAX as i128))
                .unwrap_or(IntegerSet::empty()),
            BinaryConstraintRelation::Ne => {
                let mut complement = IntegerSet::new_from_range(i64::MIN as i128, u64::MAX as i128);
                complement.set_subtract_mut(&rhs);
                complement
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstraintTerm {
    NotApplicable,
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
        rhs: &IntegerSet,
    ) -> Self {
        ConstraintTerm::UnaryConstraint {
            var,
            superset: relation.to_set(&rhs),
        }
    }

    pub fn compare_literal(
        &mut self,
        relation: BinaryConstraintRelation,
        rhs: &preseli::IntegerSet,
    ) {
        match self {
            ConstraintTerm::NotApplicable => (),
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
                let mut new_rhs_set = rhs.clone();
                new_rhs_set.sub_all(value);
                *self = Self::unary_constraint_from_relation(*var, relation, &new_rhs_set)
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
                    let mut new_rhs_set = rhs.clone();
                    new_rhs_set.div_all(value);
                    *self = Self::unary_constraint_from_relation(*var, relation, &new_rhs_set)
                } else {
                    let inverse_relation = match relation {
                        BinaryConstraintRelation::Lt => BinaryConstraintRelation::Gt,
                        BinaryConstraintRelation::Gt => BinaryConstraintRelation::Lt,
                        BinaryConstraintRelation::Eq | BinaryConstraintRelation::Ne => {
                            unreachable!()
                        }
                    };
                    let mut ge_zero = rhs.clone();
                    let mut lt_zero = ge_zero.take_below(&0);

                    let ge_zero_constraint = if !ge_zero.is_empty() {
                        ge_zero.div_all(value);

                        Some(inverse_relation.to_set(&ge_zero))
                    } else {
                        None
                    };
                    let lt_zero_constraint = if !lt_zero.is_empty() {
                        lt_zero.div_all(value);
                        Some(inverse_relation.to_set(&lt_zero))
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

    pub fn compare_term(&self, op: BinaryConstraintRelation, rhs: &Self) -> Self {
        match (self, rhs) {
            (lhs, ConstraintTerm::Literal(rhs_lit)) => {
                let mut lhs = lhs.clone();
                lhs.compare_literal(op, rhs_lit);
                lhs
            }
            (&ConstraintTerm::Var(lhs), &ConstraintTerm::Var(rhs)) => {
                ConstraintTerm::BinaryConstraint {
                    lhs,
                    lhs_offset: DynSet::new_from_range(0, 0),
                    relation: op,
                    rhs,
                }
            }
            a => todo!("handle constraint variants: {:#?}", a),
        }
    }

    pub fn apply_term(&self, op: ConstraintOp, rhs: &Self) -> Self {
        match (self, rhs) {
            (ConstraintTerm::NotApplicable, _) => ConstraintTerm::NotApplicable,
            (_, ConstraintTerm::NotApplicable) => ConstraintTerm::NotApplicable,
            (ConstraintTerm::Literal(lit), set) | (set, ConstraintTerm::Literal(lit)) => {
                let mut set = set.clone();
                set.apply_literal(op, lit);
                set
            }
            (ConstraintTerm::Var(var), set) | (set, ConstraintTerm::Var(var)) => {
                let mut set = set.clone();
                set.apply_var(op, *var);
                set
            }
            (ConstraintTerm::UnaryConstraint { .. }, _)
            | (_, ConstraintTerm::UnaryConstraint { .. }) => todo!("constraint as operand (unary)"),
            (ConstraintTerm::BinaryConstraint { .. }, _)
            | (_, ConstraintTerm::BinaryConstraint { .. }) => {
                todo!("constraint as operand (binary)")
            }

            (
                ConstraintTerm::UnaryOperation {
                    var: lvar,
                    op: lop,
                    value: lvalue,
                },
                ConstraintTerm::UnaryOperation {
                    var: rvar,
                    op: rop,
                    value: rvalue,
                },
            ) => {
                if op == ConstraintOp::Mul {
                    todo!("(x + a)(y + b) not supported, make this a nice error")
                } else {
                    #[allow(clippy::redundant_else)]
                    if lop == rop && *rop == ConstraintOp::Add {
                        let mut lvalue = lvalue.clone();
                        lvalue.add_all(&rvalue);
                        ConstraintTerm::BinaryOperation {
                            lhs: lvar.clone(),
                            lhs_offset: lvalue,
                            op: ConstraintOp::Add,
                            rhs: rvar.clone(),
                        }
                    } else {
                        todo!("arbitrary multipliers for variables (big TODO haha)")
                    }
                }
            }
            (ConstraintTerm::UnaryOperation { .. }, ConstraintTerm::BinaryOperation { .. })
            | (ConstraintTerm::BinaryOperation { .. }, ConstraintTerm::UnaryOperation { .. }) => {
                todo!("binary and unary operands can't be applied together, make this a nice error")
            }
            (ConstraintTerm::BinaryOperation { .. }, ConstraintTerm::BinaryOperation { .. }) => {
                todo!("two binary operands can't be applied together, make this a nice error")
            }
        }
    }

    pub fn apply_literal(&mut self, op: ConstraintOp, lit: &preseli::IntegerSet) {
        match self {
            ConstraintTerm::NotApplicable => (),
            ConstraintTerm::Var(s) => {
                *self = ConstraintTerm::UnaryOperation {
                    var: *s,
                    op,
                    value: lit.clone(),
                }
            }
            ConstraintTerm::Literal(s) => match op {
                ConstraintOp::Mul => s.mul_all(lit),
                ConstraintOp::Add => s.add_all(lit),
            },
            // TODO: mark as dependent on the value of var
            ConstraintTerm::UnaryConstraint { .. } => todo!(),
            ConstraintTerm::BinaryConstraint { .. } => todo!(),

            ConstraintTerm::UnaryOperation {
                var: _,
                op: existing_op,
                value,
            } if *existing_op == op => match op {
                ConstraintOp::Mul => value.mul_all(&lit),
                ConstraintOp::Add => value.add_all(&lit),
            },
            ConstraintTerm::UnaryOperation { op: _, .. } => {
                todo!()
            }
            ConstraintTerm::BinaryOperation { .. } => todo!(),
        }
    }

    pub fn apply_var(&mut self, op: ConstraintOp, var: Symbol) {
        match self {
            ConstraintTerm::NotApplicable => (),
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
