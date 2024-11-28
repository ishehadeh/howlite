use std::{cmp::Ordering, collections::HashMap, ops, path::Display, rc::Weak};

use crate::symtab::Symbol;
use aries::{
    backtrack::Backtrack,
    core::{Lit, VarRef},
    model::{
        lang::{
            self,
            expr::{self, eq, gt, lt, or},
            linear::{LinearLeq, LinearSum},
            IAtom, IVar, SAtom, SVar,
        },
        Model,
    },
    reif::Reifiable,
    solver::Solver,
    utils::input::Sym,
};
use howlite_syntax::ast::InfixOp;
use preseli::IntegerSet;
use sunstone::{
    multi::DynSet,
    ops::{ArithmeticSet, Bounded, PartialBounded, SetSubtract, Union},
    step_range::StepRange,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstraintOp {
    Mul,
    Add,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]

pub enum ModelVarRef {
    HltVar(Symbol),
    StepRange(usize),
    Lit(usize),
}

impl std::fmt::Display for ModelVarRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModelVarRef::HltVar(symbol) => write!(f, "sym[{symbol:?}]"),
            ModelVarRef::StepRange(step) => write!(f, "step[{step}]"),
            ModelVarRef::Lit(lit) => write!(f, "lit[{lit}]"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Linear(LinearSum),
    Cond(Vec<Lit>),
}

pub enum Cmp {
    Leq,
    Lt,
    Geq,
    Gt,
    Eq,
    Ne,
}

impl Term {
    pub fn and(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Term::Cond(mut lhs), Term::Cond(rhs)) => {
                lhs.extend_from_slice(&rhs);
                Term::Cond(lhs)
            }
            _ => todo!(),
        }
    }

    fn cmp(self, rhs: Self, cmp: Cmp, model: &mut Model<ModelVarRef>) -> Self {
        let (lhs, rhs) = match (self, rhs) {
            (Term::Linear(lhs), Term::Linear(rhs)) => (lhs, rhs),
            _ => todo!(),
        };
        rhs.simplify();
        lhs.simplify();

        let linear_leq = match cmp {
            Cmp::Gt => LinearLeq::new(lhs - rhs, 1),
            Cmp::Lt => LinearLeq::new(lhs - rhs, -1),
            Cmp::Geq => LinearLeq::new(-(lhs - rhs), 0),
            Cmp::Leq => LinearLeq::new(lhs - rhs, 0),
            _ => todo!(),
        };
        Self::Cond(vec![model.reify(linear_leq)])
    }
}

impl ops::Add<Term> for Term {
    type Output = Term;

    fn add(self, rhs: Term) -> Self::Output {
        match (self, rhs) {
            (Term::Linear(lhs), Term::Linear(rhs)) => Self::Linear(lhs + rhs),
            (Term::Cond(_), _) | (_, Term::Cond(_)) => todo!(),
        }
    }
}

impl ops::Sub<Term> for Term {
    type Output = Term;

    fn sub(self, rhs: Term) -> Self::Output {
        match (self, rhs) {
            (Term::Linear(lhs), Term::Linear(rhs)) => Self::Linear(lhs - rhs),
            (Term::Cond(_), _) | (_, Term::Cond(_)) => todo!(),
        }
    }
}

impl ops::Mul<Term> for Term {
    type Output = Option<Term>;

    fn mul(self, rhs: Term) -> Self::Output {
        match (self, rhs) {
            (Term::Linear(lhs), Term::Linear(rhs)) => {
                if rhs.terms().is_empty() {
                    assert!(rhs.denom() == 1);

                    Some(Term::Linear(lhs * rhs.constant()))
                } else if lhs.terms().is_empty() {
                    assert!(lhs.denom() == 1);

                    Some(Term::Linear(rhs * lhs.constant()))
                } else {
                    None
                }
            }
            (Term::Cond(_), _) | (_, Term::Cond(_)) => todo!(),
        }
    }
}

pub struct ModelBuilder {
    tmp_var_index: usize,
    pub model: Model<ModelVarRef>,
}

impl ModelBuilder {
    pub fn new() -> Self {
        Self {
            tmp_var_index: 0,
            model: Model::new(),
        }
    }

    fn next_var_index(&mut self) -> usize {
        let i = self.tmp_var_index;
        self.tmp_var_index += 1;
        i
    }

    pub fn reify_term(&mut self, term: Term) -> Lit {
        match term {
            Term::Linear(linear_sum) => {
                linear_sum.simplify();
                self.model.reify(linear_sum.geq(1))
            }
            Term::Cond(vec) => self.model.reify(expr::and(vec)),
        }
    }

    fn constrain_via_stripes(&mut self, var: IVar, stripes: impl Iterator<Item = StepRange<i128>>) {
        let mut options = Vec::new();

        for step_range in stripes {
            if step_range.is_size_one() {
                options.push(self.model.reify(eq(var, *step_range.lo())));
            } else if *step_range.step() == 1i128 {
                options.push(self.model.reify(expr::and([
                    var.geq(step_range.lo().clone()),
                    var.leq(step_range.hi().clone()),
                ])))
            } else {
                let next_var_index = self.next_var_index();
                let step_var = self.model.new_ivar(
                    0,
                    step_range.size(),
                    ModelVarRef::StepRange(next_var_index),
                );
                options.push(self.model.reify(eq(
                    var,
                    (step_var * *step_range.step()).var() + *step_range.lo(),
                )))
            }
        }

        self.model.enforce(or(options), []);
    }

    pub fn add_set_to_model(&mut self, name: ModelVarRef, set: &IntegerSet) -> Term {
        let var = self
            .model
            .new_ivar(*set.get_range().lo(), *set.get_range().hi(), name);
        match set.inner() {
            sunstone::multi::DynSetData::Empty => todo!(),
            sunstone::multi::DynSetData::Small(s) => {
                self.constrain_via_stripes(var, s.stripes());
            }
            sunstone::multi::DynSetData::Contiguous => (),
            sunstone::multi::DynSetData::Stripe(stripe_set) => {
                self.constrain_via_stripes(var, stripe_set.stripes().cloned());
            }
        }

        Term::Linear(IAtom::new(var, 0).into())
    }

    pub fn add_lit(&mut self, set: &IntegerSet) -> Term {
        assert!(!set.is_empty());
        let bounds = set.partial_bounds().unwrap();

        if bounds.len() == 1 {
            Term::Linear(IAtom::new(IVar::ZERO, **bounds.lo()).into())
        } else {
            let next_var = self.next_var_index();
            self.add_set_to_model(ModelVarRef::Lit(next_var), set)
        }
    }

    pub fn add_lit_single(&mut self, set: i128) -> Term {
        Term::Linear(IAtom::new(IVar::ZERO, set).into())
    }

    pub fn add_var(&mut self, name: Symbol, ty: &IntegerSet) -> Term {
        self.add_set_to_model(ModelVarRef::HltVar(name), ty)
    }

    pub fn do_infix(&mut self, lhs: Term, op: InfixOp, rhs: Term) -> Option<Term> {
        match op {
            InfixOp::Add => Some(lhs + rhs),
            InfixOp::Sub => Some(lhs - rhs),

            InfixOp::Mul => lhs * rhs,
            InfixOp::CmpNe => todo!(),
            InfixOp::CmpEq => todo!(),
            InfixOp::CmpGt => Some(lhs.cmp(rhs, Cmp::Gt, &mut self.model)),
            InfixOp::CmpLt => Some(lhs.cmp(rhs, Cmp::Lt, &mut self.model)),
            InfixOp::CmpGtEq => Some(lhs.cmp(rhs, Cmp::Geq, &mut self.model)),
            InfixOp::CmpLtEq => Some(lhs.cmp(rhs, Cmp::Leq, &mut self.model)),
            InfixOp::LogicalAnd => Some(lhs.and(rhs)),
            _ => None,
        }
    }
}

impl Default for ModelBuilder {
    fn default() -> Self {
        Self::new()
    }
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
