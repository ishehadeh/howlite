use std::ops;

use crate::symtab::Symbol;
use aries::{
    core::Lit,
    model::{
        lang::{
            expr::{self, eq, geq, gt, leq, lt, neq, or},
            linear::{LinearLeq, LinearSum},
            IAtom, IVar,
        },
        Model,
    },
};
use howlite_syntax::ast::InfixOp;
use preseli::IntegerSet;
use sunstone::{
    ops::{Bounded, PartialBounded},
    step_range::StepRange,
};
use tracing::{instrument, trace};

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
    Atom(IAtom),
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
            (Term::Linear(lhs), Term::Atom(rhs)) => (lhs, LinearSum::of(vec![rhs])),
            (Term::Atom(lhs), Term::Linear(rhs)) => (LinearSum::of(vec![lhs]), rhs),
            (Term::Atom(lhs), Term::Atom(rhs)) => {
                trace!(?lhs, ?rhs, "reifying atom comparison");

                let lit = match cmp {
                    Cmp::Leq => model.reify(leq(lhs, rhs)),
                    Cmp::Lt => model.reify(lt(lhs, rhs)),
                    Cmp::Geq => model.reify(geq(lhs, rhs)),
                    Cmp::Gt => model.reify(gt(lhs, rhs)),
                    Cmp::Eq => model.reify(eq(lhs, rhs)),
                    Cmp::Ne => model.reify(neq(lhs, rhs)),
                };
                return Self::Cond(vec![lit]);
            }

            _ => todo!(),
        };

        let diff = (lhs.simplify() - rhs.simplify()).simplify();

        trace!(?diff, "simplified lhs - rhs");

        let linear_leq = match cmp {
            Cmp::Gt => LinearLeq::new(diff, 1),
            Cmp::Lt => LinearLeq::new(diff, -1),
            Cmp::Geq => LinearLeq::new(-diff, 0),
            Cmp::Leq => LinearLeq::new(diff, 0),
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
            (Term::Linear(lhs), Term::Atom(rhs)) => Self::Linear(lhs + rhs),
            (Term::Atom(lhs), Term::Linear(rhs)) => Self::Linear(rhs + lhs),
            (Term::Atom(lhs), Term::Atom(rhs)) => Self::Linear(LinearSum::of(vec![lhs, rhs])),
        }
    }
}

impl ops::Sub<Term> for Term {
    type Output = Term;

    fn sub(self, rhs: Term) -> Self::Output {
        match (self, rhs) {
            (Term::Linear(lhs), Term::Linear(rhs)) => Self::Linear(lhs - rhs),
            (Term::Cond(_), _) | (_, Term::Cond(_)) => todo!(),
            (Term::Linear(lhs), Term::Atom(rhs)) => Self::Linear(lhs - rhs),
            (Term::Atom(lhs), Term::Linear(rhs)) => Self::Linear(-rhs + lhs),
            (Term::Atom(lhs), Term::Atom(rhs)) => Self::Linear(LinearSum::of(vec![lhs, rhs])),
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
            _ => todo!(),
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
            Term::Atom(a) => self.model.reify(a.ge_lit(1)),
            Term::Linear(linear_sum) => {
                linear_sum.simplify();
                self.model.reify(linear_sum.geq(1))
            }

            Term::Cond(vec) => self.model.reify(expr::and(vec)),
        }
    }

    #[instrument(skip(self, stripes))]
    fn constrain_via_stripes(&mut self, var: IVar, stripes: impl Iterator<Item = StepRange<i128>>) {
        let mut options = Vec::new();

        for step_range in stripes {
            trace!(?step_range, "handling stripe");
            if step_range.is_size_one() {
                options.push(self.model.reify(eq(var, *step_range.lo())));
            } else if *step_range.step() == 1i128 {
                options.push(self.model.reify(expr::and([
                    var.geq(*step_range.lo()),
                    var.leq(*step_range.hi()),
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

        Term::Atom(IAtom::new(var, 0))
    }

    pub fn add_lit(&mut self, set: &IntegerSet) -> Term {
        assert!(!set.is_empty());
        let bounds = set.partial_bounds().unwrap();

        if bounds.len() == 1 {
            Term::Atom(IAtom::new(IVar::ZERO, **bounds.lo()))
        } else {
            let next_var = self.next_var_index();
            self.add_set_to_model(ModelVarRef::Lit(next_var), set)
        }
    }

    pub fn add_lit_single(&mut self, set: i128) -> Term {
        trace!(value = set, "adding constant ivar");
        Term::Atom(IAtom::new(IVar::ZERO, set))
    }

    pub fn add_var(&mut self, name: Symbol, ty: &IntegerSet) -> Term {
        trace!(value = ?ty, ?name, "adding variable");
        self.add_set_to_model(ModelVarRef::HltVar(name), ty)
    }

    #[instrument(skip(self))]
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
