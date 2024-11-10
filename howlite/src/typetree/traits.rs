use crate::{langctx::LangCtx, symtab::Symbol};
use howlite_typecheck::Ty;
use preseli::variables::VariableId;
use std::rc::Rc;
use sunstone::multi::DynSet;

/// Trait implemented on AST nodes to perform type synthesis, within the context of a program.
/// Type synthesis is the process of determining the smallest possible type that encapsulates all possible values of an expression.
/// For example:
///     synthesize_ty(`1 + 1`) -> `{2}`
///     synthesize_ty(`let x: Uint32; x + 1`) -> `Uint32`
pub trait SynthesizeTy<L> {
    fn synthesize_ty(self, ctx: &LangCtx<L>) -> Rc<Ty<Symbol>>;
}

/// Trait implemented on AST nodes that don't need any outer context to perform type synthesis.
pub trait SynthesizeTyPure {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>>;
}

impl<T: SynthesizeTyPure, L> SynthesizeTy<L> for T {
    fn synthesize_ty(self, _: &LangCtx<L>) -> Rc<Ty<Symbol>> {
        self.synthesize_ty_pure()
    }
}

pub trait ToContraintTerm {
    fn to_constraint_term(self) -> ConstraintTerm;
}

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

#[derive(Debug, Clone)]
pub enum ConstraintTerm {
    Literal(preseli::IntegerSet),
    UnaryConstraint {
        var: Symbol,
        superset: preseli::IntegerSet,
    },
    BinaryConstraint {
        rhs: Symbol,
        relation: BinaryConstraintRelation,
        lhs: Symbol,
        lhs_value: preseli::IntegerSet,
    },
    UnaryOperation {
        var: Symbol,
        op: ConstraintOp,
        value: preseli::IntegerSet,
    },
    BinaryOperation {
        lhs: Symbol,
        op: ConstraintOp,
        rhs: Symbol,
    },
}
