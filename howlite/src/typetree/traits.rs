use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol};
use howlite_typecheck::Ty;
use std::rc::Rc;

use super::constraint_term::ConstraintTerm;

/// Trait implemented on AST nodes to perform type synthesis, within the context of a program.
/// Type synthesis is the process of determining the smallest possible type that encapsulates all possible values of an expression.
/// For example:
///     synthesize_ty(`1 + 1`) -> `{2}`
///     synthesize_ty(`let x: Uint32; x + 1`) -> `Uint32`
pub trait SynthesizeTy {
    fn synthesize_ty<L: Clone>(self, ctx: &LexicalContext<'_, L>) -> Rc<Ty<Symbol>>;
}

/// Trait implemented on AST nodes that don't need any outer context to perform type synthesis.
pub trait SynthesizeTyPure {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>>;
}

impl<T: SynthesizeTyPure> SynthesizeTy for T {
    fn synthesize_ty<L: Clone>(self, _: &LexicalContext<'_, L>) -> Rc<Ty<Symbol>> {
        self.synthesize_ty_pure()
    }
}

pub trait ToContraintTerm {
    #[allow(dead_code)]
    fn to_constraint_term(self) -> ConstraintTerm;
}

pub trait PrepareLexicalCtx<L: Clone> {
    fn prepare_lexical_ctx<'a>(&self, ctx: LexicalContext<'a, L>) -> LexicalContext<'a, L>;
}
