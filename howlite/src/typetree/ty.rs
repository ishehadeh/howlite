//! Type Tree trait impls for Ty* nodes

use std::rc::Rc;

use howlite_syntax::{ast, AstNode, Span};
use howlite_typecheck::{types::TyInt, Ty};
use preseli::IntegerSet;
use sunstone::ops::{Bounded, PartialBounded};

use crate::{langctx::LangCtx, symtab::Symbol, CompilationError, CompilationErrorKind};

use super::{SynthesizeTy, SynthesizeTyPure};

impl SynthesizeTyPure for AstNode<ast::TyUnit> {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        Rc::new(Ty::unit())
    }
}

impl SynthesizeTy<Span> for AstNode<ast::TyNumberRange<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        // check that the bound is an integer set with a single set
        // returns Some(i128) if valid, none otherwise
        let validate_bound = |bound: &Rc<Ty<Symbol>>| {
            bound
                .as_int()
                .iter()
                .flat_map(|&v| {
                    v.values
                        .partial_bounds()
                        .filter(|b| b.len() == 0)
                        .map(|b| **b.lo())
                })
                .next()
        };

        let lo = validate_bound(&self.data.lo);
        let hi = validate_bound(&self.data.hi);

        if lo.is_none() {
            ctx.error(CompilationError {
                location: self.span,
                kind: CompilationErrorKind::InvalidIntegerBound {
                    got: self.data.lo.clone(),
                },
            });
        }

        if hi.is_none() {
            ctx.error(CompilationError {
                location: self.span,
                kind: CompilationErrorKind::InvalidIntegerBound {
                    got: self.data.hi.clone(),
                },
            });
        }
        match (lo, hi) {
            (Some(lo), Some(hi)) => {
                Rc::new(Ty::Int(TyInt::from_set(IntegerSet::new_from_range(lo, hi))))
            }
            _ => Rc::new(Ty::Hole),
        }
    }
}
