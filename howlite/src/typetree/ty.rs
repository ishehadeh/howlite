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

#[cfg(test)]
mod test {
    use howlite_syntax::{ast::BoxAstNode, Span};
    use proptest::{prelude::Strategy, proptest};

    use crate::{
        assert_lang_ok,
        langctx::LangCtx,
        typetree::{test_helpers::make_ty_number_range, SynthesizeTy},
    };

    proptest!(
        #[test]
        fn ty_number_range(program in any_ty_number_range_with_literal()) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert_lang_ok!(lang);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }
    );

    fn any_ty_number_range_with_literal() -> impl Strategy<Value = BoxAstNode> {
        (0..u64::MAX as i128, 0..u64::MAX as i128).prop_map(|(a, b)| make_ty_number_range(a, b))
    }
}
