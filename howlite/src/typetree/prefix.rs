use std::rc::Rc;

use howlite_syntax::ast::{ExprPrefix, PrefixOp};
use howlite_typecheck::{shape::TypeShape, types::TyInt, Ty};
use preseli::IntegerSet;
use sunstone::ops::{PartialBounded, SetOpIncludeExclude, SetOpIncludes};
use tracing::trace_span;

use crate::{langctx::lexicalctx::LexicalContext, CompilationErrorKind};

use super::SynthesizeTy;

impl SynthesizeTy for ExprPrefix {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<crate::symtab::Symbol>> {
        let term_ty = ctx.child(self.rhs).synthesize_ty();
        let expected_shape = if self.op == PrefixOp::Deref {
            TypeShape::REFERENCE
        } else {
            TypeShape::INTEGER
        };
        if term_ty.shape() != expected_shape {
            ctx.error(CompilationErrorKind::PrefixOpNotApplicable {
                prefix: self.op,
                expected: expected_shape,
                got: term_ty,
            });
            return Rc::new(Ty::Int(match self.op {
                PrefixOp::LogicalNot => TyInt::from_set(IntegerSet::new_from_range(0, 1)),
                PrefixOp::Minus => TyInt::i64(),
                PrefixOp::Plus => TyInt::i64(),
                PrefixOp::BitNot => TyInt::u64(),
                PrefixOp::Deref => return Rc::new(Ty::Hole),
            }));
        }

        let span = trace_span!("ExprPrefix", op = ?self.op, ty = ?term_ty);
        let _guard = span.enter();
        match self.op {
            PrefixOp::LogicalNot => {
                let mut output = IntegerSet::empty();
                let term_values = &term_ty.as_int().unwrap().values;
                // LogicalNot maps 0 -> 1
                if term_values.includes(0i128) {
                    output.include_mut(1);
                    if term_values.partial_bounds().unwrap().len() > 1 {
                        // more than just 0 => theres some non-zero number
                        // which would be mapped to 0
                        output.include_mut(0);
                    }
                } else {
                    output.include_mut(0);
                }
                Ty::Int(TyInt::from_set(output)).into()
            }
            PrefixOp::Minus => {
                let mut neg = term_ty.as_int().unwrap().values.clone();
                neg.neg_mut();
                Ty::Int(TyInt::from_set(neg)).into()
            }
            PrefixOp::Plus => term_ty,
            PrefixOp::BitNot => todo!(),
            PrefixOp::Deref => term_ty.as_reference().unwrap().referenced_ty.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    use howlite_syntax::ast::{ExprPrefix, PrefixOp};
    use howlite_typecheck::t_int;

    use crate::{get_node_type, typetree::test_helpers::make_ty_number_range};

    #[test]
    fn neg_simple() {
        let ty = get_node_type!(boxed
            ExprPrefix { op: PrefixOp::Minus, rhs: make_ty_number_range(-5, 100) }
        );
        assert_eq!(ty, t_int!(-100..5))
    }
}
