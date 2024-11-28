//! Type Tree trait impls for Ty* nodes

use std::rc::Rc;

use howlite_syntax::{
    ast::{self},
    AstNode, AstNodeData,
};
use howlite_typecheck::{
    types::{StructField, TyInt, TyStruct},
    Ty,
};
use preseli::IntegerSet;
use smallvec::SmallVec;
use sunstone::ops::{Bounded, PartialBounded};

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol, CompilationErrorKind};

use super::{SynthesizeTy, SynthesizeTyPure};

impl SynthesizeTyPure for AstNode<ast::TyUnit> {
    fn synthesize_ty_pure(&self) -> Rc<Ty<Symbol>> {
        Rc::new(Ty::unit())
    }
}

impl SynthesizeTy for ast::TyStruct {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let mut ty = TyStruct {
            fields: SmallVec::with_capacity(self.members.len()),
        };
        for member in &self.members {
            let node = ctx.get_node(*member);
            let (name, member_ty_node) = match &node.data {
                AstNodeData::TyStructMember(member) => (member.name.clone(), member.ty),
                _ => panic!("TyStruct node did not have a TyStructMember as a child: {node:?}"),
            };
            let member_ty = ctx.child(member_ty_node).synthesize_ty();
            ty.fields.push(StructField {
                name: ctx.sym_intern(&name),
                ty: member_ty,
            });
        }

        Rc::new(Ty::Struct(ty))
    }
}

impl SynthesizeTy for ast::TyNumberRange {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
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
        let lo_ty = ctx.child(self.lo).synthesize_ty();
        let hi_ty = ctx.child(self.lo).synthesize_ty();

        let lo = validate_bound(&lo_ty);
        let hi = validate_bound(&hi_ty);

        if lo.is_none() {
            ctx.error(CompilationErrorKind::InvalidIntegerBound { got: lo_ty.clone() })
        }

        if hi.is_none() {
            ctx.error(CompilationErrorKind::InvalidIntegerBound { got: hi_ty.clone() })
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
    use howlite_syntax::ast::BoxAstNode;
    use proptest::{prelude::Strategy, proptest};

    use crate::{get_node_type, typetree::test_helpers::make_ty_number_range};

    proptest!(
        #[test]
        fn ty_number_range(program in any_ty_number_range_with_literal()) {
            let ty = get_node_type!(program);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }
    );

    fn any_ty_number_range_with_literal() -> impl Strategy<Value = BoxAstNode> {
        (0..u64::MAX as i128, 0..u64::MAX as i128).prop_map(|(a, b)| make_ty_number_range(a, b))
    }
}
