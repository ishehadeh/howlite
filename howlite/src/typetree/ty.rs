//! Type Tree trait impls for Ty* nodes

use std::rc::Rc;

use howlite_syntax::{
    ast::{self},
    AstNodeData,
};
use howlite_typecheck::{
    types::{StructField, TyInt, TyStruct},
    Ty, TyArray,
};
use preseli::IntegerSet;
use smallvec::SmallVec;
use sunstone::ops::{Bounded, PartialBounded};

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol, CompilationErrorKind};

use super::{SynthesizeTy, SynthesizeTyPure};

impl SynthesizeTyPure for ast::TyUnit {
    fn synthesize_ty_pure(&self) -> Rc<Ty<Symbol>> {
        Rc::new(Ty::unit())
    }
}

impl SynthesizeTy for ast::TyArray {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let element_ty = ctx.child(self.element_ty).synthesize_ty();
        assert!(self.length <= usize::MAX as i128 && self.length >= 0);
        Rc::new(Ty::Array(TyArray {
            length: self.length as usize,
            element_ty,
        }))
    }
}

impl SynthesizeTy for ast::TyExprUnion {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let lhs_ty = ctx.child(self.lhs).synthesize_ty();
        let rhs_ty = ctx.child(self.rhs).synthesize_ty();
        Ty::union(&[lhs_ty, rhs_ty])
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

    use crate::{
        get_node_type,
        typetree::test_helpers::{
            any_ty_number_range_with_literal, any_ty_struct_with_literal_scalars,
        },
    };

    proptest!(
        #[test]
        fn ty_number_range(program in any_ty_number_range_with_literal()) {
            let ty = get_node_type!(program);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }

        #[test]
        fn synthesize_ty_struct_succeedes(program in any_ty_struct_with_literal_scalars()) {
            let ty = get_node_type!(program);
            assert!(ty.as_struct().is_some(), "expected struct type, got: {:?}", ty);
        }
    );
}
