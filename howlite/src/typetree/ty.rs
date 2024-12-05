//! Type Tree trait impls for Ty* nodes

use std::rc::Rc;

use howlite_syntax::{
    ast::{self, TyNamed},
    AstNodeData,
};
use howlite_typecheck::{
    types::{StorageClass, StructField, TyInt, TyStruct},
    Ty, TyArray, TyReference, TySlice,
};
use preseli::IntegerSet;
use smallvec::SmallVec;
use sunstone::{
    multi::DynSet,
    ops::{Bounded, PartialBounded},
};
use tracing::debug;

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

impl SynthesizeTy for ast::TySlice {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let element_ty = ctx.child(self.element_ty).synthesize_ty();
        let len_ty = ctx.child(self.length_ty).synthesize_ty();
        if Ty::unwrap_late_bound(len_ty.clone()).as_int().is_none() {
            ctx.error(CompilationErrorKind::InvalidSliceLengthTy(len_ty));
            Rc::new(Ty::Slice(TySlice {
                index_set: Rc::new(Ty::Int(TyInt::from_set(IntegerSet::new_from_range(
                    0i128,
                    u64::MAX as i128,
                )))),
                element_ty,
            }))
        } else {
            Rc::new(Ty::Slice(TySlice {
                index_set: len_ty,
                element_ty,
            }))
        }
    }
}

impl SynthesizeTy for TyNamed {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let name_sym = ctx.sym_intern(&self.name);
        let params = self
            .parameters
            .iter()
            .map(|p| ctx.child(*p).synthesize_ty())
            .collect::<Vec<_>>();
        let special_ints = ["Max", "Min", "u32", "s32", "s16", "s8", "s16", "s8", "u8"];
        if special_ints.contains(&self.name.as_str()) {
            debug!(inner = ?params[0], name = ?self.name, "instantiating special int type");
            let p0 = &params[0];
            let int = match &**p0 {
                Ty::Hole => return Rc::new(Ty::Hole),
                Ty::Int(ty_int) => ty_int,
                Ty::LateBound(ty_late_bound) => ty_late_bound.ty.as_int().unwrap(),
                _ => panic!("TODO: nice error"),
            };
            if self.name == "Max" {
                let hi = *int.values.partial_bounds().unwrap().hi();
                return Rc::new(Ty::Int(TyInt {
                    values: DynSet::new_from_range(*hi, *hi),
                    storage: int.storage.clone(),
                }));
            } else if self.name == "Min" {
                let lo = *int.values.partial_bounds().unwrap().lo();
                return Rc::new(Ty::Int(TyInt {
                    values: DynSet::new_from_range(*lo, *lo),
                    storage: int.storage.clone(),
                }));
            } else {
                let signed = self.name.chars().nth(0).unwrap() == 's';
                let bits: usize = self.name[1..].parse().unwrap();
                debug!(?bits, ?signed, "instantiating storage class type");

                return Rc::new(Ty::Int(TyInt {
                    values: int.values.clone(),
                    storage: StorageClass {
                        is_signed: signed,
                        bits,
                    },
                }));
            }
        }

        match ctx.ty_get(name_sym) {
            Some(v) => v.instantiate(ctx, &params),
            None => {
                ctx.error(CompilationErrorKind::UnknownType {
                    name: self.name.clone(),
                });
                Rc::new(Ty::Hole)
            }
        }
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
        let hi_ty = ctx.child(self.hi).synthesize_ty();

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

impl SynthesizeTy for ast::TyRef {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        let referenced_ty = ctx.child(self.referenced_ty).synthesize_ty();
        Rc::new(Ty::Reference(TyReference { referenced_ty }))
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use howlite_syntax::ast::TyNamed;
    use howlite_typecheck::{t_int, Ty, TyReference, TySlice};
    use proptest::proptest;

    use crate::{
        get_node_type,
        typetree::test_helpers::{
            any_ty_number_range_with_literal, any_ty_struct_with_literal_scalars,
            make_reference_ty, make_ty_slice,
        },
    };

    proptest!(
        #[test]
        fn ty_number_range(program in any_ty_number_range_with_literal()) {
            let ty = get_node_type!(program);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }

        #[test]
        fn synthesize_ty_slice_sanity(element_ty_node in any_ty_number_range_with_literal(), length_ty_node in any_ty_number_range_with_literal()) {
            let element_ty = get_node_type!(element_ty_node.clone());
            let length_ty = get_node_type!(length_ty_node.clone());
            let slice_ty_node = make_ty_slice(element_ty_node, length_ty_node);
            let slice_ty = get_node_type!(slice_ty_node);
            assert_eq!(slice_ty, Rc::new(Ty::Slice(TySlice { index_set: length_ty, element_ty })));

        }


        #[test]
        fn synthesize_ty_struct_succeedes(program in any_ty_struct_with_literal_scalars()) {
            let ty = get_node_type!(program);
            assert!(ty.as_struct().is_some(), "expected struct type, got: {:?}", ty);
        }

        #[test]
        fn synthesize_ty_ref_sanity(referenced_ty_node in any_ty_number_range_with_literal()) {
            let referenced_ty = get_node_type!(referenced_ty_node.clone());
            let ty_ref_node = make_reference_ty(referenced_ty_node);
            let ty = get_node_type!(ty_ref_node);
            assert_eq!(ty.clone(), Ty::Reference(TyReference { referenced_ty, }).into());
        }
    );

    #[test]
    fn retrieve_named_ty_simple() {
        let ty = get_node_type!(boxed
            TyNamed {
                name: "test".into(),
                parameters: Default::default()
            },
            type test = t_int!(0..5)
        );
        assert_eq!(ty, t_int!(0..5))
    }
}
