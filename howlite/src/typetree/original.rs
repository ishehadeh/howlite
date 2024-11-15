use std::rc::Rc;

use howlite_syntax::{
    ast::{ExprLet, LiteralArray, LiteralChar, LiteralInteger, LiteralString, LiteralStruct},
    AstNode, Span,
};
use howlite_typecheck::{
    types::{self, StorageClass, TyInt, TyUnion},
    Ty, TyArray,
};
use preseli::IntegerSet;
use smallvec::SmallVec;

use crate::{
    langctx::{LangCtx, VarDef},
    symtab::Symbol,
};

use super::{SynthesizeTy, SynthesizeTyPure};

impl SynthesizeTyPure for LiteralInteger {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        Rc::new(Ty::Int(TyInt::single(self.value)))
    }
}

impl SynthesizeTyPure for AstNode<&LiteralString> {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        let bytes = self.data.value.as_bytes();
        let values = IntegerSet::new_from_individual_generic(bytes);
        let element_ty = Rc::new(Ty::Int(TyInt {
            values,
            storage: StorageClass::unsigned(8),
        }));

        Rc::new(Ty::Array(TyArray {
            length: self.data.value.len(),
            element_ty,
        }))
    }
}

impl SynthesizeTyPure for &AstNode<&LiteralChar> {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        let c = self.data.value;
        let values = IntegerSet::new_from_individual(&[c as i128]);
        Rc::new(Ty::Int(TyInt {
            values,
            storage: StorageClass::unsigned(32),
        }))
    }
}

impl SynthesizeTyPure for AstNode<LiteralArray<Rc<Ty<Symbol>>>> {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        let union = TyUnion {
            tys: SmallVec::from(self.data.values.as_ref()),
        };

        Rc::new(Ty::Array(TyArray {
            length: self.data.values.len(),
            element_ty: Rc::new(Ty::Union(union)),
        }))
    }
}

impl SynthesizeTy<Span> for AstNode<LiteralStruct<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let ty = types::TyStruct {
            fields: self
                .data
                .members
                .into_iter()
                .map(|child| types::StructField {
                    name: ctx.symbols.intern(child.data.field.as_str()),
                    ty: child.data.value.clone(),
                })
                .collect(),
        };

        Rc::new(Ty::Struct(ty))
    }
}

impl SynthesizeTy<Span> for AstNode<ExprLet<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let var_symbol = ctx.symbols.intern(self.data.name.as_str());
        let var_ty = self.data.ty;
        let var_value_ty = self.data.value;
        ctx.var_def(
            ctx.root_scope_id,
            var_symbol,
            VarDef {
                assumed_ty: var_ty,
                last_assignment: var_value_ty.clone(),
            },
        );

        var_value_ty
    }
}

#[cfg(test)]
mod test {
    use crate::{
        assert_lang_ok,
        typetree::{
            test_helpers::{
                any_ident, any_literal, literal_array, literal_struct, simple_scalar_let,
            },
            SynthesizeTy,
        },
    };

    use super::LangCtx;
    use howlite_syntax::{
        ast::{BoxAstNode, LiteralChar, LiteralInteger, LiteralString},
        Span,
    };
    use howlite_typecheck::types::StorageClass;
    use preseli::IntegerSet;
    use proptest::prelude::*;
    use smol_str::ToSmolStr;
    use sunstone::ops::{SetOpIncludeExclude, SetOpIncludes};

    proptest! {
        #[test]
        fn synthesize_literal_string(s in any::<String>()) {
            let lang = LangCtx::<Span>::new();
            let program = BoxAstNode::new(Span::new(0,0), LiteralString { value: s.to_smolstr() });
            let ty = program.synthesize_ty(&lang);
            let arr = ty.as_array().expect("string didn't synthesize to array");
            assert_eq!(arr.length, s.len());
            let elem_ty = arr.element_ty.as_int().expect("element ty was not an int");
            assert_eq!(elem_ty.storage, StorageClass::unsigned(8));
            for byte in s.bytes() {
                assert!(elem_ty.values.includes(byte as i128), "string char type not include {:#02x}", byte);
            }
        }

        #[test]
        fn synthesize_literal_char(c in any::<char>()) {
            let lang = LangCtx::<Span>::new();
            let program = BoxAstNode::new(Span::new(0,0), LiteralChar { value: c });
            let ty = program.synthesize_ty(&lang);
            let int = ty.as_int().expect("char didn't synthesize to int");
            assert!(int.values.includes(c as i128));
            assert_eq!(int.storage, StorageClass::unsigned(32));
            assert_eq!({let mut empty = int.values.clone(); empty.exclude_mut(&(c as i128)) ; empty}, IntegerSet::empty());
        }


        #[test]
        fn synthesize_literal_struct(program in literal_struct(any_ident(), any_literal(), 0..24)) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert!(ty.as_struct().is_some(), "expected struct type, got: {:?}", ty);
        }

        #[test]
        fn synthesize_literal_array(program in literal_array(any_literal(), 0..10)) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert!(ty.as_array().is_some(), "expected array type, got: {:?}", ty);
        }

        #[test]
        fn synthesize_literal_array_large_int_table(program in literal_array(any::<LiteralInteger>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)), 512..1024)) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert!(ty.as_array().is_some(), "expected array type, got: {:?}", ty);
        }


        #[test]
        fn let_expr_simple(program in simple_scalar_let()) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert_lang_ok!(lang);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }
    }
}
