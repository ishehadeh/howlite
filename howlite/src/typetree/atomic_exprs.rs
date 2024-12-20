use std::rc::Rc;

use howlite_syntax::ast::{
    ExprCall, Ident, LiteralArray, LiteralChar, LiteralInteger, LiteralString, LiteralStruct,
};
use howlite_typecheck::{
    types::{self, StorageClass, TyInt},
    Ty, TyArray,
};
use preseli::IntegerSet;

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol};

use super::{SynthesizeTy, SynthesizeTyPure};

impl SynthesizeTy for ExprCall {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let ty_params: Vec<_> = self
            .ty_params
            .iter()
            .map(|t| ctx.child(*t).synthesize_ty())
            .collect();
        let params: Vec<_> = self
            .params
            .iter()
            .map(|t| ctx.child(*t).synthesize_ty())
            .collect();
        let func = ctx.func_get_or_err(ctx.sym_intern(&self.callee));
        func.check_params(ctx, &ty_params, &params)
    }
}

impl SynthesizeTy for Ident {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        ctx.get_current_var_ty_or_err(ctx.sym_intern(&self.symbol))
            
    }
}

impl SynthesizeTyPure for LiteralInteger {
    fn synthesize_ty_pure(&self) -> Rc<Ty<Symbol>> {
        Rc::new(Ty::Int(TyInt::single(self.value)))
    }
}

impl SynthesizeTyPure for LiteralString {
    fn synthesize_ty_pure(&self) -> Rc<Ty<Symbol>> {
        let bytes = self.value.as_bytes();
        let values = IntegerSet::new_from_individual_generic(bytes);
        let element_ty = Rc::new(Ty::Int(TyInt {
            values,
            storage: StorageClass::unsigned(8),
        }));

        Rc::new(Ty::Array(TyArray {
            length: self.value.len(),
            element_ty,
        }))
    }
}

impl SynthesizeTyPure for LiteralChar {
    fn synthesize_ty_pure(&self) -> Rc<Ty<Symbol>> {
        let c = self.value;
        let values = IntegerSet::new_from_individual(&[c as i128]);
        Rc::new(Ty::Int(TyInt {
            values,
            storage: StorageClass::unsigned(32),
        }))
    }
}

impl SynthesizeTy for LiteralArray {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        let tys: Vec<_> = self
            .values
            .iter()
            .map(|&child| ctx.child(child).synthesize_ty())
            .collect();
        let element_ty = Ty::union(&tys);

        Rc::new(Ty::Array(TyArray {
            length: self.values.len(),
            element_ty,
        }))
    }
}

impl SynthesizeTy for LiteralStruct {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        let ty = types::TyStruct {
            fields: self
                .members
                .iter()
                .map(|child| types::StructField {
                    name: ctx.sym_intern(child.data.field.as_str()),
                    ty: ctx.child(child.data.value).synthesize_ty(),
                })
                .collect(),
        };

        Rc::new(Ty::Struct(ty))
    }
}

#[cfg(test)]
mod test {
    use crate::{
        get_node_type,
        langctx::LangCtx,
        typetree::test_helpers::{
            any_ident, any_literal, literal_array, literal_struct, must_parse,
        },
    };

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
            let ty = get_node_type!(boxed LiteralString { value: s.to_smolstr() });

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
            let ty = get_node_type!(boxed LiteralChar { value: c });
            let int = ty.as_int().expect("char didn't synthesize to int");
            assert!(int.values.includes(c as i128));
            assert_eq!(int.storage, StorageClass::unsigned(32));
            assert_eq!({let mut empty = int.values.clone(); empty.exclude_mut(&(c as i128)) ; empty}, IntegerSet::empty());
        }


        #[test]
        fn synthesize_literal_struct(program in literal_struct(any_ident(), any_literal(), 0..24)) {
            let ty = get_node_type!(program);
            assert!(ty.as_struct().is_some(), "expected struct type, got: {:?}", ty);
        }

        #[test]
        fn synthesize_literal_array(program in literal_array(any_literal(), 0..10)) {
            let ty = get_node_type!(program);
            assert!(ty.as_array().is_some(), "expected array type, got: {:?}", ty);
        }

        // falls over on this test, now that we use actual int unions, not just an array of int sets
        // #[test]
        // fn synthesize_literal_array_large_int_table(program in literal_array(any::<LiteralInteger>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)), 512..1024)) {
        //     let ty = get_node_type!(program);
        //     assert!(ty.as_array().is_some(), "expected array type, got: {:?}", ty);
        // }

        #[test]
        fn synthesize_literal_array_medium_int_table(program in literal_array(any::<LiteralInteger>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)), 20..30)) {
            let ty = get_node_type!(program);
            assert!(ty.as_array().is_some(), "expected array type, got: {:?}", ty);
        }
    }

    #[test]
    fn test_call_simple() {
        let (block_node_id, ast) = must_parse(
            r#"
        func id(t: 0..0xffffffff): 0..0xffffffff { t }
        
        func main(): unit {
            let a: 0..0xffffffff = id(1);
        }
        "#,
        );
        let ctx = LangCtx::new(&ast);
        let _ = ctx
            .make_lexical_context(ctx.root_scope_id, block_node_id)
            .synthesize_ty();
        for err in ctx.errors.iter() {
            println!("Error: {:?}", err.error());
        }
        assert!(ctx.errors.is_empty());
    }
}
