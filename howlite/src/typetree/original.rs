use std::rc::Rc;

use howlite_syntax::{
    ast::{
        BoxAstNode, ExprInfix, ExprLet, HigherOrderNode, InfixOp, LiteralArray, LiteralChar,
        LiteralInteger, LiteralString, LiteralStruct,
    },
    AstNode, AstNodeData, Span,
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
    CompilationError,
};

use super::{SynthesizeTy, SynthesizeTyPure};

impl SynthesizeTy<Span> for BoxAstNode {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let AstNode { data, span } = self.into_inner();
        match data {
            AstNodeData::LiteralInteger(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralString(n) => AstNode::new_narrow(span, &n).synthesize_ty(ctx),
            AstNodeData::LiteralChar(n) => AstNode::new_narrow(span, &n).synthesize_ty(ctx),
            AstNodeData::ExprInfix(n) => {
                AstNode::new_narrow(span, &n.map(|c| c.synthesize_ty(ctx))).synthesize_ty(ctx)
            }
            AstNodeData::LiteralStruct(n) => AstNode::new_narrow(span, n).synthesize_ty(ctx),
            AstNodeData::LiteralArray(n) => {
                AstNode::new_narrow(span, n.map(|c| c.synthesize_ty(ctx))).synthesize_ty(ctx)
            }
            AstNodeData::ExprLet(n) => {
                AstNode::new_narrow(span, n.map(|c| c.synthesize_ty(ctx))).synthesize_ty(ctx)
            }

            AstNodeData::TyNumberRange(n) => {
                AstNode::new_narrow(span, n.map(|c| c.synthesize_ty(ctx))).synthesize_ty(ctx)
            }
            // AstNodeData::Block(n) => n.synthesize_ty(ctx),
            t => todo!("ty not implemented for test checker: {:?}", t),
        }
    }
}

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

impl SynthesizeTy<Span> for AstNode<LiteralStruct<BoxAstNode>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let ty = types::TyStruct {
            fields: self
                .data
                .members
                .into_iter()
                .map(|child| {
                    if let AstNodeData::LiteralStructMember(m) = child.into_inner().data {
                        (m.field, m.value.synthesize_ty(ctx))
                    } else {
                        panic!("child was not a struct member, this should be unreachable!")
                    }
                })
                .map(|(field, value_ty)| types::StructField {
                    name: ctx.symbols.intern(field.as_str()),
                    ty: value_ty.clone(),
                })
                .collect(),
        };

        Rc::new(Ty::Struct(ty))
    }
}

impl SynthesizeTy<Span> for AstNode<&ExprInfix<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        match self.data.op {
            InfixOp::Add => match self.data.lhs.arith_add(&self.data.rhs) {
                Ok(v) => Rc::new(v),
                Err(e) => {
                    ctx.error(CompilationError {
                        location: self.span,
                        kind: e.into(),
                    });
                    Rc::new(Ty::Hole)
                }
            },
            _ => todo!(),
        }
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
        typetree::{test_helpers::make_ty_number_range, SynthesizeTy},
    };

    use super::LangCtx;
    use howlite_syntax::{
        ast::{
            BoxAstNode, ExprLet, LiteralArray, LiteralChar, LiteralInteger, LiteralString,
            LiteralStruct, LiteralStructMember,
        },
        Span,
    };
    use howlite_typecheck::types::StorageClass;
    use preseli::IntegerSet;
    use prop::{sample::SizeRange, string::StringParam};
    use proptest::prelude::*;
    use smol_str::{SmolStr, ToSmolStr};
    use sunstone::ops::{SetOpIncludeExclude, SetOpIncludes};

    /// Any literal that cannot contain arbirary data
    fn any_atomic_literal() -> impl Strategy<Value = BoxAstNode> {
        prop_oneof![
            any::<LiteralChar>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)),
            any::<LiteralString>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)),
            any::<LiteralInteger>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v))
        ]
    }

    fn literal_struct_member<K, V>(k: K, v: V) -> impl Strategy<Value = BoxAstNode>
    where
        K: Strategy<Value = String>,
        V: Strategy<Value = BoxAstNode>,
    {
        (k, v).prop_map(|(field, value)| {
            BoxAstNode::new(
                Span::new(0, 0),
                LiteralStructMember {
                    field: field.into(),
                    value,
                },
            )
        })
    }

    fn literal_struct<K, V, S>(k: K, v: V, length: S) -> impl Strategy<Value = BoxAstNode>
    where
        K: Strategy<Value = String>,
        V: Strategy<Value = BoxAstNode>,
        S: Into<SizeRange>,
    {
        proptest::collection::vec(literal_struct_member(k, v), length).prop_map(|members| {
            BoxAstNode::new(
                Span::new(0, 0),
                LiteralStruct {
                    members: members.into_iter().collect(),
                },
            )
        })
    }

    fn literal_array<V, S>(v: V, length: S) -> impl Strategy<Value = BoxAstNode>
    where
        V: Strategy<Value = BoxAstNode>,
        S: Into<SizeRange>,
    {
        proptest::collection::vec(v, length).prop_map(|members| {
            BoxAstNode::new(
                Span::new(0, 0),
                LiteralArray {
                    values: members.into_iter().collect(),
                },
            )
        })
    }

    fn any_literal() -> impl Strategy<Value = BoxAstNode> {
        any_atomic_literal().prop_recursive(4, 32, 12, |inner| {
            prop_oneof![
                literal_struct(any_ident(), inner.clone(), 0..12),
                literal_array(inner, 0..12),
            ]
        })
    }

    fn make_expr_let(name: impl Into<SmolStr>, ty: BoxAstNode, value: BoxAstNode) -> BoxAstNode {
        BoxAstNode::new(
            Span::new(0, 0),
            ExprLet {
                name: name.into(),
                ty,
                mutable: true,
                value,
            },
        )
    }

    fn simple_scalar_let() -> impl Strategy<Value = BoxAstNode> {
        (0..u64::MAX as i128, 0..u64::MAX as i128)
            .prop_flat_map(|(a, b)| (Just(make_ty_number_range(a, b)), a.min(b)..b.max(a)))
            .prop_map(|(ty, value)| {
                make_expr_let(
                    "_a",
                    ty,
                    BoxAstNode::new(Span::new(0, 0), LiteralInteger { value }),
                )
            })
    }

    fn any_ident() -> impl Strategy<Value = String> {
        any_with::<String>(StringParam::from("[_a-zA-Z][_a-zA-Z0-9]*"))
    }

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
