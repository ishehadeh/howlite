use std::rc::Rc;

use crate::{
    langctx::{lexicalctx::LexicalContext, VarDef},
    symtab::Symbol,
};
use howlite_syntax::ast::{ExprIf, ExprLet};
use howlite_typecheck::{
    types::{TyInt, TyUnion},
    Ty,
};
use preseli::IntegerSet;
use sunstone::{
    multi::DynSet,
    ops::{SetOpIncludeExclude, SetSubtract},
};

use super::{constraint_term, constraint_tree::ConstraintTree, SynthesizeTy};

impl SynthesizeTy for ExprLet {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        let var_symbol = ctx.sym_intern(self.name.as_str());
        let var_ty = ctx.child(self.ty).synthesize_ty();
        let var_value_ty = ctx.child(self.value).synthesize_ty();
        ctx.var_def(
            var_symbol,
            VarDef {
                assumed_ty: var_ty,
                last_assignment: var_value_ty.clone(),
                is_mutable: self.mutable,
            },
        );

        var_value_ty
    }
}

impl SynthesizeTy for ExprIf {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let cond_ctx = ctx.child(self.condition);
        let mut constraints = ConstraintTree::new(&cond_ctx);
        let cond_term = constraints.get_constraint_term(self.condition);
        let mut true_ctx = ctx.child(self.success);
        let mut false_ctx = ctx.child(self.failure.unwrap());

        match cond_term {
            super::ConstraintTerm::NotApplicable => (),
            super::ConstraintTerm::Literal(_) => (),
            &super::ConstraintTerm::Var(var) => {
                let original_def = ctx.var_get(var).unwrap();
                let original_val = original_def.assumed_ty.as_int().unwrap();
                false_ctx.var_def(
                    var,
                    VarDef {
                        assumed_ty: original_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(TyInt {
                            values: IntegerSet::new_from_range(0, 0),
                            storage: original_val.storage.clone(),
                        })),
                        is_mutable: original_def.is_mutable,
                    },
                );
                let mut non_zero = original_val.clone();
                non_zero.values.exclude_mut(&0);
                true_ctx.var_def(
                    var,
                    VarDef {
                        assumed_ty: original_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(non_zero)),
                        is_mutable: original_def.is_mutable,
                    },
                );
            }
            super::ConstraintTerm::UnaryConstraint { var, superset } => todo!(),
            super::ConstraintTerm::BinaryConstraint {
                lhs,
                lhs_offset,
                relation,
                rhs,
            } => todo!(),
            super::ConstraintTerm::UnaryOperation { var, op, value } => todo!(),
            super::ConstraintTerm::BinaryOperation {
                lhs,
                lhs_offset,
                op,
                rhs,
            } => todo!(),
        };
        let t_type = true_ctx.synthesize_ty();
        let f_type = false_ctx.synthesize_ty();
        Ty::union(&[t_type, f_type])
    }
}

#[cfg(test)]
mod test {
    use crate::{
        get_node_type,
        langctx::{LangCtx, VarDef},
        typetree::test_helpers::{must_parse_expr, simple_scalar_let},
    };

    use howlite_typecheck::t_int;
    use proptest::prelude::*;

    proptest! {

        #[test]
        fn let_expr_simple(program in simple_scalar_let()) {
            let ty = get_node_type!(program);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }
    }

    #[test]
    fn narrowing_eq_unary() {
        let (block_node_id, ast) = must_parse_expr(
            r#"
            if a < 5 {
                    a + 5
                } else {
                    a
                }
            "#,
        );
        let ctx = LangCtx::new(&ast);
        ctx.var_def(
            ctx.root_scope_id,
            ctx.symbols.intern("a"),
            VarDef {
                assumed_ty: t_int!(0..10),
                last_assignment: t_int!(0..10),
                is_mutable: true,
            },
        );
        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty(),
            t_int!(0..5)
        )
    }

    #[test]
    fn narrowing_eq_var_only() {
        let (block_node_id, ast) = must_parse_expr(
            r#"
            if a {
                a + 1
            } else {
                15
            }
            "#,
        );
        let ctx = LangCtx::new(&ast);
        ctx.var_def(
            ctx.root_scope_id,
            ctx.symbols.intern("a"),
            VarDef {
                assumed_ty: t_int!(0..10),
                last_assignment: t_int!(0..10),
                is_mutable: true,
            },
        );
        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty(),
            t_int!(2..11, 15)
        )
    }
}
