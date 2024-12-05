use std::rc::Rc;

use crate::{
    langctx::{lexicalctx::LexicalContext, VarDef},
    symtab::Symbol,
    typetree::ModelVarRef, CompilationErrorKind,
};
use aries::{
    backtrack::Backtrack,
    core::Lit,
    model::{extensions::Shaped, lang::IVar},
    solver::Solver,
};
use howlite_syntax::ast::{ExprIf, ExprLet, ExprReturn, ExprWhile};
use howlite_typecheck::{types::TyInt, Ty};
use preseli::IntegerSet;
use sunstone::{multi::DynSet, ops::Union};
use tracing::{debug, instrument};

#[instrument(skip(solver))]
pub fn determine_all_values<Lbl>(solver: &mut Solver<Lbl>, vars: &[IVar]) -> Vec<IntegerSet>
where
    Lbl: std::fmt::Display + std::fmt::Debug + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
{
    let mut sets: Vec<IntegerSet> = Vec::with_capacity(vars.len());
    sets.resize_with(vars.len(), IntegerSet::empty);
    while solver.propagate_and_backtrack_to_consistent() {
        let mut clause = Vec::with_capacity(vars.len() * 2);

        for (var, set) in vars.iter().zip(&mut sets) {
            let (lower, upper) = solver.model.state.bounds((*var).into());
            debug!(?var, ?lower, ?upper);

            set.union(DynSet::new_from_range(lower, upper));

            clause.push(Lit::lt(*var, lower));
            clause.push(Lit::gt(*var, upper));
        }
        if let Some(dl) = solver.backtrack_level_for_clause(&clause) {
            solver.restore(dl);
            solver.reasoners.sat.add_clause(clause);
            // solver.decide(Lit::gt(*var, upper));
        } else {
            break;
        }
    }

    sets
}

fn narrow_cond(
    ctx: &LexicalContext,
    assumed_ty: bool,
    true_ctx: LexicalContext,
    false_ctx: Option<LexicalContext>,
) {
    let mut constraints = ConstraintTree::new(ctx.clone(), assumed_ty);
    let cond_term = constraints.get_constraint_term(ctx.node());

    debug!(vars = ?constraints.modified_vars, "narrowing based on condition");
    
    if let Some(cond_term) = cond_term {
        let cond_lit = constraints.model_builder.reify_term(cond_term);
        let ctxs = std::iter::once((cond_lit, true_ctx))
            .chain(false_ctx.into_iter().map(|ctx| (cond_lit.not(), ctx)));
        for (clause, child_ctx) in ctxs {
            let model = constraints.model_builder.model.clone();

            let modified_vars_model: Vec<IVar> = constraints
                .modified_vars
                .iter()
                .map(|(mv, _, _)| model.get_int_var(&mv).unwrap())
                .collect();

            let mut solver = Solver::new(model);
            solver.reasoners.sat.add_clause([clause]);

            let new_values = determine_all_values(&mut solver, &modified_vars_model);
            for (i, true_var_value) in new_values.into_iter().enumerate() {
                let (_, name, path) = &constraints.modified_vars[i];
                let original_def = match child_ctx.get_current_var_ty(*name) {
                    Some(v) => v,
                    None => {
                        ctx.error(CompilationErrorKind::UnknownVariable { name: ctx.sym_stringify(*name) });
                        Rc::new(Ty::Hole)
                    }
                };
            
                let original_ty = original_def
                    .access_path(path.as_slice())
                    .unwrap();
                let value = Rc::new(Ty::Int(TyInt {
                    values: true_var_value,
                    storage: original_ty.as_int().unwrap().storage.clone(),
                }));

                child_ctx.narrow(
                    *name,
                    original_def
                            .assign_path(path.as_slice(), value)
                            .unwrap(),
                );
            }
        }
    }
}

use super::{constraint_tree::ConstraintTree, SynthesizeTy};

impl SynthesizeTy for ExprLet {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        let var_symbol = ctx.sym_intern(self.name.as_str());
        let var_ty = ctx.child(self.ty).synthesize_ty();
        let var_value_ty = ctx.child(self.value).synthesize_ty();
        if let Err(e) = var_value_ty.is_assignable_to(&var_ty) {
            ctx.error(CompilationErrorKind::InvalidAssignment(e));
        }

        let assign_ty = var_ty.assign_path(&[], var_value_ty.clone()).unwrap();
        ctx.var_def(
            var_symbol,
            VarDef {
                assumed_ty: var_ty.clone(),
                last_assignment: assign_ty.clone(),
                is_mutable: self.mutable,
            },
        );

        assign_ty
    }
}

impl SynthesizeTy for ExprWhile {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let cond_ctx = ctx.child(self.condition);
        let body_ctx = ctx.new_with_scope().child(self.body);

        narrow_cond(&cond_ctx, true, body_ctx.clone(), None);
        body_ctx.clear_assigned_tys();
        let _ = body_ctx.synthesize_ty();

        Ty::unit().into()
    }
}

impl SynthesizeTy for ExprIf {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let cond_ctx = ctx.child(self.condition);
        let true_ctx = ctx.new_with_scope().child(self.success);
        let false_ctx = self.failure.map(|else_| ctx.new_with_scope().child(else_));
        narrow_cond(&cond_ctx, false, true_ctx.clone(), false_ctx.clone());

        let t_type = true_ctx.synthesize_ty();
        let f_type = false_ctx
            .map(|f| f.synthesize_ty())
            .unwrap_or(Rc::new(Ty::unit()));

        Ty::union(&[t_type, f_type])
    }
}

impl SynthesizeTy for ExprReturn {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let value_ty = ctx.child(self.value).synthesize_ty();
        ctx.add_return_ty(value_ty);
        Rc::new(Ty::unit())
    }
}

#[cfg(test)]
mod test {
    use crate::{
        get_node_type,
        langctx::{LangCtx, VarDef},
        typetree::test_helpers::{must_parse_expr, simple_scalar_let},
    };

    use howlite_typecheck::{shape::TypeShape, t_int, types::TyInt, Ty};
    use proptest::prelude::*;
    use tracing::instrument;
    use tracing_test::traced_test;

    proptest! {

        #[test]
        fn let_expr_simple(program in simple_scalar_let()) {
            let ty = get_node_type!(program);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }
    }

    #[instrument]
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
                assumed_ty: Ty::Int(TyInt::u32()).into(),
                last_assignment: Ty::Int(TyInt::u32()).into(),
                is_mutable: true,
            },
        );
        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty(),
            t_int!(5i128..0xffffffff)
        )
    }

    #[traced_test]
    #[test]
    fn narrowing_eq_binary() {
        let (block_node_id, ast) = must_parse_expr(
            r#"
            if a < b {
                // => a = 3
                a + b
            } else {
                a - b
            }
            "#,
        );
        let ctx = LangCtx::new(&ast);
        ctx.var_def(
            ctx.root_scope_id,
            ctx.symbols.intern("a"),
            VarDef {
                assumed_ty: t_int!(3, 11),
                last_assignment: t_int!(3, 11),
                is_mutable: true,
            },
        );

        ctx.var_def(
            ctx.root_scope_id,
            ctx.symbols.intern("b"),
            VarDef {
                assumed_ty: t_int!(5, 7),
                last_assignment: t_int!(5, 7),
                is_mutable: true,
            },
        );
        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty(),
            t_int!(6, 4, 8, 10)
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

    #[test]
    fn while_incrementing() {
        let (block_node_id, ast) = must_parse_expr(
            r#"{
                let mut i : 0..10 = 0;
                while i < 10 {
                    // assignment not implemented...
                    let j: 0..10 = i + 1;
                }
            }
            "#,
        );
        let ctx = LangCtx::new(&ast);

        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty()
                .shape(),
            TypeShape::UNIT
        )
    }
}
