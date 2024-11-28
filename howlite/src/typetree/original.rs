use std::rc::Rc;

use crate::{
    langctx::{lexicalctx::LexicalContext, VarDef},
    symtab::Symbol,
    typetree::{BinaryConstraintRelation, ModelVarRef},
};
use aries::{
    backtrack::Backtrack,
    core::Lit,
    model::{
        extensions::Shaped,
        lang::{
            expr::{self, eq, gt, lt, or},
            IVar,
        },
        Model,
    },
    solver::Solver,
};
use howlite_syntax::ast::{ExprIf, ExprLet};
use howlite_typecheck::{types::TyInt, Ty};
use preseli::{constraints::OffsetLtConstraint, IntegerSet};
use sunstone::{
    multi::DynSet,
    ops::{Bounded, IntersectMut, PartialBounded, SetOpIncludeExclude, SetSubtract, Union},
    step_range::StepRange,
};
use tracing::debug;

fn constrain_via_stripes<Lbl>(
    model: &mut Model<Lbl>,
    var: IVar,
    stripes: impl Iterator<Item = StepRange<i128>>,
    mut lbl_gen: impl FnMut() -> Lbl,
) where
    Lbl: std::fmt::Display + std::fmt::Debug + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
{
    let mut options = Vec::new();

    for step_range in stripes {
        if step_range.is_size_one() {
            options.push(model.reify(eq(var, *step_range.lo())));
        } else if *step_range.step() == 1i128 {
            options.push(model.reify(expr::and([
                var.geq(step_range.lo().clone()),
                var.leq(step_range.hi().clone()),
            ])))
        } else {
            let step_var = model.new_ivar(0, step_range.size(), lbl_gen());
            options.push(model.reify(eq(
                var,
                (step_var * *step_range.step()).var() + *step_range.lo(),
            )))
        }
    }

    model.enforce(or(options), []);
}

pub fn set_to_expr<Lbl>(
    model: &mut Model<Lbl>,
    name: Lbl,
    set: &IntegerSet,
    lbl_gen: impl FnMut() -> Lbl,
) -> IVar
where
    Lbl: std::fmt::Display + std::fmt::Debug + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
{
    let var = model.new_ivar(*set.get_range().lo(), *set.get_range().hi(), name);
    match set.inner() {
        sunstone::multi::DynSetData::Empty => todo!(),
        sunstone::multi::DynSetData::Small(s) => {
            constrain_via_stripes(model, var, s.stripes(), lbl_gen);
        }
        sunstone::multi::DynSetData::Contiguous => (),
        sunstone::multi::DynSetData::Stripe(stripe_set) => {
            constrain_via_stripes(model, var, stripe_set.stripes().cloned(), lbl_gen);
        }
    }

    var
}

pub fn determine_all_values<Lbl>(solver: &mut Solver<Lbl>, vars: &[IVar]) -> Vec<IntegerSet>
where
    Lbl: std::fmt::Display + std::fmt::Debug + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
{
    let mut sets: Vec<IntegerSet> = Vec::with_capacity(vars.len());
    sets.resize_with(vars.len(), || IntegerSet::empty());
    while let Some(sol) = solver.solve().unwrap() {
        let mut clause = Vec::with_capacity(vars.len() * 2);

        for (var, set) in vars.iter().zip(&mut sets) {
            let (lower, upper) = sol.bounds((*var).into());

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

use super::{constraint_tree::ConstraintTree, SynthesizeTy};

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
        let mut constraints = ConstraintTree::new(cond_ctx);
        let cond_term = constraints.get_constraint_term(self.condition);
        let true_ctx = ctx.new_with_scope().child(self.success);
        let false_ctx = ctx.new_with_scope().child(self.failure.unwrap());

        let cond_lit = constraints.model_builder.reify_term(cond_term.unwrap());
        let true_model = constraints.model_builder.model.clone();
        let false_model = constraints.model_builder.model;

        let modified_vars_model: Vec<IVar> = constraints
            .modified_vars
            .iter()
            .map(|s| true_model.get_int_var(&ModelVarRef::HltVar(*s)).unwrap())
            .collect();

        let mut true_solver = Solver::new(true_model);
        let mut false_solver = Solver::new(false_model);
        true_solver.reasoners.sat.add_clause([cond_lit]);
        false_solver.reasoners.sat.add_clause([cond_lit.not()]);

        for (solver, ctx) in [
            (&mut true_solver, &true_ctx),
            (&mut false_solver, &false_ctx),
        ] {
            let new_values = determine_all_values(solver, &modified_vars_model);
            for (i, true_var_value) in new_values.into_iter().enumerate() {
                let name = constraints.modified_vars[i];
                let original_def = ctx.var_get_or_err(name);
                let original_int_def = original_def.last_assignment.as_int().unwrap();
                ctx.var_def(
                    name,
                    VarDef {
                        assumed_ty: original_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(TyInt {
                            values: true_var_value,
                            storage: original_int_def.storage.clone(),
                        })),
                        is_mutable: original_def.is_mutable,
                    },
                );
            }
        }

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
    use tracing_test::traced_test;

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
            t_int!(5..10)
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
}
