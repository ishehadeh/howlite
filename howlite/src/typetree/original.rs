use std::{rc::Rc, sync::Arc};

use crate::{
    langctx::{lexicalctx::LexicalContext, VarDef},
    symtab::Symbol,
    typetree::BinaryConstraintRelation,
};
use aries::{
    backtrack::{Backtrack, DecLvl},
    core::{
        state::{Cause, Domains, Term},
        Lit, VarRef,
    },
    model::{
        extensions::{AssignmentExt, Shaped},
        lang::{
            expr::{self, eq, geq, gt, leq, lt, neq, or, And, Or},
            IAtom, IVar,
        },
        symbols::SymbolTable,
        types::TypeHierarchy,
        Model,
    },
    reasoners::eq::ReifyEq,
    reif::{ReifExpr, Reifiable},
    solver::{
        search::{
            activity::{ActivityBrancher, BranchingParams, DefaultHeuristic},
            combinators::{AndThen, UntilFirstConflict, WithGeomRestart},
            conflicts::ConflictBasedBrancher,
            default_brancher,
            lexical::Lexical,
        },
        Solver,
    },
    utils::input::Sym,
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

pub fn determine_all_values<const N: usize, Lbl>(
    solver: &mut Solver<Lbl>,
    vars: [IVar; N],
) -> [IntegerSet; N]
where
    Lbl: std::fmt::Display + std::fmt::Debug + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
{
    let mut sets: [IntegerSet; N] = core::array::from_fn(|_| IntegerSet::empty());
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
        let mut constraints = ConstraintTree::new(&cond_ctx);
        let cond_term = constraints.get_constraint_term(self.condition);
        let true_ctx = ctx.new_with_scope().child(self.success);
        let false_ctx = ctx.new_with_scope().child(self.failure.unwrap());

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
            super::ConstraintTerm::UnaryConstraint { var, superset } => {
                let original_def = ctx.var_get(*var).unwrap();
                let original_val = original_def.assumed_ty.as_int().unwrap();

                let mut true_val = original_val.clone();
                true_val.values.intersect_mut(superset);

                let mut false_val = original_val.clone();
                false_val.values.set_subtract_mut(&true_val.values);
                false_ctx.var_def(
                    *var,
                    VarDef {
                        assumed_ty: original_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(false_val)),
                        is_mutable: original_def.is_mutable,
                    },
                );
                true_ctx.var_def(
                    *var,
                    VarDef {
                        assumed_ty: original_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(true_val)),
                        is_mutable: original_def.is_mutable,
                    },
                );
            }
            super::ConstraintTerm::BinaryConstraint {
                lhs,
                lhs_offset,
                relation,
                rhs,
            } => {
                let original_lhs_def = ctx.var_get(*lhs).unwrap();
                let original_lhs_val = original_lhs_def.assumed_ty.as_int().unwrap();
                let original_rhs_def = ctx.var_get(*rhs).unwrap();
                let original_rhs_val = original_rhs_def.assumed_ty.as_int().unwrap();

                let mut model = Model::new();

                let mut lbl_counter = 0;
                let lhs_solver_var =
                    set_to_expr(&mut model, "lhs", &original_lhs_val.values, || {
                        lbl_counter += 1;
                        format!("_{lbl_counter}").leak()
                    });

                let rhs_solver_var =
                    set_to_expr(&mut model, "rhs", &original_rhs_val.values, || {
                        lbl_counter += 1;
                        format!("_{lbl_counter}").leak()
                    });
                model.print_state();

                let p = model.new_bvar("a");

                match relation {
                    BinaryConstraintRelation::Lt => {
                        // model.enforce(lt(lhs_solver_var, rhs_solver_var), [p.true_lit()]);
                        // model.enforce(geq(lhs_solver_var, rhs_solver_var), [p.false_lit()]);
                        model.bind(lt(lhs_solver_var, rhs_solver_var), p.true_lit());
                    }
                    BinaryConstraintRelation::Eq => todo!(),
                    BinaryConstraintRelation::Gt => {
                        model.enforce(gt(lhs_solver_var, rhs_solver_var), [p.true_lit()]);
                    }
                    BinaryConstraintRelation::Ne => todo!(),
                }
                let original_model = model.clone();
                model.print_state();

                let mut solver = Solver::new(model);

                let true_clause = solver.reasoners.sat.add_clause([p.true_lit()]);
                let [lhs_true_values, rhs_true_values] = determine_all_values(
                    &mut solver,
                    [lhs_solver_var.into(), rhs_solver_var.into()],
                );
                solver.model.print_state();

                let mut solver = Solver::new(original_model);

                solver.reasoners.sat.add_clause([p.false_lit()]);
                let [lhs_false_values, rhs_false_values] = determine_all_values(
                    &mut solver,
                    [lhs_solver_var.into(), rhs_solver_var.into()],
                );

                let true_lhs_val = TyInt {
                    values: lhs_true_values,
                    storage: original_lhs_val.storage.clone(),
                };
                let true_rhs_val = TyInt {
                    values: rhs_true_values,
                    storage: original_rhs_val.storage.clone(),
                };

                let false_lhs_val = TyInt {
                    values: lhs_false_values,
                    storage: original_lhs_val.storage.clone(),
                };
                let false_rhs_val = TyInt {
                    values: rhs_false_values,
                    storage: original_rhs_val.storage.clone(),
                };
                debug!(?false_lhs_val);
                debug!(?false_rhs_val);
                debug!(?true_lhs_val);
                debug!(?true_rhs_val);

                false_ctx.var_def(
                    *lhs,
                    VarDef {
                        assumed_ty: original_lhs_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(false_lhs_val)),
                        is_mutable: original_lhs_def.is_mutable,
                    },
                );
                false_ctx.var_def(
                    *rhs,
                    VarDef {
                        assumed_ty: original_rhs_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(false_rhs_val)),
                        is_mutable: original_rhs_def.is_mutable,
                    },
                );
                true_ctx.var_def(
                    *lhs,
                    VarDef {
                        assumed_ty: original_lhs_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(true_lhs_val)),
                        is_mutable: original_lhs_def.is_mutable,
                    },
                );
                true_ctx.var_def(
                    *rhs,
                    VarDef {
                        assumed_ty: original_rhs_def.assumed_ty.clone(),
                        last_assignment: Rc::new(Ty::Int(true_rhs_val)),
                        is_mutable: original_rhs_def.is_mutable,
                    },
                );
            }
            super::ConstraintTerm::UnaryOperation { .. } => todo!(),
            super::ConstraintTerm::BinaryOperation { .. } => todo!(),
        };
        let t_type = true_ctx.synthesize_ty();
        let f_type = false_ctx.synthesize_ty();
        dbg!(&t_type, &f_type);
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
