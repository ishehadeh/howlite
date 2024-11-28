use std::rc::Rc;

use howlite_syntax::{ast, tree::DefaultLinearTreeId, AstNodeData};
use howlite_typecheck::{types::TyLateBound, Ty, TyBinder};

use crate::{
    langctx::{lexicalctx::LexicalContext, FuncDef, TyDef, VarDef},
    symtab::Symbol,
    CompilationErrorKind,
};

use super::SynthesizeTy;

/// Read a list of type parameters and define them as late bound types,
/// Then return a list of (Symbol, Ty) tuples that will be accepted by TyDef/FuncDef for type params.
fn eval_ty_params(
    ty_params: &[DefaultLinearTreeId],
    param_ctx: &LexicalContext<'_, '_>,
    param_ty_def_ctx: &LexicalContext<'_, '_>,
    instantiated_ctx: Option<&LexicalContext<'_, '_>>,
) -> Vec<(Symbol, Rc<Ty<Symbol>>)> {
    let mut params = Vec::with_capacity(ty_params.len());
    for param in ty_params {
        let param_node = param_ctx.get_node(*param);
        match &param_node.data {
            AstNodeData::TyParam(p) => {
                let param_name_sym = param_ctx.sym_intern(&p.name);
                let param_super_ty = param_ctx.child(p.super_ty).synthesize_ty();

                let late_bound_param_ty = Rc::new(Ty::LateBound( TyLateBound {
                    name: param_name_sym,
                    ty: param_super_ty.clone(),
                }));
                // TODO: include default type param defs?
                param_ty_def_ctx.ty_def(TyDef {
                    name: param_name_sym,
                    ty: late_bound_param_ty.clone(),
                    params: vec![]
                });
                if let Some(ictx) = instantiated_ctx {
                    ictx.ty_def(TyDef {
                        name: param_name_sym,
                        ty: param_super_ty.clone(),
                        params: vec![]
                    });
                }
                params.push((param_name_sym, param_super_ty))
            },
            _ => panic!("found non-TyParam AST ref in ty param list, this should be impossible. (assuming the parser is correct (which if your seeing this message it clearly isn't))")
        }
    }

    params
}

impl SynthesizeTy for ast::DefType {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let name_sym = ctx.sym_intern(&self.name);

        // create a new scope now, as we eval the params we'll add them as LateBound
        let body_ctx = ctx.child(self.ty).new_with_scope();
        let params = eval_ty_params(&self.ty_params, ctx, &body_ctx, None);

        let ty = body_ctx.synthesize_ty();
        ctx.ty_def(TyDef {
            name: name_sym,
            params,
            ty,
        });
        Ty::unit().into()
    }
}

impl SynthesizeTy for ast::DefFunc {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let name_sym = ctx.sym_intern(&self.name);

        // create a new scope now, as we eval the type params we'll add them as LateBound
        // this scope will be used to eval parameters, but it will not be used for the body
        // instead we'll just use the super type (since we don't actually specialize the impl, this is safe)
        let param_ctx = ctx.new_with_scope();

        // scope for the body, includes params and type params (but their super type)
        let body_ctx = ctx.child(self.body).new_with_scope();

        let ty_params = eval_ty_params(&self.ty_params, ctx, &param_ctx, Some(&body_ctx));

        let mut params = Vec::with_capacity(self.params.len());
        for &param in &self.params {
            let param_node = param_ctx.get_node(param);
            let param = match &param_node.data {
                AstNodeData::DefParam(p) => p,
                _ => {
                    panic!("found non-DefParam AST ref in param list, this should be impossible.")
                }
            };

            let name = ctx.sym_intern(&param.name);
            let param_ty = param_ctx.child(param.ty).synthesize_ty();
            let mut ty_binder = TyBinder::new(&ty_params);
            let body_t = ty_binder.bind(param_ty.clone());
            assert!(
                ty_binder.errors().is_empty(),
                "TyBinder errors while instantiating defaults = {:?}",
                ty_binder.errors()
            );
            params.push(param_ty.clone());
            body_ctx.var_def(
                name,
                VarDef {
                    assumed_ty: body_t.clone(),
                    last_assignment: body_t,
                    is_mutable: param.mutable,
                },
            );
        }

        let return_ty = param_ctx.child(self.return_ty).synthesize_ty();
        let mut ty_binder = TyBinder::new(&ty_params);
        let instantiated_return_ty = ty_binder.bind(return_ty.clone());
        assert!(
            ty_binder.errors().is_empty(),
            "TyBinder errors while instantiating defaults = {:?}",
            ty_binder.errors()
        );
        let body_ty = body_ctx.synthesize_ty();

        if let Err(e) = body_ty.is_assignable_to(&instantiated_return_ty) {
            ctx.error(CompilationErrorKind::BadReturn {
                return_ty: return_ty.clone(),
                name: self.name.clone(),
                source: e,
            });
        }
        ctx.func_def(FuncDef {
            name: name_sym,
            params,
            ty_params,
            returns: return_ty,
        });
        Ty::unit().into()
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::{langctx::LangCtx, typetree::test_helpers::must_parse};
    use howlite_typecheck::{
        t_int, t_struct,
        types::{StructField, TyInt, TyLateBound, TyStruct},
        Ty,
    };
    use preseli::IntegerSet;
    use smallvec::smallvec;
    use tracing_test::traced_test;

    #[test]
    pub fn def_type_simple() {
        let (block_node_id, ast) = must_parse(
            r#"type MyU32 = 0..0xFFFFFFFF;
            "#,
        );
        let ctx = LangCtx::new(&ast);
        let _ = ctx
            .make_lexical_context(ctx.root_scope_id, block_node_id)
            .synthesize_ty();

        let ty = ctx
            .ty_get(ctx.root_scope_id, ctx.symbols.intern("MyU32"))
            .expect("could not find type in root scope");
        assert_eq!(ty.params, vec![]);
        assert_eq!(ty.ty, Ty::Int(TyInt::u32()).into());
    }

    #[test]
    #[traced_test]
    pub fn def_type_with_params_simple() {
        let (block_node_id, ast) = must_parse(
            r#"type Pair32[T: 0..0xFFFFFFFF, U: 0..0xFFFFFFFF] = { a: T, b: U };
            "#,
        );

        let ctx = LangCtx::new(&ast);
        let root_lexical_ctx = ctx.make_lexical_context(ctx.root_scope_id, block_node_id);
        let _ = root_lexical_ctx.synthesize_ty();
        dbg!(&ctx);

        let sym_pair32 = ctx.symbols.intern("Pair32");
        let sym_t = ctx.symbols.intern("T");
        let sym_u = ctx.symbols.intern("U");
        let sym_a = ctx.symbols.intern("a");
        let sym_b = ctx.symbols.intern("b");
        let ty_u32: Rc<_> = Ty::Int(TyInt::u32()).into();

        let ty = ctx
            .ty_get(ctx.root_scope_id, sym_pair32)
            .expect("could not find type in root scope");
        assert_eq!(
            ty.params,
            vec![(sym_t, ty_u32.clone()), (sym_u, ty_u32.clone())]
        );
        assert_eq!(
            ty.ty,
            Ty::Struct(TyStruct {
                fields: smallvec![
                    StructField {
                        name: sym_a,
                        ty: Ty::LateBound(TyLateBound {
                            name: sym_t,
                            ty: ty_u32.clone()
                        })
                        .into()
                    },
                    StructField {
                        name: sym_b,
                        ty: Ty::LateBound(TyLateBound {
                            name: sym_u,
                            ty: ty_u32.clone()
                        })
                        .into()
                    }
                ]
            })
            .into()
        );
        assert_eq!(
            ty.instantiate(&root_lexical_ctx, &[t_int!(1), t_int!(2)]),
            t_struct! {
                sym_a => t_int!(1),
                sym_b => t_int!(2),
            }
        )
    }

    #[test]
    pub fn def_func_simple() {
        let (block_node_id, ast) = must_parse(r#"func id(t: 0..0xffffffff): 0..0xffffffff { t }"#);
        let ctx = LangCtx::new(&ast);
        let _ = ctx
            .make_lexical_context(ctx.root_scope_id, block_node_id)
            .synthesize_ty();

        let f = ctx
            .func_get(ctx.root_scope_id, ctx.symbols.intern("id"))
            .expect("could not find type in root scope");
        assert_eq!(f.params, vec![Ty::Int(TyInt::u32()).into()]);
        assert_eq!(f.returns, Ty::Int(TyInt::u32()).into());
    }

    #[test]
    pub fn def_func_generic() {
        let (block_node_id, ast) = must_parse(
            r#"
            func not[T: 0 | 1](a: T): T { 
                let b: T = a;
                !b
            }
            "#,
        );
        let ctx = LangCtx::new(&ast);
        let root_ctx = ctx.make_lexical_context(ctx.root_scope_id, block_node_id);
        let _ = root_ctx.synthesize_ty();

        let sym_t = ctx.symbols.intern("T");
        let f = ctx
            .func_get(ctx.root_scope_id, ctx.symbols.intern("not"))
            .expect("could not find type in root scope");
        let late_bound_generic: Rc<_> = Ty::LateBound(TyLateBound {
            name: sym_t,
            ty: Ty::Int(TyInt::from_set(IntegerSet::new_from_range(0, 1))).into(),
        })
        .into();
        assert_eq!(f.params, vec![late_bound_generic.clone()]);
        assert_eq!(f.returns, late_bound_generic);
        assert_eq!(
            f.check_params(&root_ctx, &[t_int!(0)], &[t_int!(0)]),
            t_int!(0)
        );
        let errs = ctx.errors.errors.into_iter().collect::<Vec<_>>();
        assert!(errs.is_empty(), "errors = {errs:?}");
    }
}
