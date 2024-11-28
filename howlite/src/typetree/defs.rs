use std::rc::Rc;

use howlite_syntax::{ast, AstNodeData};
use howlite_typecheck::{types::TyLateBound, Ty};

use crate::{
    langctx::{lexicalctx::LexicalContext, TyDef},
    symtab::Symbol,
};

use super::SynthesizeTy;

impl SynthesizeTy for ast::DefType {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let name_sym = ctx.sym_intern(&self.name);
        let mut params = Vec::with_capacity(self.ty_params.len());

        // create a new scope now, as we eval the params we'll add them as LateBound
        let body_ctx = ctx.child(self.ty).new_with_scope();
        for param in &self.ty_params {
            let param_node = ctx.get_node(*param);
            match &param_node.data {
                AstNodeData::TyParam(p) => {
                    let param_name_sym = ctx.sym_intern(&p.name);
                    let param_super_ty = ctx.child(p.super_ty).synthesize_ty();

                    let late_bound_param_ty = Rc::new(Ty::LateBound( TyLateBound {
                        name: param_name_sym,
                        ty: param_super_ty.clone(),
                    }));
                    // TODO: include default type param defs?
                    body_ctx.ty_def(TyDef {
                        name: param_name_sym,
                        ty: late_bound_param_ty.clone(),
                        params: vec![]
                    });
                    params.push((param_name_sym, param_super_ty))
                },
                _ => panic!("found non-TyParam AST ref in DefType.ty_params, this should be impossible. (assuming the parser is correct (which if your seeing this message it clearly isn't))")
            }
        }

        let ty = body_ctx.synthesize_ty();
        ctx.ty_def(TyDef {
            name: name_sym,
            params,
            ty,
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
}
