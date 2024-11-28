use std::rc::Rc;

use howlite_syntax::ast::{ArrayAccess, FieldAccess};
use howlite_typecheck::Ty;
use preseli::IntegerSet;
use sunstone::ops::{PartialBounded, Subset};

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol, CompilationErrorKind};

use super::SynthesizeTy;

impl SynthesizeTy for FieldAccess {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let base_ty = ctx.child(self.lhs).synthesize_ty();
        let field_symbol = ctx.sym_intern(&self.field);
        match base_ty.access_field(field_symbol) {
            Ok(v) => v,
            Err(e) => {
                ctx.error(CompilationErrorKind::FieldDoesNotExists {
                    base: base_ty,
                    field: self.field.clone(),
                    source: e,
                });
                Rc::new(Ty::Hole)
            }
        }
    }
}

impl SynthesizeTy for ArrayAccess {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let base_ty = ctx.child(self.lhs).synthesize_ty();
        let index_ty = ctx.child(self.index).synthesize_ty();

        let (array_elem_val, array_index_set) = match &*base_ty {
            Ty::Array(a) => (
                a.element_ty.clone(),
                IntegerSet::new_from_range(0, a.length as i128),
            ),
            Ty::Slice(a) => (
                a.element_ty.clone(),
                // this *SHOULD* never fail
                IntegerSet::new_from_range(
                    0,
                    *a.get_allowed_indexes().unwrap().partial_hi().unwrap(),
                ),
            ),
            _ => {
                ctx.error(CompilationErrorKind::UnexpectedArrayAccessTy(
                    base_ty.clone(),
                ));
                return base_ty;
            }
        };
        let index_val = match index_ty.as_int() {
            Some(v) => &v.values,
            None => {
                ctx.error(CompilationErrorKind::UnexpectedArrayAccessIndexTy(
                    index_ty.clone(),
                ));
                return array_elem_val;
            }
        };
        if !index_val.subset_of(&array_index_set) {
            ctx.error(CompilationErrorKind::IndexDoesNotExists {
                base: base_ty.clone(),
                index: index_val.clone(),
            });
            return array_elem_val;
        }

        array_elem_val
    }
}

#[cfg(test)]
mod test {
    use howlite_typecheck::t_int;

    use crate::{langctx::LangCtx, typetree::test_helpers::must_parse_expr};

    #[test]
    fn struct_access() {
        let (block_node_id, ast) = must_parse_expr(
            r#"
        {
            let a: { _0: 0..10, _1: 3..5} = #{ _0: 3, _1: 3};
            a._0 + a._1
        }
        "#,
        );
        let ctx = LangCtx::new(&ast);
        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty(),
            t_int!(6)
        )
    }

    #[test]
    fn array_access() {
        let (block_node_id, ast) = must_parse_expr(
            r#"
        {
            let a: [0..100;  5] = #[0, 1, 2, 3, 4];
            a[0]
        }
        "#,
        );
        let ctx = LangCtx::new(&ast);
        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty(),
            t_int!(0..4)
        )
    }
}
