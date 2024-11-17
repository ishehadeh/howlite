use std::rc::Rc;

use crate::{
    langctx::{lexicalctx::LexicalContext, VarDef},
    symtab::Symbol,
};
use howlite_syntax::ast::ExprLet;
use howlite_typecheck::Ty;

use super::SynthesizeTy;

impl SynthesizeTy for ExprLet<Rc<Ty<Symbol>>> {
    fn synthesize_ty<L: Clone>(self, ctx: &LexicalContext<L>) -> Rc<Ty<Symbol>> {
        let var_symbol = ctx.sym_intern(self.name.as_str());
        let var_ty = self.ty;
        let var_value_ty = self.value;
        ctx.var_def(
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
    use crate::{get_node_type, typetree::test_helpers::simple_scalar_let};

    use proptest::prelude::*;

    proptest! {

        #[test]
        fn let_expr_simple(program in simple_scalar_let()) {
            let ty = get_node_type!(program);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }
    }
}
