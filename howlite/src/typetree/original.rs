use std::rc::Rc;

use crate::{
    langctx::{LangCtx, VarDef},
    symtab::Symbol,
};
use howlite_syntax::{ast::ExprLet, AstNode, Span};
use howlite_typecheck::Ty;

use super::SynthesizeTy;

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
        typetree::{test_helpers::simple_scalar_let, SynthesizeTy},
    };

    use super::LangCtx;
    use howlite_syntax::Span;
    use proptest::prelude::*;

    proptest! {

        #[test]
        fn let_expr_simple(program in simple_scalar_let()) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert_lang_ok!(lang);
            assert!(ty.as_int().is_some(), "expected int type, got: {:?}", ty);
        }
    }
}
