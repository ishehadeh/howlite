use std::rc::Rc;

use howlite_syntax::ast::{ExprInfix, InfixOp};
use howlite_typecheck::Ty;
use sunstone::ops::ArithmeticSet;

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol};

use super::SynthesizeTy;

impl SynthesizeTy for ExprInfix {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        let lhs = ctx.child(self.lhs).synthesize_ty();
        let rhs = ctx.child(self.rhs).synthesize_ty();
        let op_result = match self.op {
            InfixOp::Add => lhs.arithmetic_rec(&*rhs, |a, b| a.add_all(b)),
            InfixOp::Mul => lhs.arithmetic_rec(&*rhs, |a, b| a.mul_all(b)),
            InfixOp::Div => lhs.arithmetic_rec(&*rhs, |a, b| a.div_all(b)),
            InfixOp::Sub => lhs.arithmetic_rec(&*rhs, |a, b| a.sub_all(b)),
            op => todo!("infix op: {op:?}"),
        };
        match op_result {
            Ok(v) => Rc::new(v),
            Err(e) => {
                ctx.error(e.into());
                Rc::new(Ty::Hole)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use howlite_typecheck::t_int;

    use crate::{langctx::LangCtx, typetree::test_helpers::must_parse_expr};

    #[test]
    fn average_3() {
        let (block_node_id, ast) = must_parse_expr(
            r#"
        {
            let a: 0..10 = 3;
            let b: 0..10 = 5;
            let c: 0..10 = 1;
            let avg: 0..10 = (a + b + c) / 3;
            avg
        }
        "#,
        );
        let ctx = LangCtx::new(&ast);
        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty(),
            t_int!(3)
        )
    }
}
