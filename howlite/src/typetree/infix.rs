use std::rc::Rc;

use howlite_syntax::{
    ast::{ExprInfix, InfixOp},
    AstNode, Span,
};
use howlite_typecheck::Ty;
use preseli::IntegerSet;
use sunstone::ops::ArithmeticSet;

use crate::{langctx::LangCtx, symtab::Symbol, CompilationError};

use super::SynthesizeTy;
fn do_op<F>(
    op: F,
    ctx: &LangCtx<Span>,
    loc: Span,
    lhs: Rc<Ty<Symbol>>,
    rhs: Rc<Ty<Symbol>>,
) -> Rc<Ty<Symbol>>
where
    F: FnOnce(IntegerSet, &IntegerSet) -> IntegerSet,
{
    match lhs.arithmetic(&*rhs, |a, b| op(a.clone(), b)) {
        Ok(v) => Rc::new(v),
        Err(e) => {
            ctx.error(CompilationError {
                location: loc,
                kind: e.into(),
            });
            Rc::new(Ty::Hole)
        }
    }
}
impl SynthesizeTy<Span> for AstNode<&ExprInfix<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let ExprInfix { lhs, op, rhs } = self.data;

        let op_result = match op {
            InfixOp::Add => lhs.arithmetic_rec(&*rhs, |a, b| a.add_all(b)),
            InfixOp::Mul => lhs.arithmetic_rec(&*rhs, |a, b| a.mul_all(b)),
            InfixOp::Div => lhs.arithmetic_rec(&*rhs, |a, b| a.div_all(b)),
            InfixOp::Sub => lhs.arithmetic_rec(&*rhs, |a, b| a.sub_all(b)),
            op => todo!("infix op: {op:?}"),
        };
        match op_result {
            Ok(v) => Rc::new(v),
            Err(e) => {
                ctx.error(CompilationError {
                    location: self.span,
                    kind: e.into(),
                });
                Rc::new(Ty::Hole)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use howlite_typecheck::t_int;

    use crate::{
        langctx::LangCtx,
        typetree::{test_helpers::must_parse_expr, TypeTreeBuilder},
    };

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
        let ctx = LangCtx::new();
        let tt_builder = TypeTreeBuilder::new(&ctx, &ast);
        assert_eq!(tt_builder.get_ty(block_node_id), t_int!(3))
    }
}
