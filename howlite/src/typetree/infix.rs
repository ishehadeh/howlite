use std::rc::Rc;

use howlite_syntax::{
    ast::{ExprInfix, InfixOp},
    AstNode, Span,
};
use howlite_typecheck::Ty;

use crate::{langctx::LangCtx, symtab::Symbol, CompilationError};

use super::SynthesizeTy;

impl SynthesizeTy<Span> for AstNode<&ExprInfix<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        match self.data.op {
            InfixOp::Add => match self.data.lhs.arith_add(&self.data.rhs) {
                Ok(v) => Rc::new(v),
                Err(e) => {
                    ctx.error(CompilationError {
                        location: self.span,
                        kind: e.into(),
                    });
                    Rc::new(Ty::Hole)
                }
            },
            _ => todo!(),
        }
    }
}
