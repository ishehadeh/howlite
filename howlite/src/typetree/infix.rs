use std::rc::Rc;

use howlite_syntax::{
    ast::{ExprInfix, ExprTypeConstruction, InfixOp, PrefixOp},
    AstNodeData,
};
use howlite_typecheck::{
    types::{StorageClass, TyInt}, AccessPath, Ty, TyArray, TyReference
};
use preseli::IntegerSet;
use sunstone::ops::ArithmeticSet;
use tracing::debug;

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol, CompilationErrorKind};

use super::SynthesizeTy;

impl SynthesizeTy for ExprInfix {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        let lhs = ctx.child(self.lhs).synthesize_ty();
        let rhs = ctx.child(self.rhs).synthesize_ty();

        let op_result = match self.op {
            InfixOp::CmpEq
            | InfixOp::CmpGt
            | InfixOp::CmpLt
            | InfixOp::CmpGtEq
            | InfixOp::CmpLtEq
            | InfixOp::CmpNe => Ok(Rc::new(Ty::Int(TyInt {
                values: IntegerSet::new_from_range(0, 1),
                storage: StorageClass::signed(32),
            }))),
            InfixOp::Add => lhs.arithmetic_rec(&*rhs, |a, b| a.add_all(b)).map(Rc::new),
            InfixOp::Mul => lhs.arithmetic_rec(&*rhs, |a, b| a.mul_all(b)).map(Rc::new),
            InfixOp::Div => lhs.arithmetic_rec(&*rhs, |a, b| a.div_all(b)).map(Rc::new),
            InfixOp::Sub => lhs.arithmetic_rec(&*rhs, |a, b| a.sub_all(b)).map(Rc::new),
            InfixOp::Assign => {
                let mut top = self.lhs;
                let ty = ctx.child(top).synthesize_ty();


                let mut path = AccessPath::default();
                loop {
                    match &ctx.get_node(top).data {
                        AstNodeData::Ident(ident) => {
                            let symbol = ctx.sym_intern(&ident.symbol);
                            let var_ty = ctx.get_current_var_ty_or_err(symbol);
                            let assumed_ty = ctx.var_get_or_err(symbol).assumed_ty;

                            if let Err(e) = rhs.is_assignable_to(&assumed_ty) {
                                ctx.error(CompilationErrorKind::InvalidAssignment(e));
                            }
                            ctx.var_update(symbol, |mut d| {
                                d.last_assignment = var_ty.assign_path(path.as_slice(), rhs.clone()).unwrap();
                                d
                            });
                            break;
                        }
                        AstNodeData::ArrayAccess(_) => {
                            // can't infer array types
                            break;
                        }
                        AstNodeData::ExprPrefix(prefix) if prefix.op == PrefixOp::Deref => {
                            top = prefix.rhs;
                            path.push_deref();
                        }
                        AstNodeData::FieldAccess(field) => {
                            if field.field == "len"
                                && ctx
                                    .child(field.lhs)
                                    .synthesize_ty()
                                    .as_slice()
                                    .is_some()
                            {
                                break;
                            }
                            path.push_field(ctx.sym_intern(&field.field));
                            top = field.lhs
                        }
                        _ => break,
                    }
                }

                Ok(rhs)
            }
            op => todo!("infix op: {op:?}"),
        };
        match op_result {
            Ok(v) => v,
            Err(e) => {
                ctx.error(e.into());
                Rc::new(Ty::Hole)
            }
        }
    }
}

impl SynthesizeTy for ExprTypeConstruction {
    fn synthesize_ty(&self, ctx: &LexicalContext<'_, '_>) -> Rc<Ty<Symbol>> {
        let ty = ctx.child(self.ty).synthesize_ty();
        let value = ctx.child(self.value).synthesize_ty();
        if let Err(e) = value.is_assignable_to(&*ty) {
            ctx.error(CompilationErrorKind::CannotConstruct {
                value,
                source: e,
                ty: ty.clone(),
            });
        }
        ty
    }
}

#[cfg(test)]
mod test {
    use howlite_typecheck::{t_int, t_struct};

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

    #[test]
    fn construct_struct() {
        let (block_node_id, ast) = must_parse_expr(
            r#"
        (#{
          a: 5,
          b: 2  
        } : { a: 0..100, b: 0..100 })
        "#,
        );
        let ctx = LangCtx::new(&ast);
        let sym_a = ctx.symbols.intern("a");
        let sym_b = ctx.symbols.intern("b");

        assert_eq!(
            ctx.make_lexical_context(ctx.root_scope_id, block_node_id)
                .synthesize_ty(),
            t_struct!(
                sym_a => t_int!(0..100),
                sym_b => t_int!(0..100)
            )
        )
    }
}
