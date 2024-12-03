use std::rc::Rc;

use howlite_syntax::{
    ast::{ExprInfix, ExprTypeConstruction, InfixOp, PrefixOp},
    AstNodeData,
};
use howlite_typecheck::{
    types::{StorageClass, TyInt},
    Ty, TyArray, TyReference,
};
use preseli::IntegerSet;
use sunstone::ops::ArithmeticSet;
use tracing::debug;
use tracing_subscriber::field::debug;

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
                let mut new_ty = rhs;
                let mut top_node = self.lhs;
                loop {
                    match &ctx.get_node(top_node).data {
                        AstNodeData::Ident(ident) => {
                            let var_sym = ctx.sym_intern(&ident.symbol);
                            debug!(?new_ty, var=?ctx.var_get(var_sym), "assign symbol");

                            if let Some(Err(err)) = ctx
                                .var_get(var_sym)
                                .map(|v| new_ty.is_assignable_to(&v.assumed_ty))
                            {
                                ctx.error(CompilationErrorKind::InvalidAssignment(err))
                            }

                            if !ctx.var_update(var_sym, |mut d| {
                                d.last_assignment = new_ty.clone();
                                d
                            }) {
                                ctx.error(CompilationErrorKind::UnknownVariable {
                                    name: ident.symbol.clone(),
                                });
                            }
                            break;
                        }
                        AstNodeData::ArrayAccess(idx) => {
                            let arr_ty = ctx.child(idx.lhs).synthesize_ty();
                            if let Some(arr) = arr_ty.as_array() {
                                if arr.length == 1 {
                                    new_ty = Rc::new(Ty::Array(TyArray {
                                        length: 1,
                                        element_ty: new_ty.clone(),
                                    }));
                                    top_node = idx.lhs;
                                    continue;
                                }
                            }
                            break;
                        }
                        AstNodeData::ExprPrefix(prefix) if prefix.op == PrefixOp::Deref => {
                            let operand_ty = ctx.child(prefix.rhs).synthesize_ty();
                            if operand_ty.as_reference().is_none() {
                                ctx.error(CompilationErrorKind::DerefNonReference(
                                    operand_ty.clone(),
                                ));
                            }
                            // even if its not just assume they intended it to be reference
                            new_ty = Rc::new(Ty::Reference(TyReference {
                                referenced_ty: new_ty,
                            }));

                            top_node = prefix.rhs;
                        }
                        AstNodeData::FieldAccess(field) => {
                            let struc_ty = ctx.child(field.lhs).synthesize_ty();
                            new_ty =
                                match struc_ty.assign_field(ctx.sym_intern(&field.field), new_ty) {
                                    Ok(v) => v,
                                    Err(e) => {
                                        ctx.error(CompilationErrorKind::FieldDoesNotExists {
                                            base: struc_ty,
                                            field: field.field.clone(),
                                            source: e,
                                        });
                                        Rc::new(Ty::Hole)
                                    }
                                };
                            top_node = field.lhs
                        }
                        _ => {
                            ctx.error(CompilationErrorKind::CannotAssign);
                            new_ty = Rc::new(Ty::Hole);
                            break;
                        }
                    }
                }

                Ok(new_ty)
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
