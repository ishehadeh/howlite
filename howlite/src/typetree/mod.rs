//! Type Tree
//! -----------
//!
//! This module maps AST nodes to types.
//!
//! Depending on the node, the associated type may have different meanings.
//! For example, ast::Ty[...] nodes are mapped to the actual type they represent.
//! On the other hand, most expressions are mapped to the type representing the set of possible values
//! that would result from the given expression.

mod access;
mod atomic_exprs;
mod constraint_term;
mod constraint_tree;
mod infix;
mod original;
mod traits;
mod ty;

// pub use infix::*;

use std::rc::Rc;

pub use constraint_term::*;
// pub use constraint_tree::*;

#[cfg(test)]
mod test_helpers;

use howlite_syntax::AstNodeData;
use howlite_typecheck::Ty;
pub use traits::{SynthesizeTy, SynthesizeTyPure};

use crate::{
    langctx::{lexicalctx::LexicalContext, Scope},
    symtab::Symbol,
};

pub struct Implication {
    pub predicate: preseli::IntegerSet,
    pub conculsion: Scope,
}

impl SynthesizeTy for AstNodeData {
    fn synthesize_ty(&self, ctx: &LexicalContext) -> Rc<Ty<Symbol>> {
        match self {
            AstNodeData::LiteralInteger(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralString(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralChar(n) => n.synthesize_ty(ctx),
            AstNodeData::ExprInfix(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralStruct(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralArray(n) => n.synthesize_ty(ctx),
            AstNodeData::ExprLet(n) => n.synthesize_ty(ctx),

            AstNodeData::TyNumberRange(n) => n.synthesize_ty(ctx),
            AstNodeData::Ident(n) => n.synthesize_ty(ctx),
            AstNodeData::TyNamed(_) => todo!(),
            AstNodeData::Block(n) => {
                let child_scope_ctx = ctx.new_with_scope();

                if n.returns {
                    n.statements
                        .iter()
                        .map(|&stmt| child_scope_ctx.child(stmt).synthesize_ty())
                        .last()
                        .unwrap_or_else(|| Rc::new(Ty::unit()))
                } else {
                    Rc::new(Ty::unit())
                }
            }

            AstNodeData::ExprIf(v) => v.synthesize_ty(ctx),
            

            AstNodeData::FieldAccess(f) => f.synthesize_ty(ctx),
            AstNodeData::ArrayAccess(a) => a.synthesize_ty(ctx),
            AstNodeData::Repaired(_) => todo!(),
            AstNodeData::DefFunc(_) => todo!(),
            AstNodeData::DefParam(_) => todo!(),
            AstNodeData::DefImport(_) => todo!(),
            AstNodeData::ExprCall(_) => todo!(),
            AstNodeData::ExprPrefix(_) => todo!(),
            AstNodeData::ExprTypeConstruction(_) => todo!(),
            AstNodeData::ExprWhile(_) => todo!(),
            AstNodeData::DefType(_) => todo!(),
            AstNodeData::DefExternFunc(_) => todo!(),
            AstNodeData::DefExternVar(_) => todo!(),
            AstNodeData::Program(_) => todo!(),
            AstNodeData::TyRef(_) => todo!(),
            AstNodeData::TyExprUnion(_) => todo!(),
            AstNodeData::TyStruct(ts) => ts.synthesize_ty(ctx),
            AstNodeData::TyArray(ta) => ta.synthesize_ty(ctx),
            AstNodeData::TyUnit(tu) => tu.synthesize_ty(ctx),
            AstNodeData::TyParam(_) => todo!(),
            AstNodeData::TySlice(_) => todo!(),

            AstNodeData::TyStructMember(_) => unreachable!("TyStruct handles its children, we should never try to synthesize a TyStructMember directly"),
            AstNodeData::LiteralStructMember(_) => {
                unreachable!("literal struct member should never appear in the AST")
            }
        }
    }
}
