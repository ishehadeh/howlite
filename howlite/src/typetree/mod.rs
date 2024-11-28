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
mod stmts;
mod traits;
mod ty;
mod prefix;
mod defs;

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
            AstNodeData::Program(p) => {
                for &def in &p.definitions {
                    ctx.child(def).synthesize_ty();
                }
                Rc::new(Ty::unit())
            }

            // literals
            AstNodeData::LiteralInteger(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralString(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralChar(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralStruct(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralArray(n) => n.synthesize_ty(ctx),

            // expression
            AstNodeData::ExprInfix(n) => n.synthesize_ty(ctx),
            AstNodeData::Ident(n) => n.synthesize_ty(ctx),
            AstNodeData::FieldAccess(f) => f.synthesize_ty(ctx),
            AstNodeData::ArrayAccess(a) => a.synthesize_ty(ctx),
            AstNodeData::ExprCall(_) => todo!(),
            AstNodeData::ExprPrefix(ep) => ep.synthesize_ty(ctx),
            AstNodeData::ExprTypeConstruction(etc) => etc.synthesize_ty(ctx),

            // statement-like expressions
            AstNodeData::ExprLet(n) => n.synthesize_ty(ctx),
            AstNodeData::ExprWhile(ew) => ew.synthesize_ty(ctx),
            AstNodeData::ExprIf(v) => v.synthesize_ty(ctx),
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

            
            AstNodeData::Repaired(_) => todo!(),

            // definitions
            AstNodeData::DefType(dt) => dt.synthesize_ty(ctx),
            AstNodeData::TyParam(_) => unreachable!("TyParam nodes should be handled by their parent directly, not passed to the generic tc dispatcher"),
            AstNodeData::DefExternFunc(_) => todo!(),
            AstNodeData::DefExternVar(_) => todo!(),
            AstNodeData::DefFunc(df) => df.synthesize_ty(ctx),
            AstNodeData::DefParam(_) => unreachable!("DefParam nodes should be handled by their parent directly, not passed to the generic tc dispatcher"),
            AstNodeData::DefImport(_) => todo!(),

            // types
            AstNodeData::TyNumberRange(n) => n.synthesize_ty(ctx),
            AstNodeData::TyNamed(tn) => tn.synthesize_ty(ctx),
            AstNodeData::TyRef(tr) => tr.synthesize_ty(ctx),
            AstNodeData::TyExprUnion(tu) => tu.synthesize_ty(ctx),
            AstNodeData::TyStruct(ts) => ts.synthesize_ty(ctx),
            AstNodeData::TyArray(ta) => ta.synthesize_ty(ctx),
            AstNodeData::TyUnit(tu) => tu.synthesize_ty(ctx),
            AstNodeData::TySlice(ts) => ts.synthesize_ty(ctx),

            // unreachable nodes
            AstNodeData::TyStructMember(_) => unreachable!("TyStruct handles its children, we should never try to synthesize a TyStructMember directly"),
            AstNodeData::LiteralStructMember(_) => {
                unreachable!("literal struct member should never appear in the AST")
            }
        }
    }
}
