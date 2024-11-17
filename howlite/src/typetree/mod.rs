//! Type Tree
//! -----------
//!
//! This module maps AST nodes to types.
//!
//! Depending on the node, the associated type may have different meanings.
//! For example, ast::Ty[...] nodes are mapped to the actual type they represent.
//! On the other hand, most expressions are mapped to the type representing the set of possible values
//! that would result from the given expression.

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

use dashmap::DashMap;
use howlite_syntax::{
    ast::HigherOrderNode,
    tree::{DefaultLinearTreeId, Tree},
    AstNode, AstNodeData, Span,
};
use howlite_typecheck::Ty;
use traits::PrepareLexicalCtx;
// pub use original::*;
pub use traits::{SynthesizeTy, SynthesizeTyPure};

use crate::{
    langctx::{lexicalctx::LexicalContext, LangCtx, ScopeId},
    symtab::Symbol,
};

pub struct TypeTreeBuilder<'a, 'b> {
    tree: &'a Tree<AstNode>,
    ctx: &'b LangCtx<Span>,
    ty: DashMap<(ScopeId, DefaultLinearTreeId), Rc<Ty<Symbol>>>,
}

impl<'a, 'b> TypeTreeBuilder<'a, 'b> {
    pub fn new(ctx: &'b LangCtx<Span>, tree: &'a Tree<AstNode>) -> Self {
        Self {
            tree,
            ty: DashMap::new(),
            ctx,
        }
    }

    fn calc_ty(&self, node_id: DefaultLinearTreeId, scope: ScopeId) -> Rc<Ty<Symbol>> {
        let node = self.tree.get(node_id);
        let ctx = self.ctx.make_lexical_context(scope, node.span.clone());
        let child_ctx = node.data.prepare_lexical_ctx(ctx.clone());

        let node = node
            .clone()
            .map(|child_id| self.get_ty(child_id, child_ctx.get_scope()));
        let ty = node.data.synthesize_ty(&ctx);
        _ = self.ty.insert((ctx.get_scope(), node_id), ty.clone());
        ty
    }

    pub fn get_ty(&self, node_id: DefaultLinearTreeId, scope: ScopeId) -> Rc<Ty<Symbol>> {
        self.ty
            .get(&(scope, node_id))
            .map(|c| c.value().clone())
            .unwrap_or_else(|| self.calc_ty(node_id, scope))
    }
}

impl<T> PrepareLexicalCtx<Span> for AstNodeData<T> {
    fn prepare_lexical_ctx<'a>(&self, ctx: LexicalContext<'a, Span>) -> LexicalContext<'a, Span> {
        match self {
            AstNodeData::Block(_) => ctx.with_scope(),
            _ => ctx,
        }
    }
}

impl SynthesizeTy for AstNodeData<Rc<Ty<Symbol>>> {
    fn synthesize_ty<L: Clone>(self, ctx: &LexicalContext<L>) -> Rc<Ty<Symbol>> {
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
                if n.returns {
                    n.statements
                        .last()
                        .cloned()
                        .unwrap_or_else(|| Rc::new(Ty::unit()))
                } else {
                    Rc::new(Ty::unit())
                }
            }

            AstNodeData::LiteralStructMember(_) => todo!(),
            AstNodeData::FieldAccess(_) => todo!(),
            AstNodeData::ArrayAccess(_) => todo!(),
            AstNodeData::Repaired(_) => todo!(),
            AstNodeData::DefFunc(_) => todo!(),
            AstNodeData::DefParam(_) => todo!(),
            AstNodeData::DefImport(_) => todo!(),
            AstNodeData::ExprIf(_) => todo!(),
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
            AstNodeData::TyStruct(_) => todo!(),
            AstNodeData::TyStructMember(_) => todo!(),
            AstNodeData::TyArray(_) => todo!(),
            AstNodeData::TyUnit(_) => todo!(),
            AstNodeData::TyParam(_) => todo!(),
            AstNodeData::TySlice(_) => todo!(),
        }
    }
}
