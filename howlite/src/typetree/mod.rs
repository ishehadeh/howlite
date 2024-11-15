//! Type Tree
//! -----------
//!
//! This module maps AST nodes to types.
//!
//! Depending on the node, the associated type may have different meanings.
//! For example, ast::Ty[...] nodes are mapped to the actual type they represent.
//! On the other hand, most expressions are mapped to the type representing the set of possible values
//! that would result from the given expression.

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
    tree::{DefaultLinearTreeId, LinearTreeNodeId, Tree},
    AstNode, AstNodeData, Span,
};
use howlite_typecheck::Ty;
// pub use original::*;
pub use traits::{SynthesizeTy, SynthesizeTyPure};

use crate::{langctx::LangCtx, symtab::Symbol};

pub struct TypeTreeBuilder<'a, 'b> {
    tree: &'a Tree<AstNode>,
    ctx: &'b LangCtx<Span>,
    ty: DashMap<DefaultLinearTreeId, Rc<Ty<Symbol>>>,
}

impl<'a, 'b> TypeTreeBuilder<'a, 'b> {
    pub fn new(ctx: &'b LangCtx<Span>, tree: &'a Tree<AstNode>) -> Self {
        Self {
            tree,
            ty: DashMap::new(),
            ctx,
        }
    }

    fn calc_ty(&self, node_id: DefaultLinearTreeId) -> Rc<Ty<Symbol>> {
        let node = self.tree.get(node_id);
        let node = node.clone().map(|child_id| self.get_ty(child_id));
        let ty = node.synthesize_ty(self.ctx);
        _ = self.ty.insert(node_id, ty.clone());
        ty
    }

    pub fn get_ty(&self, node_id: DefaultLinearTreeId) -> Rc<Ty<Symbol>> {
        self.ty
            .get(&node_id)
            .map(|c| c.value().clone())
            .unwrap_or_else(|| self.calc_ty(node_id))
    }
}

impl SynthesizeTy<Span> for AstNode<AstNodeData<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let AstNode { data, span } = self;
        match data {
            AstNodeData::LiteralInteger(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralString(n) => AstNode::new_narrow(span, &n).synthesize_ty(ctx),
            AstNodeData::LiteralChar(n) => AstNode::new_narrow(span, &n).synthesize_ty(ctx),
            AstNodeData::ExprInfix(n) => AstNode::new_narrow(span, &n).synthesize_ty(ctx),
            AstNodeData::LiteralStruct(n) => AstNode::new_narrow(span, n).synthesize_ty(ctx),
            AstNodeData::LiteralArray(n) => AstNode::new_narrow(span, n).synthesize_ty(ctx),
            AstNodeData::ExprLet(n) => AstNode::new_narrow(span, n).synthesize_ty(ctx),

            AstNodeData::TyNumberRange(n) => AstNode::new_narrow(span, n).synthesize_ty(ctx),
            AstNodeData::Ident(n) => ctx
                .var_get(ctx.root_scope_id, ctx.symbols.intern(&n.symbol))
                .unwrap()
                .last_assignment
                .clone(),
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
            t => todo!("Ast node not implemented for type checker: {:?}", t),
        }
    }
}
