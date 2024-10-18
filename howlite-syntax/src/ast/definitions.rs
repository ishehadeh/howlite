use allocator_api2::{alloc::Allocator, vec::Vec};
use lrpar::Span;

use crate::{tree::NodeId, TreeChildren};

use super::AstNode;

#[derive(Debug, Clone)]
pub struct DefType<A: Allocator> {
    pub name: Span,
    pub alias: bool,
    pub ty: NodeId<AstNode>,
    pub ty_params: Vec<NodeId<AstNode>, A>,
}

impl<A: Allocator> TreeChildren<AstNode> for DefType<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.ty).chain(self.ty_params.iter().copied())
    }
}

impl<A: Allocator> PartialEq for DefType<A> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.alias == other.alias
            && self.ty == other.ty
            && self.ty_params == other.ty_params
    }
}

#[derive(Debug, Clone)]
pub struct DefExternFunc<A: Allocator> {
    pub name: Span,
    pub ty_params: Vec<NodeId<AstNode>, A>,
    pub params: Vec<NodeId<AstNode>, A>,
    pub return_ty: NodeId<AstNode>,
}

impl<A: Allocator> TreeChildren<AstNode> for DefExternFunc<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.ty_params
            .iter()
            .copied()
            .chain(self.params.iter().copied())
            .chain(std::iter::once(self.return_ty))
    }
}

impl<A: Allocator> PartialEq for DefExternFunc<A> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.params == other.params
            && self.ty_params == other.ty_params
            && self.return_ty == other.return_ty
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefExternVar {
    pub name: Span,
    pub mutable: bool,
    pub ty: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for DefExternVar {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.ty)
    }
}

#[derive(Debug, Clone)]
pub struct DefImport<A: Allocator> {
    pub file: Span,
    pub identifiers: Option<Vec<NodeId<AstNode>, A>>,
}

impl<A: Allocator> TreeChildren<AstNode> for DefImport<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.identifiers.iter().flat_map(|v| v.iter()).copied()
    }
}

impl<A: Allocator> PartialEq for DefImport<A> {
    fn eq(&self, other: &Self) -> bool {
        self.file == other.file && self.identifiers == other.identifiers
    }
}

#[derive(Debug, Clone)]
pub struct DefFunc<A: Allocator> {
    pub name: Span,
    pub ty_params: Vec<NodeId<AstNode>, A>,
    pub params: Vec<NodeId<AstNode>, A>,
    pub return_ty: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
}

impl<A: Allocator> TreeChildren<AstNode> for DefFunc<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.ty_params
            .iter()
            .copied()
            .chain(self.params.iter().copied())
            .chain(std::iter::once(self.return_ty))
    }
}

impl<A: Allocator> PartialEq for DefFunc<A> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.params == other.params
            && self.ty_params == other.ty_params
            && self.return_ty == other.return_ty
            && self.body == other.body
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefParam {
    pub mutable: bool,
    pub name: Span,
    pub ty: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for DefParam {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.ty)
    }
}
