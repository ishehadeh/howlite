use allocator_api2::{alloc::Allocator, vec::Vec};
use lrpar::Span;

use crate::tree::NodeId;

use super::AstNode;

#[derive(Debug, Clone, PartialEq)]
pub struct DefType<A: Allocator> {
    pub name: Span,
    pub alias: bool,
    pub ty: NodeId<AstNode>,
    pub ty_params: Vec<NodeId<AstNode>, A>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefExternFunc<A: Allocator> {
    pub name: Span,
    pub params: Vec<NodeId<AstNode>, A>,
    pub ty_params: Vec<NodeId<AstNode>, A>,
    pub return_ty: NodeId<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefExternVar {
    pub name: Span,
    pub mutable: bool,
    pub ty: NodeId<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefImport<A: Allocator> {
    pub file: Span,
    pub identifiers: Option<Vec<NodeId<AstNode>, A>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefFunc<A: Allocator> {
    pub name: Span,
    pub params: Vec<NodeId<AstNode>, A>,
    pub ty_params: Vec<NodeId<AstNode>, A>,
    pub return_ty: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefParam {
    pub mutable: bool,
    pub name: Span,
    pub ty: NodeId<AstNode>,
}
