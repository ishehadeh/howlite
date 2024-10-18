use crate::gen_node_impls;
use allocator_api2::{alloc::Allocator, vec::Vec};
use lrpar::Span;

use crate::tree::NodeId;

use super::AstNode;

#[derive(Debug, Clone)]
pub struct DefType<A: Allocator> {
    pub name: Span,
    pub alias: bool,
    pub ty: NodeId<AstNode>,
    pub ty_params: Vec<NodeId<AstNode>, A>,
}
gen_node_impls!(DefType<A> { name, alias, &ty_params*, &ty });

#[derive(Debug, Clone)]
pub struct DefExternFunc<A: Allocator> {
    pub name: Span,
    pub ty_params: Vec<NodeId<AstNode>, A>,
    pub params: Vec<NodeId<AstNode>, A>,
    pub return_ty: NodeId<AstNode>,
}
gen_node_impls!(DefExternFunc<A> { name, &ty_params*, &params*, &return_ty });

#[derive(Debug, Clone, PartialEq)]
pub struct DefExternVar {
    pub name: Span,
    pub mutable: bool,
    pub ty: NodeId<AstNode>,
}
gen_node_impls!(DefExternVar { name, mutable, &ty });

#[derive(Debug, Clone)]
pub struct DefImport {
    pub file: Span,
}
gen_node_impls!(DefImport { file });

#[derive(Debug, Clone)]
pub struct DefFunc<A: Allocator> {
    pub name: Span,
    pub ty_params: Vec<NodeId<AstNode>, A>,
    pub params: Vec<NodeId<AstNode>, A>,
    pub return_ty: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
}
gen_node_impls!(DefFunc<A> { name, &ty_params*, &params*, &return_ty, &body });

#[derive(Debug, Clone, PartialEq)]
pub struct DefParam {
    pub mutable: bool,
    pub name: Span,
    pub ty: NodeId<AstNode>,
}
gen_node_impls!(DefParam { name, mutable, &ty });
