use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
use lrpar::Span;

use crate::tree::NodeId;

use super::AstNode;

#[derive(Debug, Clone, PartialEq)]
pub struct TyArray {
    pub element_ty: NodeId<AstNode>,
    pub length: i128,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TySlice {
    pub element_ty: NodeId<AstNode>,
    pub length_ty: NodeId<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyExprUnion {
    pub lhs: NodeId<AstNode>,
    pub rhs: NodeId<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyStructMember {
    pub mutable: bool,
    pub name: Span,
    pub ty: NodeId<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyNamed<A: Allocator = Global> {
    pub name: NodeId<AstNode>,
    pub parameters: Vec<NodeId<AstNode>, A>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyRef {
    pub referenced_ty: NodeId<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyStruct<A: Allocator> {
    pub members: Vec<NodeId<AstNode>, A>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberRange {
    pub lo: NodeId<AstNode>,
    pub hi: NodeId<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyParam {
    pub name: Span,
    pub super_ty: NodeId<AstNode>,

    pub default_ty: Option<NodeId<AstNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyUnit {}
