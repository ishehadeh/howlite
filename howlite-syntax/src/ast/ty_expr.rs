use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
use lrpar::Span;

use crate::{tree::NodeId, TreeChildren};

use super::AstNode;

#[derive(Debug, Clone, PartialEq)]
pub struct TyArray {
    pub element_ty: NodeId<AstNode>,
    pub length: i128,
}

impl TreeChildren<AstNode> for TyArray {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.element_ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TySlice {
    pub element_ty: NodeId<AstNode>,
    pub length_ty: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for TySlice {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.element_ty).chain(std::iter::once(self.length_ty))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyExprUnion {
    pub lhs: NodeId<AstNode>,
    pub rhs: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for TyExprUnion {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.lhs).chain(std::iter::once(self.rhs))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyStructMember {
    pub mutable: bool,
    pub name: Span,
    pub ty: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for TyStructMember {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.ty)
    }
}

#[derive(Debug, Clone)]
pub struct TyNamed<A: Allocator = Global> {
    pub name: NodeId<AstNode>,
    pub parameters: Vec<NodeId<AstNode>, A>,
}

impl<A: Allocator> TreeChildren<AstNode> for TyNamed<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.name).chain(self.parameters.iter().copied())
    }
}

impl<A: Allocator> PartialEq for TyNamed<A> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.parameters == other.parameters
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyRef {
    pub referenced_ty: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for TyRef {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.referenced_ty)
    }
}

#[derive(Debug, Clone)]
pub struct TyStruct<A: Allocator> {
    pub members: Vec<NodeId<AstNode>, A>,
}

impl<A: Allocator> TreeChildren<AstNode> for TyStruct<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.members.iter().copied()
    }
}

impl<A: Allocator> PartialEq for TyStruct<A> {
    fn eq(&self, other: &Self) -> bool {
        self.members == other.members
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberRange {
    pub lo: NodeId<AstNode>,
    pub hi: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for TyNumberRange {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        [self.lo, self.hi].into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyParam {
    pub name: Span,
    pub super_ty: NodeId<AstNode>,

    pub default_ty: Option<NodeId<AstNode>>,
}

impl TreeChildren<AstNode> for TyParam {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        Some(self.super_ty).into_iter().chain(self.default_ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyUnit {}

impl TreeChildren<AstNode> for TyUnit {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::empty()
    }
}
