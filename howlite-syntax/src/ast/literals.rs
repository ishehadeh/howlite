use allocator_api2::{alloc::Allocator, vec::Vec};
use lrpar::Span;

use crate::{tree::NodeId, TreeChildren};

use super::AstNode;

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralInteger {
    pub value: i128,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralChar {
    pub value: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralString {
    pub value: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralStructMember {
    pub field: Span,
    pub value: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for LiteralStructMember {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.value)
    }
}

#[derive(Debug, Clone)]
pub struct LiteralStruct<A: Allocator> {
    pub members: Vec<NodeId<AstNode>, A>,
}

impl<A: Allocator> TreeChildren<AstNode> for LiteralStruct<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.members.iter().copied()
    }
}

impl<A: Allocator> PartialEq for LiteralStruct<A> {
    fn eq(&self, other: &Self) -> bool {
        self.members == other.members
    }
}

#[derive(Debug, Clone)]
pub struct LiteralArray<A: Allocator> {
    pub values: Vec<NodeId<AstNode>, A>,
}

impl<A: Allocator> TreeChildren<AstNode> for LiteralArray<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.values.iter().copied()
    }
}

impl<A: Allocator> PartialEq for LiteralArray<A> {
    fn eq(&self, other: &Self) -> bool {
        self.values == other.values
    }
}
