use allocator_api2::{alloc::Allocator, vec::Vec};
use lrpar::Span;

use crate::tree::NodeId;

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

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralStruct<A: Allocator> {
    pub members: Vec<NodeId<AstNode>, A>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralArray<A: Allocator> {
    pub values: Vec<NodeId<AstNode>, A>,
}
