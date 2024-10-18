use allocator_api2::{alloc::Allocator, vec::Vec};
use lrpar::Span;

use crate::{gen_node_impls, tree::NodeId};

use super::AstNode;

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralInteger {
    pub value: i128,
}
gen_node_impls!(LiteralInteger { value });

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralChar {
    pub value: Span,
}
gen_node_impls!(LiteralChar { value });

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralString {
    pub value: Span,
}
gen_node_impls!(LiteralString { value });

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralStructMember {
    pub field: Span,
    pub value: NodeId<AstNode>,
}
gen_node_impls!(LiteralStructMember { field, &value });

#[derive(Debug, Clone)]
pub struct LiteralStruct<A: Allocator> {
    pub members: Vec<NodeId<AstNode>, A>,
}
gen_node_impls!(LiteralStruct<A> { &members* });

#[derive(Debug, Clone)]
pub struct LiteralArray<A: Allocator> {
    pub values: Vec<NodeId<AstNode>, A>,
}
gen_node_impls!(LiteralArray<A> { &values* });
