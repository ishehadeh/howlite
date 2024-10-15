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
pub struct LiteralStruct {
    pub members: Vec<NodeId<AstNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralArray {
    pub values: Vec<NodeId<AstNode>>,
}
