use lrpar::Span;
use num_bigint::BigInt;

use crate::tree::NodeId;

use super::AstNode;


#[derive(Debug, Clone, PartialEq)]
pub struct LiteralInteger {
    pub value: BigInt,
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
    pub struct_ty: NodeId<AstNode>,
    pub members: Vec<NodeId<AstNode>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LiteralArray {
    pub values_ty: NodeId<AstNode>,
    pub values: Vec<NodeId<AstNode>>,
}
