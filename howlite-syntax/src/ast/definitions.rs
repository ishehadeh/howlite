use lrpar::Span;

use crate::treeslab::NodeId;

use super::AstNode;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefType {
    pub name: Span,
    pub alias: bool,
    pub ty: NodeId<AstNode>,
    pub ty_params: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefExtern {
    pub name: Span,
    pub params: Vec<NodeId<AstNode>>,
    pub return_ty: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefFunc {
    pub name: Span,
    pub params: Vec<NodeId<AstNode>>,
    pub return_ty: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefParam {
    pub mutable: bool,
    pub name: Span,
    pub ty: NodeId<AstNode>,
}
