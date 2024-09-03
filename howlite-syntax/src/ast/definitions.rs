use lrpar::Span;

use crate::tree::NodeId;

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
pub struct DefExternFunc {
    pub name: Span,
    pub params: Vec<NodeId<AstNode>>,
    pub ty_params: Vec<NodeId<AstNode>>,
    pub return_ty: NodeId<AstNode>,
}
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefExternVar {
    pub name: Span,
    pub mutable: bool,
    pub ty: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefImport {
    pub file: Span,
    pub identifiers: Option<Vec<NodeId<AstNode>>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefFunc {
    pub name: Span,
    pub params: Vec<NodeId<AstNode>>,
    pub ty_params: Vec<NodeId<AstNode>>,
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
