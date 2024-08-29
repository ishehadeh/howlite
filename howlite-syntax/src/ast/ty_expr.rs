use logos::Span;
use num_bigint::BigInt;

use crate::treeslab::NodeId;

use super::AstNode;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyArray {
    pub element_ty: NodeId<AstNode>,
    pub length: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TySlice {
    pub element_ty: NodeId<AstNode>,
    pub length_ty: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyExprUnion {
    pub lhs: NodeId<AstNode>,
    pub rhs: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyStructMember {
    pub mutable: bool,
    pub name: Span,
    pub ty: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyNamed {
    pub name: NodeId<AstNode>,
    pub parameters: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyRef {
    pub referenced_ty: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyStruct {
    pub members: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberRange {
    pub lo: NodeId<AstNode>,
    pub hi: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyUnit {}
