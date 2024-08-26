use num_bigint::BigInt;

use crate::treeslab::NodeId;

use super::{AstNode, Ident};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralInteger {
    pub value: BigInt,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralMember {
    pub field: Ident,
    pub value: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub members: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralArray {
    pub values: Vec<NodeId<AstNode>>,
}

impl StructLiteral {
    pub fn with_member(mut self, id: NodeId<AstNode>) -> StructLiteral {
        self.members.push(id);
        self
    }
}
