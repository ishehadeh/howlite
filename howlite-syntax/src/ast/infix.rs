use crate::treeslab::NodeId;

use super::AstNode;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfixOp {
    Add,
    Sub,
    Div,
    Mul,

    Assign, // TODO

    CmpNe, // TODO
    CmpEq, // TODO
    CmpGt, // TODO
    CmpLt, // TODO
    CmpGe, // TODO
    CmpLe, // TODO

    BitOr,
    BitAnd,
    BitXor,
    BitLShift,
    BitRShift,

    LogicalOr,  // TODO
    LogicalAnd, // TODO
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ExprInfix {
    pub lhs: NodeId<AstNode>,
    pub op: InfixOp,
    pub rhs: NodeId<AstNode>,
}
