use crate::{tree::NodeId, TreeChildren};

use super::AstNode;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfixOp {
    Add,
    Sub,
    Div,
    Mul,

    Assign,

    CmpNe,
    CmpEq,
    CmpGt,
    CmpLt,
    CmpGtEq,
    CmpLtEq,

    BitOr,
    BitAnd,
    BitXor,
    BitLShift,
    BitRShift,

    LogicalOr,
    LogicalAnd,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprInfix {
    pub lhs: NodeId<AstNode>,
    pub op: InfixOp,
    pub rhs: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for ExprInfix {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.lhs).chain(std::iter::once(self.rhs))
    }
}

impl InfixOp {
    pub fn as_token(&self) -> &'static str {
        match self {
            InfixOp::Add => "+",
            InfixOp::Sub => "-",
            InfixOp::Div => "/",
            InfixOp::Mul => "*",
            InfixOp::Assign => "=",
            InfixOp::CmpNe => "!=",
            InfixOp::CmpEq => "==",
            InfixOp::CmpGt => ">",
            InfixOp::CmpLt => "<",
            InfixOp::CmpGtEq => ">=",
            InfixOp::CmpLtEq => "<=",
            InfixOp::BitOr => "|",
            InfixOp::BitAnd => "&",
            InfixOp::BitXor => "^",
            InfixOp::BitLShift => "<<",
            InfixOp::BitRShift => ">>",
            InfixOp::LogicalOr => "||",
            InfixOp::LogicalAnd => "&&",
        }
    }
}
