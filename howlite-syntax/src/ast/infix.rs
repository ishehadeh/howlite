use crate::{gen_node_impls, tree::DefaultLinearTreeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprInfix<ChildT = DefaultLinearTreeId> {
    pub lhs: ChildT,
    pub op: InfixOp,
    pub rhs: ChildT,
}
gen_node_impls!(ExprInfix { &lhs, op, &rhs });

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
