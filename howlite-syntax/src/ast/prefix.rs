use crate::tree::NodeId;

use super::AstNode;


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrefixOp {
    LogicalNot,
    Minus,
    Plus,
    BitNot,
    Deref,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ExprPrefix {
    pub op: PrefixOp,
    pub rhs: NodeId<AstNode>,
}
