use crate::{tree::NodeId, TreeChildren};

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

impl TreeChildren<AstNode> for ExprPrefix {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.rhs)
    }
}
