use crate::{gen_node_impls, tree::NodeId, TreeChildren};

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

gen_node_impls!(ExprPrefix { op, &rhs });
