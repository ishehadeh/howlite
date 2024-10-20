use crate::{gen_node_impls, tree::DefaultLinearTreeId};

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
pub struct ExprPrefix<ChildT = DefaultLinearTreeId> {
    pub op: PrefixOp,
    pub rhs: ChildT,
}

gen_node_impls!(ExprPrefix { op, &rhs });
