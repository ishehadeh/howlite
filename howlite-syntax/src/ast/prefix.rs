use crate::{gen_node_impls, tree::DefaultLinearTreeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum PrefixOp {
    LogicalNot,
    Minus,
    Plus,
    BitNot,
    Deref,
    Ref,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprPrefix<ChildT = DefaultLinearTreeId> {
    pub op: PrefixOp,
    pub rhs: ChildT,
}

gen_node_impls!(ExprPrefix { op, &rhs });

impl PrefixOp {
    pub fn as_token(&self) -> &'static str {
        match self {
            PrefixOp::LogicalNot => "!",
            PrefixOp::Minus => "-",
            PrefixOp::Plus => "+",
            PrefixOp::BitNot => "~",
            PrefixOp::Deref => "*",
            PrefixOp::Ref => "&",
        }
    }
}

impl std::fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.as_token())
    }
}
