use lrpar::Span;
use proptest::{
    prelude::{Just, Strategy},
    prop_oneof,
};
pub mod string;

use crate::ast::{BoxAstNode, ExprInfix, InfixOp, LiteralInteger};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Radix {
    Bin,
    Dec,
    Oct,
    Hex,
}

pub fn radix() -> impl Strategy<Value = Radix> + Clone {
    prop_oneof![
        Just(Radix::Bin),
        Just(Radix::Dec),
        Just(Radix::Oct),
        Just(Radix::Hex),
    ]
}

pub fn infix_op() -> impl Strategy<Value = InfixOp> + Clone {
    prop_oneof![
        Just(InfixOp::Add),
        Just(InfixOp::Sub),
        Just(InfixOp::Div),
        Just(InfixOp::Mul),
        Just(InfixOp::Assign),
        Just(InfixOp::CmpNe),
        Just(InfixOp::CmpEq),
        Just(InfixOp::CmpGt),
        Just(InfixOp::CmpLt),
        Just(InfixOp::CmpGtEq),
        Just(InfixOp::CmpLtEq),
        Just(InfixOp::BitOr),
        Just(InfixOp::BitAnd),
        Just(InfixOp::BitXor),
        Just(InfixOp::BitLShift),
        Just(InfixOp::BitRShift),
        Just(InfixOp::LogicalOr),
        Just(InfixOp::LogicalAnd),
    ]
}

impl Radix {
    pub fn prefix(&self) -> &'static str {
        match self {
            Radix::Bin => "0b",
            Radix::Dec => "",
            Radix::Oct => "0o",
            Radix::Hex => "0x",
        }
    }

    pub fn write_digits(&self, s: &mut String, u: u64) {
        use std::fmt::Write;

        match self {
            Radix::Bin => write!(s, "{:b}", u).unwrap(),
            Radix::Dec => write!(s, "{}", u).unwrap(),
            Radix::Oct => write!(s, "{:o}", u).unwrap(),
            Radix::Hex => write!(s, "{:x}", u).unwrap(),
        }
    }
}

pub fn literal_integer(s: impl Strategy<Value = i128>) -> impl Strategy<Value = BoxAstNode> {
    s.prop_map(|value| BoxAstNode::new(Span::new(0, 0), LiteralInteger { value }))
}

pub fn expr_infix(
    lhs: impl Strategy<Value = BoxAstNode>,
    rhs: impl Strategy<Value = BoxAstNode>,
    op: impl Strategy<Value = InfixOp>,
) -> impl Strategy<Value = BoxAstNode> {
    (lhs, op, rhs)
        .prop_map(|(lhs, op, rhs)| BoxAstNode::new(Span::new(0, 0), ExprInfix { lhs, op, rhs }))
}
