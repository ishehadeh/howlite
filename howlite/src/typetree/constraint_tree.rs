use howlite_syntax::ast::{ExprInfix, InfixOp, LiteralInteger};
use preseli::IntegerSet;

use super::{traits::ToContraintTerm, BinaryConstraintRelation, ConstraintOp, ConstraintTerm};

impl ToContraintTerm for LiteralInteger {
    fn to_constraint_term(self) -> ConstraintTerm {
        ConstraintTerm::Literal(IntegerSet::new_from_range(self.value, self.value))
    }
}

impl ToContraintTerm for ExprInfix<ConstraintTerm> {
    fn to_constraint_term(self) -> ConstraintTerm {
        match self.op {
            InfixOp::Add => self.lhs.apply_term(ConstraintOp::Add, self.rhs),
            InfixOp::Mul => self.lhs.apply_term(ConstraintOp::Mul, self.rhs),
            InfixOp::Sub => todo!("to_constraint_term: InfixOp::Sub"),
            InfixOp::Div => todo!("to_constraint_term: InfixOp::Div"),
            InfixOp::Assign => todo!(),
            InfixOp::CmpNe => self
                .lhs
                .compare_term(BinaryConstraintRelation::Ne, self.rhs),
            InfixOp::CmpEq => self
                .lhs
                .compare_term(BinaryConstraintRelation::Eq, self.rhs),
            InfixOp::CmpGt => self
                .lhs
                .compare_term(BinaryConstraintRelation::Gt, self.rhs),
            InfixOp::CmpLt => self
                .lhs
                .compare_term(BinaryConstraintRelation::Lt, self.rhs),
            InfixOp::CmpGtEq => todo!(),
            InfixOp::CmpLtEq => todo!(),
            InfixOp::BitOr => todo!("bit-wise constraints"),
            InfixOp::BitAnd => todo!("bit-wise constraints"),
            InfixOp::BitXor => todo!("bit-wise constraints"),
            InfixOp::BitLShift => todo!("bit-wise constraints"),
            InfixOp::BitRShift => todo!("bit-wise constraints"),
            InfixOp::LogicalOr => todo!("constraint groups"),
            InfixOp::LogicalAnd => todo!("constraint groups"),
        }
    }
}
