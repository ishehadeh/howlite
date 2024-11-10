use howlite_syntax::ast::{ExprInfix, InfixOp, LiteralInteger};
use preseli::IntegerSet;
use sunstone::{
    multi::DynSet,
    ops::{ArithmeticSet, Bounded, PartialBounded, Union},
};

use super::traits::{ConstraintTerm, ToContraintTerm};

impl ToContraintTerm for LiteralInteger {
    fn to_constraint_term(self) -> ConstraintTerm {
        ConstraintTerm::Literal(IntegerSet::new_from_range(self.value, self.value))
    }
}

// impl ToContraintTerm for ExprInfix<ConstraintTerm> {
//     fn to_constraint_term(self) -> ConstraintTerm {}
// }

pub fn do_infix(op: InfixOp, a: &mut IntegerSet, b: &IntegerSet) {
    match op {
        InfixOp::Add => a.add_all(b),
        InfixOp::Sub => todo!("need a sub_all method for IntegerSet"),
        InfixOp::Div => {
            if let Some(bounds) = b.partial_bounds() {
                if bounds.lo() == bounds.hi() {
                    a.div_scalar(bounds.lo());
                } else {
                    todo!("non-scalar div")
                }
            } else {
                panic!("cannot divide by empty set")
            }
        }
        InfixOp::Mul => a.mul_all(b),
        InfixOp::Assign => todo!(),
        InfixOp::CmpNe => todo!(),
        InfixOp::CmpEq => todo!(),
        InfixOp::CmpGt => todo!(),
        InfixOp::CmpLt => todo!(),
        InfixOp::CmpGtEq => todo!(),
        InfixOp::CmpLtEq => todo!(),
        InfixOp::BitOr => *a = a.clone().union(b.clone()),
        InfixOp::BitAnd => todo!(),
        InfixOp::BitXor => todo!(),
        InfixOp::BitLShift => todo!(),
        InfixOp::BitRShift => todo!(),
        InfixOp::LogicalOr => todo!(),
        InfixOp::LogicalAnd => todo!(),
    }
}
