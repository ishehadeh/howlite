use howlite_syntax::ast::{ExprInfix, LiteralInteger};
use preseli::IntegerSet;

use super::{traits::ToContraintTerm, ConstraintTerm};

impl ToContraintTerm for LiteralInteger {
    fn to_constraint_term(self) -> ConstraintTerm {
        ConstraintTerm::Literal(IntegerSet::new_from_range(self.value, self.value))
    }
}

impl ToContraintTerm for ExprInfix<ConstraintTerm> {
    fn to_constraint_term(self) -> ConstraintTerm {
        match (self.lhs, self.op, self.rhs) {
            _ => todo!(),
        }
    }
}
