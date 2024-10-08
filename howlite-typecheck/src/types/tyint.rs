use preseli::IntegerSet;
use sunstone::ops::ArithmeticSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyInt {
    pub values: IntegerSet,
}

impl TyInt {
    pub fn add(&self, rhs: &TyInt) -> TyInt {
        let mut result = self.clone();
        result.values.add_all(&rhs.values);
        result
    }

    pub fn mul(&self, rhs: &TyInt) -> TyInt {
        let mut result = self.clone();
        result.values.mul_all(&rhs.values);
        result
    }
}
