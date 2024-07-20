use num_bigint::{BigInt, Sign};

use crate::IntegerRange;

#[test]
pub fn test_construction() {
    let r1 = IntegerRange::new(1, 3);
    assert_eq!(r1.lo, BigInt::new(Sign::Plus, vec![1]));
    assert_eq!(r1.hi, BigInt::new(Sign::Plus, vec![3]));

    let r2: IntegerRange = (-5, 3).into();
    assert_eq!(r2.lo, BigInt::new(Sign::Minus, vec![5]));
    assert_eq!(r2.hi, BigInt::new(Sign::Plus, vec![3]));
}

#[test]
pub fn test_intersect() {
    assert_eq!(
        IntegerRange::new(-4, 3).intersect(&IntegerRange::new(2, 5)),
        Some(IntegerRange::new(2, 3))
    );
    assert_eq!(
        IntegerRange::new(-4, 3).intersect(&IntegerRange::new(4, 10)),
        None
    )
}
