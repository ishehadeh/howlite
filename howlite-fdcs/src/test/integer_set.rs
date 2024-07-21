use num_bigint::{BigInt, Sign};

use crate::{IntegerRange, IntegerSet};

#[test]
pub fn test_intersect() {
    let s1: IntegerSet = IntegerSet::new(&[(0, 5), (10, 20)]);
    let s2: IntegerSet = IntegerSet::new(&[(3, 12), (21, 30)]);
    assert_eq!(s1.intersect(&s2), IntegerSet::new(&[(3, 5), (10, 12)]));
}