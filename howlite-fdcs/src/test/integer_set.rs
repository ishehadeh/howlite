use num_bigint::BigInt;

use crate::IntegerSet;

#[test]
pub fn test_intersect() {
    let s1: IntegerSet = IntegerSet::new_from_tuples(&[(0, 5), (10, 20)]);
    let s2: IntegerSet = IntegerSet::new_from_tuples(&[(3, 12), (21, 30)]);
    assert_eq!(
        s1.intersect(&s2),
        IntegerSet::new_from_tuples(&[(3, 5), (10, 12)])
    );
}

#[test]
pub fn test_exclude_below() {
    let mut s1: IntegerSet = IntegerSet::new_from_tuples(&[(0, 2), (5, 20)]);
    s1.exclude_below(&BigInt::from(4));
    assert_eq!(s1, IntegerSet::new_from_tuples(&[(4, 20)]));

    let mut s2: IntegerSet = IntegerSet::new_from_tuples(&[(0, 10), (15, 20)]);
    s2.exclude_below(&BigInt::from(17));
    assert_eq!(s2, IntegerSet::new_from_tuples(&[(17, 20)]));

    let mut s3: IntegerSet = IntegerSet::new_from_tuples(&[(0, 200)]);
    s3.exclude_below(&BigInt::from(100));
    assert_eq!(s3, IntegerSet::new_from_tuples(&[(100, 200)]));
}

#[test]
pub fn test_exclude_above() {
    let mut s1: IntegerSet = IntegerSet::new_from_tuples(&[(0, 2), (5, 20)]);
    s1.exclude_above(&BigInt::from(4));
    assert_eq!(s1, IntegerSet::new_from_tuples(&[(0, 4)]));

    let mut s2: IntegerSet = IntegerSet::new_from_tuples(&[(0, 2), (5, 20)]);
    s2.exclude_above(&BigInt::from(7));
    assert_eq!(s2, IntegerSet::new_from_tuples(&[(0, 2), (5, 7)]));
}

#[test]
pub fn test_normalize() {
    assert_eq!(
        IntegerSet::new_from_tuples(&[(0, 20)]),
        IntegerSet::new_from_tuples(&[(0, 5), (3, 7), (5, 7), (3, 20)])
    );
}

#[test]
pub fn test_exclude_range() {
    // TODO: fix this test when we normalize, a bunch of cases tested here will be normalized out.
    //  ^^ this is relevant now (2024-08-18)
    let mut s1: IntegerSet = IntegerSet::new_from_tuples(&[(0, 20)]);
    s1.exclude_range(&(3, 7).into());
    assert_eq!(s1, IntegerSet::new_from_tuples(&[(0, 2), (8, 20)]));

    let mut partial_hi_exact: IntegerSet = IntegerSet::new_from_tuples(&[(0, 10)]);
    partial_hi_exact.exclude_range(&(5, 10).into());
    assert_eq!(partial_hi_exact, IntegerSet::new_from_tuples(&[(0, 4)]));
}

#[test]
pub fn test_exclude() {
    let mut s1: IntegerSet = IntegerSet::new_from_tuples(&[(0, 5), (3, 7), (5, 7), (3, 20)]);
    let s2: IntegerSet = IntegerSet::new_from_tuples(&[(0, 5), (10, 20)]);
    s1.exclude(&s2);
    assert_eq!(s1, IntegerSet::new_from_tuples(&[(6, 9)]));
}
