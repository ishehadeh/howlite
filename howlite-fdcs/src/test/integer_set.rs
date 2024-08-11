use num_bigint::BigInt;

use crate::{IntegerRange, IntegerSet};

#[test]
pub fn test_intersect() {
    let s1: IntegerSet = IntegerSet::new(&[(0, 5), (10, 20)]);
    let s2: IntegerSet = IntegerSet::new(&[(3, 12), (21, 30)]);
    assert_eq!(s1.intersect(&s2), IntegerSet::new(&[(3, 5), (10, 12)]));
}

#[test]
pub fn test_exclude_below() {
    // TODO: normalize integer sets, so overlapping ranges like this don't happen
    let mut s1: IntegerSet = IntegerSet::new(&[(0, 5), (3, 20)]);
    s1.exclude_below(&BigInt::from(4));
    assert_eq!(s1, IntegerSet::new(&[(4, 5), (4, 20)]));
}

#[test]
pub fn test_exclude_above() {
    let mut s1: IntegerSet = IntegerSet::new(&[(0, 5), (3, 20)]);
    s1.exclude_above(&BigInt::from(4));
    assert_eq!(s1, IntegerSet::new(&[(0, 4), (3, 4)]));
}

#[test]
pub fn test_exclude_range() {
    // TODO: fix this test when we normalize, a bunch of cases tested here will be normalized out.
    let mut s1: IntegerSet = IntegerSet::new(&[(0, 5), (3, 7), (5, 7), (3, 20)]);
    s1.exclude_range(&(3, 7).into());
    assert_eq!(s1, IntegerSet::new(&[(0, 2), (8, 20)]));

    let mut partial_hi_exact: IntegerSet = IntegerSet::new(&[(0, 10)]);
    partial_hi_exact.exclude_range(&(5, 10).into());
    assert_eq!(partial_hi_exact, IntegerSet::new(&[(0, 4)]));
}

#[test]
pub fn test_exclude() {
    let mut s1: IntegerSet = IntegerSet::new(&[(0, 5), (3, 7), (5, 7), (3, 20)]);
    let s2: IntegerSet = IntegerSet::new(&[(0, 5), (10, 20)]);
    s1.exclude(&s2);
    assert_eq!(s1, IntegerSet::new(&[(6, 9)]));
}
