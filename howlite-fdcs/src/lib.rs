use std::collections::BTreeSet;

use num_bigint::BigInt;

#[cfg(test)]
mod test;

mod integer_range;
pub use integer_range::IntegerRange;

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct IntegerSet {
    ranges: BTreeSet<IntegerRange>
}

impl IntegerSet {
    pub fn new<RangeT: Into<BigInt> + Clone>(ranges: &[(RangeT, RangeT)]) -> IntegerSet {
        IntegerSet {
            ranges: ranges.iter().map(IntegerRange::from).collect()
        }
    }

    pub fn intersect(&self, other: &IntegerSet) -> IntegerSet {
        let mut intersect = IntegerSet::default();
        for r0 in other.ranges.iter() {
            for r1 in self.ranges.iter() {
                if let Some(r_intersect) = r0.intersect(&r1) { 
                    intersect.ranges.insert(r_intersect);
                }
            }
        }

        intersect
    }
}


pub struct Variable {
    id: usize
}

/// An Domain Constraint is an expression in the form X in I, where I is non-empty set of integers
pub struct DomainConstraint {
    variable: Variable,
    /// I, a non-empty set of integers
    domain: IntegerSet
}

/// A Store is a non-empty set of [DomainConstraint]s
pub struct Store {
    constraints: DomainConstraint
}


pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
