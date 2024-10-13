use preseli::IntegerSet;
use sunstone::ops::{ArithmeticSet, Bounded, PartialBounded, Union};

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

pub struct StorageClass {
    pub is_signed: bool,

    // number of bits used to store this integer
    pub bits: usize,
}

impl StorageClass {
    pub fn normalize(&self, set: &mut IntegerSet) {
        // sanity checks, if this the follow isn't true we'll have problems...
        assert!(self.bits <= 64);
        assert!(self.min_value() <= 0); // this is implied by the follow, but just in case...
        assert!(!self.is_signed || self.max_value() == (-self.min_value()) as u64 - 1);
        assert!(self.is_signed || self.min_value() == 0);

        // if the set has no bounds then it is empty, so there's nothing to do
        if let Some(range) = set.partial_bounds().map(|r| r.clone_endpoints()) {
            // TODO: don't hard i128 here, instead use preseli::Scalar, or better yet make it generic
            let big_min = self.min_value() as i128;
            let big_max = self.max_value() as i128;

            let mut too_lo = set.take_below(&big_min);
            let mut too_hi = set.take_above(&big_max);

            if let Some(too_lo_range) = too_lo.partial_bounds().map(|a| a.clone_endpoints()) {
                too_lo.add_scalar(too_lo_range.lo());
                too_lo.mod_scalar(&(-big_min + 1));
                too_lo.add_scalar(&(range.hi() - big_min));
                set.union(too_lo);
            }

            if !too_hi.is_empty() {
                // the opposite, entire range is too large
                too_hi.mod_scalar(&(big_max + 1));
                if self.min_value() != 0 {
                    too_hi.add_scalar(&big_min);
                }
                set.union(too_hi);
            }
        }
    }

    fn max_if_unsigned(&self) -> u64 {
        assert!(self.bits <= 64);
        u64::MAX >> (64 - self.bits)
    }

    pub fn max_value(&self) -> u64 {
        if self.is_signed {
            self.max_if_unsigned() >> 1
        } else {
            self.max_if_unsigned()
        }
    }

    pub fn min_value(&self) -> i64 {
        if self.is_signed {
            -(self.max_value() as i64) - 1
        } else {
            0
        }
    }
}

#[test]
pub fn min_and_max_values() {
    macro_rules! size_map_entry {
        ($t:ident) => {
            (
                $t::MIN as i64,
                $t::MAX as u64,
                StorageClass {
                    bits: $t::BITS as usize,
                    #[allow(unused_comparisons)]
                    is_signed: $t::MIN < 0,
                },
            )
        };
    }
    let size_map = [
        size_map_entry!(i8),
        size_map_entry!(u8),
        size_map_entry!(i16),
        size_map_entry!(u16),
        size_map_entry!(i32),
        size_map_entry!(u32),
        size_map_entry!(i64),
        size_map_entry!(u64),
    ];
    for (expected_lo, expected_hi, class) in size_map {
        assert_eq!(expected_lo, class.min_value());
        assert_eq!(expected_hi, class.max_value());
    }
}

#[test]
pub fn normalize_storage_class_u8() {
    use sunstone::ops::SetOpIncludes;
    use sunstone::range::Range;

    let sclass_u8 = StorageClass {
        bits: 8,
        is_signed: false,
    };

    let range = Range::new(240, 300);
    let mut set = IntegerSet::new_from_range(*range.lo() as i128, *range.hi() as i128);
    sclass_u8.normalize(&mut set);
    assert_eq!(set, IntegerSet::new_from_tuples(&[(240, 255), (0, 45)]));

    // replicate this with normal ints.
    // like Howlite, rust guarentees wrapping on overflow (I think)
    let mut a: u8 = 240;
    for _ in *range.lo()..*range.hi() {
        assert!(
            set.includes(a as i128),
            "set does not include {}! set: {:?}",
            a,
            set
        );

        // wrapping methods disable overflow checks
        a = a.wrapping_add(1);
    }
}

#[test]
pub fn normalize_storage_class_i8() {
    use sunstone::ops::SetOpIncludes;
    use sunstone::range::Range;

    let sclass_i8 = StorageClass {
        bits: 8,
        is_signed: true,
    };

    let range = Range::new(110, 155);
    let mut set = IntegerSet::new_from_range(*range.lo() as i128, *range.hi() as i128);
    sclass_i8.normalize(&mut set);
    assert_eq!(
        set,
        IntegerSet::new_from_tuples(&[(110, 127), (-128, -128 + (155 - 128))])
    );

    // replicate this with normal ints.
    // like Howlite, rust guarentees wrapping on overflow (I think)
    let mut a: i8 = 110;
    for _ in *range.lo()..*range.hi() {
        assert!(
            set.includes(a as i128),
            "set does not include {}! set: {:?}",
            a,
            set
        );

        // wrapping methods disable overflow checks
        a = a.wrapping_add(1);
    }
}
