use crate::{
    ops::{self, IntersectMut, UnionMut},
    SetElement,
};

#[derive(Clone, Debug, PartialEq, Eq)]
/// A BitField stores set elements using a series of bits. A natural number `x` is in the set if the `x`th bit in this series is `1`.
///
/// WIDTH is the number of bits in the set / 64 (e.g. WIDTH = 2 means 128 bit set)
pub struct BitField<const WIDTH: usize = 1> {
    field: [u64; WIDTH],
}

impl<const WIDTH: usize> BitField<WIDTH> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from_slice(slice: &[usize]) -> Self {
        let mut field: [u64; WIDTH] = [0; WIDTH];
        for x in slice {
            let (block, bit) = Self::elem_addr(*x);
            field[block] |= 1 << bit as u64;
        }
        Self { field }
    }

    const fn elem_addr(el: usize) -> (usize, usize) {
        let bit_index = el % u64::BITS as usize;
        let block_index = el / u64::BITS as usize;
        (block_index, bit_index)
    }
}

impl<const WIDTH: usize> Default for BitField<WIDTH> {
    fn default() -> Self {
        Self { field: [0; WIDTH] }
    }
}

impl<'a, const WIDTH: usize> ops::Subset for &'a BitField<WIDTH> {
    fn subset_of(self, rhs: Self) -> bool {
        for i in 0..WIDTH {
            if !self.field[i] & rhs.field[i] != 0 {
                return false;
            }
        }

        true
    }

    fn strict_subset_of(self, rhs: Self) -> bool {
        let mut eq = true;
        for i in 0..WIDTH {
            if !self.field[i] & rhs.field[i] != 0 {
                return false;
            }

            eq = eq && (self.field[i] == rhs.field[i])
        }

        !eq
    }
}

impl<'a, const WIDTH: usize> ops::IntersectMut<&'a BitField<WIDTH>> for BitField<WIDTH> {
    fn intersect_mut(&mut self, rhs: &'a Self) {
        for i in 0..WIDTH {
            self.field[i] &= rhs.field[i];
        }
    }
}

impl<const WIDTH: usize> ops::IntersectMut<BitField<WIDTH>> for BitField<WIDTH> {
    fn intersect_mut(&mut self, rhs: Self) {
        for i in 0..WIDTH {
            self.field[i] &= rhs.field[i];
        }
    }
}

impl<'a, const WIDTH: usize> ops::UnionMut<&'a BitField<WIDTH>> for BitField<WIDTH> {
    fn union_mut(&mut self, rhs: &'a Self) {
        for i in 0..WIDTH {
            self.field[i] |= rhs.field[i];
        }
    }
}

impl<const WIDTH: usize> ops::UnionMut<BitField<WIDTH>> for BitField<WIDTH> {
    fn union_mut(&mut self, rhs: Self) {
        for i in 0..WIDTH {
            self.field[i] |= rhs.field[i];
        }
    }
}

impl<const WIDTH: usize> ops::Intersect<BitField<WIDTH>> for BitField<WIDTH> {
    type Output = Self;

    fn intersect(mut self, rhs: Self) -> Self {
        self.intersect_mut(rhs);
        self
    }
}

impl<'a, const WIDTH: usize> ops::Intersect<&'a BitField<WIDTH>> for BitField<WIDTH> {
    type Output = BitField<WIDTH>;

    fn intersect(mut self, rhs: &'a BitField<WIDTH>) -> BitField<WIDTH> {
        self.intersect_mut(rhs);
        self
    }
}

impl<const WIDTH: usize> ops::Union<BitField<WIDTH>> for BitField<WIDTH> {
    type Output = Self;

    fn union(mut self, rhs: Self) -> Self {
        self.union_mut(rhs);
        self
    }
}

impl<'a, const WIDTH: usize> ops::Union<BitField<WIDTH>> for &'a BitField<WIDTH> {
    type Output = BitField<WIDTH>;

    fn union(self, rhs: BitField<WIDTH>) -> BitField<WIDTH> {
        let mut new = self.clone();
        new.union_mut(rhs);
        new
    }
}

impl<'a, const WIDTH: usize> ops::Union<&'a BitField<WIDTH>> for &'a BitField<WIDTH> {
    type Output = BitField<WIDTH>;

    fn union(self, rhs: &'a BitField<WIDTH>) -> BitField<WIDTH> {
        let mut new = self.clone();
        new.union_mut(rhs);
        new
    }
}

impl<const WIDTH: usize> ops::Set<usize> for BitField<WIDTH> {
    fn includes(&self, n: usize) -> bool {
        let (block_index, bit_index) = BitField::<WIDTH>::elem_addr(n);
        if block_index > WIDTH {
            return false;
        }
        (self.field[block_index] & (1 << bit_index)) != 0
    }
}

impl<const WIDTH: usize> ops::SetMut<usize> for BitField<WIDTH> {
    type Output = BitField<WIDTH>;

    fn include(mut self, element: usize) -> Self::Output {
        self.include_mut(element);
        self
    }

    fn include_mut(&mut self, n: usize) {
        let (block_index, bit_index) = Self::elem_addr(n);
        if block_index >= WIDTH {
            panic!("input element out of range");
        }
        self.field[block_index] |= 1 << bit_index;
    }
}

impl Copy for BitField<1> {}
impl Copy for BitField<2> {}

#[cfg(test)]
mod test {
    use crate::{
        bitfield::BitField,
        ops::{Intersect, Set, SetMut, Union},
    };

    #[test]
    pub fn bitfield() {
        let mut a: BitField<4> = BitField::default();
        a.include_mut(255);
        a.include_mut(1);

        assert!(a.includes(1));
        assert!(a.includes(255));
        assert!(!a.includes(10000));

        let mut b: BitField<4> = BitField::default();
        b.include_mut(200);
        b.include_mut(1);

        assert_eq!(BitField::default().include(1), a.clone().intersect(&b));
        assert_eq!(
            BitField::default().include(1).include(200).include(255),
            a.union(b)
        )
    }
}
