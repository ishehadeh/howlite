use std::collections::BTreeMap;

use num_prime::BitTest;

use crate::{
    ops::{self, Bounded, IntersectMut, SetOpIncludes, SetSubtract, UnionMut},
    range::Range,
    step_range::StepRange,
    stripeset::StripeSet,
};

#[derive(Clone, PartialEq, Eq)]
/// A BitField stores set elements using a series of bits. A natural number `x` is in the set if the `x`th bit in this series is `1`.
///
/// WIDTH is the number of bits in the set / 64 (e.g. WIDTH = 2 means 128 bit set)
pub struct BitField<const WIDTH: usize = 1> {
    field: [u64; WIDTH],
}

impl<const WIDTH: usize> std::fmt::Debug for BitField<WIDTH> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BitField {{ ")?;
        for (block_no, block) in self.field.iter().copied().enumerate() {
            let mut rem_values = block;
            for i in 0..u64::BITS as usize {
                if rem_values & 1 != 0 {
                    write!(f, " {},", i + block_no * 64)?
                }

                rem_values >>= 1;
            }
        }

        write!(f, " }}")
    }
}

impl<const WIDTH: usize> BitField<WIDTH> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn iter_values_from(
        &self,
        start_block_no: usize,
        start_bit_ind: usize,
    ) -> impl Iterator<Item = usize> + '_ {
        (start_block_no..WIDTH).flat_map(move |block_no| {
            (if start_block_no == block_no {
                start_bit_ind
            } else {
                0
            }..u64::BITS as usize)
                .filter(move |&i| self.field[block_no].bit(i))
                .map(move |bit_ind| bit_ind + block_no * u64::BITS as usize)
        })
    }

    pub fn longest_stripe_from(&self, block_no: usize, index: usize) -> StepRange<usize> {
        let n = block_no * u64::BITS as usize + index;
        let mut series_length = BTreeMap::<usize, usize>::new();
        let mut max_len = 0;
        self.iter_values_from(block_no, index + 1).for_each(|x| {
            let offset = x - n;

            // TODO: optimize
            let keys_iter: Vec<_> = series_length.keys().cloned().collect();
            for size in keys_iter {
                if offset % size == 0 {
                    *(series_length.get_mut(&size).unwrap()) += 1;
                }
                if series_length[&size] < max_len {
                    series_length.remove(&size);
                } else if series_length[&size] > max_len {
                    max_len = series_length[&size]
                }
            }
            series_length.insert(offset, 1);
        });

        match series_length.into_iter().filter(|(_, l)| *l > 2).max() {
            Some((size, length)) => dbg!(StepRange::new(n, size * length + n, size)),
            None => StepRange::new(n, n, 1),
        }
    }

    pub fn add_to_stripe(&self, stripe: &mut StripeSet<usize>) {
        for n in self.iter_values_from(0, 0) {
            if !stripe.includes(n) {
                stripe.add_range(self.longest_stripe_from(n / 64, n % 64));
            }
        }
    }

    /// Returns the highest value in the set, or None if the set is empty
    pub fn hi(&self) -> Option<usize> {
        let mut offset = (WIDTH - 1) * u64::BITS as usize;
        for block in self.field.iter().rev() {
            let last_set_bit = u64::BITS - block.leading_zeros();
            if last_set_bit > 0 {
                return Some(offset + last_set_bit as usize - 1);
            }

            offset -= u64::BITS as usize;
        }

        None
    }

    /// Returns the lowest value in the set, or None if the set is empty
    pub fn lo(&self) -> Option<usize> {
        let mut offset = 0;
        for block in self.field.iter() {
            let first_set_bit = block.trailing_zeros() as u64;
            if first_set_bit < u64::BITS as u64 {
                return Some(offset + first_set_bit as usize);
            }

            offset += u64::BITS as usize;
        }

        None
    }

    pub fn include_between(&mut self, lo: usize, hi: usize) {
        let (lo_block_ind, lo_bit_ind) = Self::elem_addr(lo);
        let (hi_block_ind, hi_bit_ind) = Self::elem_addr(hi);

        // range contains at least one complete block
        if lo_block_ind + 1 < hi_block_ind {
            for i in (lo_block_ind + 1)..hi_block_ind {
                self.field[i] = u64::MAX;
            }
        }

        self.field[lo_block_ind] |= u64::MAX << lo_bit_ind;
        self.field[hi_block_ind] |= !(u64::MAX << hi_bit_ind);
    }

    pub fn includes_step_range(&self, step_range: StepRange<usize>) -> bool {
        // TODO: preformance, we could use a mask if step_range.step() < 64

        let mut i = *step_range.lo();
        while i < *step_range.hi() {
            if !self.includes(i) {
                return false;
            }
            i += step_range.step()
        }

        true
    }

    pub fn includes_range(&self, range: Range<usize>) -> bool {
        let (lo_block_ind, lo_bit_ind) = Self::elem_addr(*range.lo());
        let (hi_block_ind, hi_bit_ind) = Self::elem_addr(*range.hi());

        // range contains at least one complete block
        if lo_block_ind + 1 < hi_block_ind {
            for i in (lo_block_ind + 1)..hi_block_ind {
                if self.field[i] != u64::MAX {
                    return false;
                }
            }
        }

        let lo_block = u64::MAX << lo_bit_ind;
        let hi_block = !(u64::MAX << hi_bit_ind);
        self.field[lo_block_ind] & lo_block == lo_block
            && self.field[hi_block_ind] & !(u64::MAX << hi_bit_ind) == hi_block
    }

    /// Returns the lowest and highest value in the set, or None if the set is empty
    pub fn range(&self) -> Option<Range<usize>> {
        if let Some(lo) = self.lo() {
            if let Some(hi) = self.hi() {
                Some(Range::new(lo, hi))
            } else {
                unreachable!("hi() should only return None if lo() returned None!")
            }
        } else {
            None
        }
    }

    pub fn arith_add_scalar(mut self, n: usize) -> Self {
        #[cfg(debug_assertions)]
        {
            if let Some(hi) = self.hi() {
                assert!(n + hi < WIDTH * 64)
            }
        }
        let value_shift = n % 64;
        //bits that would be shifted off the end
        let overflow_mask: u64 = !(u64::MAX >> value_shift);
        let mut overflow_last = 0;
        let mut overflow_now;
        for x in &mut self.field {
            // save bits that will overflow on this block
            overflow_now = *x & overflow_mask;

            *x <<= value_shift;

            // apply overflow bits from prev iter
            *x |= overflow_last;
            overflow_last = overflow_now >> (u64::BITS as usize - value_shift);
        }

        let block_shift = n / 64;
        if block_shift > 0 {
            let mut i = self.field.len();
            while i > 0 {
                if i <= block_shift {
                    self.field[i - 1] = 0;
                } else {
                    self.field[i - 1] = self.field[i - 1 - block_shift];
                }

                i -= 1;
            }
        }

        self
    }

    pub fn arith_sub_scalar(mut self, n: usize) -> Self {
        #[cfg(debug_assertions)]
        {
            if let Some(lo) = self.lo() {
                assert!(lo > n)
            }
        }

        let value_shift = n % 64;
        //bits that would be shifted off the end
        let overflow_mask: u64 = !(u64::MAX << value_shift);
        let mut overflow_last = 0;
        let mut overflow_now;
        println!("{overflow_mask:064b}");
        for x in self.field.iter_mut().rev() {
            // save bits that will overflow on this block
            overflow_now = *x & overflow_mask;
            println!("*x  = {x:064b}");

            *x >>= value_shift;
            println!("*x' = {x:064b}");

            // apply overflow bits from prev iter
            *x |= overflow_last;
            overflow_last = overflow_now << (u64::BITS as usize - value_shift);
            println!("o   = {overflow_last:064b}");
        }

        let block_shift = n / 64;
        if block_shift > 0 {
            for i in 0..self.field.len() {
                if i < self.field.len() - block_shift {
                    self.field[i] = self.field[i + block_shift];
                } else {
                    self.field[i] = 0;
                }
            }
        }

        self
    }

    pub fn from_slice(slice: &[usize]) -> Self {
        let mut field: [u64; WIDTH] = [0; WIDTH];
        for x in slice {
            let (block, bit) = Self::elem_addr(*x);
            field[block] |= 1 << bit as u64;
        }
        Self { field }
    }

    pub fn arith_add<const SUM_WIDTH: usize>(&self, other: &Self) -> BitField<SUM_WIDTH> {
        let mut sum = BitField::<SUM_WIDTH>::new();
        for i in 0..self.field.len() {
            for j in 0..other.field.len() {
                let block_offset = i + j;
                for bit_offset in 0..u64::BITS {
                    if self.field[i] & (1 << bit_offset) > 0 {
                        let upper_mask = u64::MAX << (u64::BITS - bit_offset);
                        sum.field[block_offset] |= other.field[j] << bit_offset;
                        sum.field[block_offset + 1] |= other.field[j] & upper_mask;
                    }
                }
            }
        }

        sum
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

impl<const WIDTH: usize> ops::Set for BitField<WIDTH> {
    type ElementT = usize;
}

impl<const WIDTH: usize> ops::SetOpIncludes<usize> for BitField<WIDTH> {
    fn includes(&self, n: usize) -> bool {
        let (block_index, bit_index) = BitField::<WIDTH>::elem_addr(n);
        if block_index > WIDTH {
            return false;
        }
        (self.field[block_index] & (1 << bit_index)) != 0
    }
}

impl<const WIDTH: usize> ops::SetOpIncludeExclude<usize> for BitField<WIDTH> {
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

    fn exclude_mut(&mut self, element: &usize) {
        let (block_index, bit_index) = Self::elem_addr(*element);
        if block_index >= WIDTH {
            panic!("input element out of range");
        }
        self.field[block_index] &= !(1 << bit_index);
    }
}

impl<'a, const WIDTH: usize> SetSubtract<&'a Self> for BitField<WIDTH> {
    fn set_subtract_mut(&mut self, rhs: &'a Self) {
        for l in self.field.iter_mut() {
            for &r in rhs.field.iter() {
                *l &= !r
            }
        }
    }
}

impl Copy for BitField<1> {}
impl Copy for BitField<2> {}

#[cfg(test)]
mod test {
    use crate::{
        bitfield::BitField,
        ops::{Intersect, Set, SetOpIncludeExclude, SetOpIncludes, Union},
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

    #[test]
    pub fn add() {
        let mut a: BitField<4> = BitField::default();
        a.include_mut(10);
        a.include_mut(1);
        dbg!(&a);
        assert!(a.includes(1));
        assert!(a.includes(10));

        let a = a.arith_add_scalar(100);
        dbg!(&a);
        assert!(a.includes(101));
        assert!(a.includes(110));
        assert!(!a.includes(10));

        let a = a.arith_add_scalar(63);
        dbg!(&a);
        assert!(a.includes(164));
        assert!(a.includes(173));
        assert!(!a.includes(101));
    }

    #[test]
    pub fn sub() {
        let mut a: BitField<4> = BitField::default();
        a.include_mut(200);
        a.include_mut(150);
        dbg!(&a);
        assert!(a.includes(200));
        assert!(a.includes(150));

        let b = a.clone().arith_sub_scalar(43);
        dbg!(&b);
        assert!(b.includes(157));
        assert!(b.includes(107));
        assert!(!b.includes(200));

        let a = a.arith_sub_scalar(100);
        dbg!(&a);
        assert!(a.includes(100));
        assert!(a.includes(50));
        assert!(!a.includes(150));
    }

    #[test]
    pub fn add_set() {
        let mut a: BitField<4> = BitField::default();
        a.include_mut(200);
        a.include_mut(150);
        let mut b: BitField<4> = BitField::default();
        b.include_mut(100);
        b.include_mut(23);
        let sum = a.arith_add::<8>(&b);
        dbg!(&sum);
        assert!(sum.includes(300));
        assert!(sum.includes(173));
        assert!(sum.includes(223));
        assert!(sum.includes(250));
    }
}
