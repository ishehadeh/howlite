use std::collections::BTreeMap;

use num::ToPrimitive;
use tracing::{debug, instrument, trace, warn, Level};

use crate::{
    ops::{self, ArithmeticSet, Bounded, IntersectMut, SetOpIncludes, SetSubtract, UnionMut},
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
        for val in self.iter_values_from(0, 0) {
            write!(f, " {},", val)?
        }

        write!(f, " }}")
    }
}

impl<const BLOCKS: usize> BitField<BLOCKS> {
    pub fn new() -> Self {
        Default::default()
    }

    /// How many values can be kept in each per block
    /// total number of values that may be store in the bit field
    /// is equal to Self::block_width() * Self::BLOCKS
    pub const fn block_width() -> usize {
        u64::BITS as usize
    }

    pub fn iter_values_from(
        &self,
        start_block_no: usize,
        start_bit_ind: usize,
    ) -> impl Iterator<Item = usize> + '_ {
        (start_block_no..BLOCKS).flat_map(move |block_no| {
            (if start_block_no == block_no {
                start_bit_ind
            } else {
                0
            }..Self::block_width())
                .filter(move |&i| (self.field[block_no] & (1u64 << i)) != 0)
                .map(move |bit_ind| bit_ind + block_no * Self::block_width())
        })
    }

    pub fn longest_stripe_from(&self, block_no: usize, index: usize) -> StepRange<usize> {
        let n = block_no * Self::block_width() + index;
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
            Some((size, length)) => StepRange::new(n, size * length + n, size),
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
    pub fn iter_step_ranges(&self) -> impl Iterator<Item = StepRange<usize>> + '_ {
        self.iter_values_from(0, 0)
            .map(|n| self.longest_stripe_from(n / 64, n % 64))
    }

    /// Returns the highest value in the set, or None if the set is empty
    pub fn hi(&self) -> Option<usize> {
        let mut offset = (BLOCKS - 1) * Self::block_width();
        for block in self.field.iter().rev() {
            let last_set_bit = u64::BITS - block.leading_zeros();
            if last_set_bit > 0 {
                return Some(offset + last_set_bit as usize - 1);
            }

            offset -= Self::block_width();
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

            offset += Self::block_width();
        }

        None
    }

    // include all elements from lo (inclusive) up to hi (exclusive)
    pub fn include_between(&mut self, lo: usize, hi: usize) {
        let (lo_block_ind, lo_bit_ind) = Self::elem_addr(lo);
        let (hi_block_ind, hi_bit_ind) = Self::elem_addr(hi);

        // range contains at least one complete block
        if lo_block_ind + 1 < hi_block_ind {
            for i in (lo_block_ind + 1)..hi_block_ind {
                self.field[i] = u64::MAX;
            }
        }

        if lo_block_ind < hi_block_ind {
            self.field[lo_block_ind] |= u64::MAX << lo_bit_ind;
            self.field[hi_block_ind] |= !(u64::MAX << hi_bit_ind);
        } else {
            self.field[lo_block_ind] |= (u64::MAX << lo_bit_ind) & !(u64::MAX << hi_bit_ind);
        }
    }

    // remove all elements from lo (inclusive) up to hi (exclusive)
    pub fn exclude_range(&mut self, lo: usize, hi: usize) {
        // FIXME: there's some bug here if lo and hi are on a small interval
        let (lo_block_ind, lo_bit_ind) = Self::elem_addr(lo);
        let (hi_block_ind, hi_bit_ind) = Self::elem_addr(hi);

        // range contains at least one complete block
        if lo_block_ind + 1 < hi_block_ind {
            for i in (lo_block_ind + 1)..hi_block_ind {
                self.field[i] = 0;
            }
        }

        if lo_block_ind < hi_block_ind {
            self.field[lo_block_ind] &= !(u64::MAX << lo_bit_ind);
            self.field[hi_block_ind] &= u64::MAX << hi_bit_ind;
        } else {
            self.field[lo_block_ind] &= !((u64::MAX << lo_bit_ind) & (u64::MAX << hi_bit_ind));
        }
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

    pub fn set_subtract_step_range(&mut self, step_range: StepRange<usize>) {
        // TODO: preformance, we could use a mask if step_range.step() < 64

        let mut i = *step_range.lo();
        while i < *step_range.hi() && i < BLOCKS * Self::block_width() {
            let (block, bit) = Self::elem_addr(i);
            self.field[block] &= !(1 << bit);
            i += step_range.step();
        }
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

    /// check if all elements of this set + offset are divisble by n.
    pub fn is_divisible_by_with_offset(&self, n: usize, offset: isize) -> bool {
        // we'll approach this by trying to repeatedly apply a mask to our field.
        // see which bits need to be set:
        // in every `n_block_offset` blocks, `n_bit_offset` bit must be set
        let (n_block_offset, n_bit_offset) = Self::elem_addr(n);

        // if possible, try creating a mask, so we can test multiple values at once
        // for example, if n = 3 then mask could be 0b001001001001[...]
        // this doesn't always round perfectly to `Self::block_width` though,
        // so store how many bits we should advance with `mask_bits`
        let (mask, mask_bits) = if n_block_offset == 0 {
            let mut mask: u64 = 0;
            let mut off = n_bit_offset;
            while off < Self::block_width() {
                mask |= 1 << off;
                off += n_bit_offset;
            }
            (mask, off - n_bit_offset)
        } else {
            (1 << n_bit_offset, n_bit_offset)
        };

        let mut field_block_offset = 0;

        // untested bits begining from field_block_offset, this can span multiple blocks
        let mut field_bit_offset = n_block_offset * Self::block_width();
        match offset.cmp(&0) {
            std::cmp::Ordering::Less => {
                let offset_abs = offset.abs().to_usize().unwrap();
                field_bit_offset += (offset_abs) % n;
            }
            std::cmp::Ordering::Equal => (),
            std::cmp::Ordering::Greater => {
                field_bit_offset += n - offset.to_usize().unwrap() % n;
            }
        }
        while field_block_offset < BLOCKS {
            if field_bit_offset >= Self::block_width() {
                if self.field[field_block_offset] != 0 {
                    return false;
                }
                field_bit_offset -= Self::block_width();
                field_block_offset += 1;
            } else {
                let adjusted_mask = !(mask << field_bit_offset);
                // use xor to get all the bits that don't divide n
                let unexpected_bits = self.field[field_block_offset] & adjusted_mask;
                // again, ignore lower bits we already checked
                if unexpected_bits >> field_bit_offset != 0 {
                    return false;
                }
                field_bit_offset =
                    (mask_bits + field_bit_offset) % 64 + n_block_offset * Self::block_width();
                field_block_offset += 1;
            }
        }

        true
    }

    pub fn arith_add_scalar(mut self, n: usize) -> Self {
        self.arith_add_scalar_mut(n);
        self
    }

    #[instrument(level=Level::TRACE)]
    pub fn arith_add_scalar_mut(&mut self, n: usize) {
        #[cfg(debug_assertions)]
        {
            if let Some(hi) = self.hi() {
                assert!(n + hi < BLOCKS * 64)
            }
        }
        let value_shift = n % 64;
        if value_shift > 0 {
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
                overflow_last = overflow_now >> (Self::block_width() - value_shift);
            }
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
    }

    pub fn arith_sub_scalar(mut self, n: usize) -> Self {
        self.arith_sub_scalar_mut(n);
        self
    }

    pub fn reverse(&mut self) {
        self.field.reverse();
        for i in 0..BLOCKS {
            self.field[i] = self.field[i].reverse_bits();
        }
    }

    #[instrument(level=Level::TRACE)]
    pub fn arith_sub_scalar_mut(&mut self, n: usize) {
        if n == 0 {
            warn!("subtraction by zero on underlying bitfield set, this usually suggests an issue somewhere else...");
            return;
        }

        #[cfg(debug_assertions)]
        {
            if let Some(lo) = self.lo() {
                assert!(lo >= n)
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
            overflow_last = overflow_now << (Self::block_width() - value_shift);
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
    }

    pub fn from_slice(slice: &[usize]) -> Self {
        let mut field: [u64; BLOCKS] = [0; BLOCKS];
        for x in slice {
            let (block, bit) = Self::elem_addr(*x);
            field[block] |= 1 << bit as u64;
        }
        Self { field }
    }

    pub fn arith_add<const SUM_WIDTH: usize>(&self, other: &Self) -> BitField<SUM_WIDTH> {
        let mut sum = BitField::<SUM_WIDTH>::new();
        for i in 0..self.field.len() {
            if self.field[i] == 0 {
                continue;
            }

            for j in 0..other.field.len() {
                if other.field[j] == 0 {
                    continue;
                }

                let block_offset = i + j;

                for bit_offset in 0..Self::block_width() {
                    if self.field[i] & (1 << bit_offset) > 0 {
                        // offset the entire other block by this bit & block index in self
                        sum.field[block_offset] |= other.field[j] << bit_offset;

                        // if necessary overflow to the next block in the sum
                        if bit_offset > 0 {
                            let upper_mask = u64::MAX << (Self::block_width() - bit_offset);
                            sum.field[block_offset + 1] |= other.field[j] & upper_mask;
                        }
                    }
                }
            }
        }

        sum
    }

    pub fn arith_sub<const SUM_WIDTH: usize>(&self, other: &Self) -> BitField<SUM_WIDTH> {
        let mut sum = BitField::<SUM_WIDTH>::new();
        for i in 0..self.field.len() {
            debug!(lhs_i = ?i, lhs_block = format!("{:064b}", self.field[i]));

            if self.field[i] == 0 {
                continue;
            }

            for j in 0..other.field.len() {
                debug!(rhs_i = ?j, rhs_block = format!("{:064b}", other.field[j]));

                if other.field[j] == 0 {
                    continue;
                }

                let block_offset = i - j;

                for bit_offset in 0..Self::block_width() {
                    if other.field[j] & (1 << bit_offset) > 0 {
                        debug!(bit = ?bit_offset);
                        // offset the entire other block by this bit & block index in self
                        sum.field[block_offset] |= self.field[i] >> bit_offset;

                        // if necessary overflow to the next block in the sum
                        if bit_offset > 0 {
                            let lower_mask = u64::MAX >> (Self::block_width() - bit_offset);
                            debug!("lower_mask = {:064b}", lower_mask);

                            sum.field[block_offset - 1] |=
                                (self.field[i] & lower_mask) << (Self::block_width() - bit_offset);
                        }

                        debug!(
                            blocks = ?(block_offset, block_offset - 1),
                            hi = format!("{:064b}", sum.field[block_offset]),
                            lo = format!("{:064b}", sum.field[block_offset - 1]),
                            "updated sum"
                        );
                    }
                }
            }
        }

        sum
    }

    const fn elem_addr(el: usize) -> (usize, usize) {
        let bit_index = el % Self::block_width();
        let block_index = el / Self::block_width();
        (block_index, bit_index)
    }

    fn get_range(&self, start: usize, end: usize) -> u64 {
        assert!(end >= start);
        assert!(end - start <= 63);
        let (block, bit) = Self::elem_addr(start);
        let (block_end, bit_end) = Self::elem_addr(end);
        if block == block_end {
            let end_offset = Self::block_width() - 1 - bit_end;
            (self.field[block] << end_offset) >> (bit + end_offset)
        } else {
            let lower = self.field[block] >> bit;
            let upper = self.field[block_end] << (Self::block_width() - 1 - bit_end);

            lower | upper
        }
    }

    /// return a new set: { x - n : x in self, x >= n }
    pub fn take_ge(&mut self, n: usize) -> Self {
        let mut new = Self::new();
        let (first_block, bit_offset) = Self::elem_addr(n);
        for i in first_block..BLOCKS {
            let start = i * Self::block_width() + bit_offset;
            let end = (start + Self::block_width() - 1).min(Self::block_width() * BLOCKS - 1);
            new.field[i - first_block] = self.get_range(start, end);
        }
        self.exclude_range(n, Self::block_width() * BLOCKS - 1);
        new
    }

    pub fn set_subtract_mut_with_offset(&mut self, rhs: &Self, offset: isize) {
        let offset_shift_lhs = if offset > 0 {
            Self::elem_addr(offset as usize)
        } else {
            (0, 0)
        };
        let offset_shift_rhs = if offset < 0 {
            Self::elem_addr(-offset as usize)
        } else {
            (0, 0)
        };
        for (i, l) in self.field.iter_mut().enumerate() {
            if i < offset_shift_lhs.0 {
                continue;
            }
            for (j, &r) in rhs.field.iter().enumerate() {
                if j < offset_shift_rhs.0 {
                    continue;
                }
                if i == offset_shift_lhs.0 {
                    *l &= !r << offset_shift_lhs.1
                } else if j == offset_shift_rhs.0 {
                    *l &= !r >> offset_shift_rhs.1
                } else {
                    *l &= !r
                }
            }
        }
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

impl<const WIDTH: usize> ArithmeticSet<&Self, usize> for BitField<WIDTH> {
    fn add_all(&mut self, rhs: &Self) {
        self.arith_add::<WIDTH>(rhs);
    }

    fn mul_all(&mut self, _rhs: &Self) {
        todo!()
    }

    fn add_scalar(&mut self, rhs: usize) {
        self.arith_add_scalar_mut(rhs);
    }

    fn mul_scalar(&mut self, _rhs: usize) {
        todo!()
    }

    fn div_scalar(&mut self, _rhs: usize) {
        todo!("div_scalar");
    }

    #[instrument]
    fn mod_scalar(&mut self, rhs: usize) {
        // divide the set into n slices:
        //  M_1 = [rhs, 2*rhs)
        //  M_2 = [2*rhs, 3*rhs)
        //  ...
        //  M_n = [n*rhs, (n+1)*rhs]
        // and union each of these sets with [0, rhs):
        //     [0, rhs) | (M_1 - rhs) | (M_2 - 2 * rhs) | ... | (M_n - n * rhs)

        let mut split_point = rhs;

        while split_point < WIDTH * Self::block_width() {
            let mut lo_block_ind = 0;
            let end: usize = (split_point + rhs - 1).min(Self::block_width() * WIDTH - 1);

            let len = end - split_point;
            let num_blocks = len / Self::block_width();
            for i in 0..num_blocks {
                self.field[lo_block_ind] |= self.get_range(
                    split_point + i * Self::block_width(),
                    split_point + (i + 1) * Self::block_width() - 1,
                );
                lo_block_ind += 1;
            }
            self.field[lo_block_ind] |=
                self.get_range(split_point + num_blocks * Self::block_width(), end);
            split_point += rhs;
        }
        self.exclude_range(rhs, Self::block_width() * WIDTH - 1);
    }

    fn sub_all(&mut self, _rhs: &Self) {
        todo!()
    }

    fn div_all(&mut self, _rhs: &Self) {
        todo!()
    }
}

impl Copy for BitField<1> {}
impl Copy for BitField<2> {}

#[cfg(test)]
mod test {
    use tracing_test::traced_test;

    use crate::{
        bitfield::BitField,
        ops::{ArithmeticSet, Intersect, SetOpIncludeExclude, SetOpIncludes, Union},
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

    #[traced_test]
    #[test]
    pub fn arith_sub_set() {
        let mut a: BitField<4> = BitField::default();
        a.include_mut(200);
        a.include_mut(150);
        let mut b: BitField<4> = BitField::default();
        b.include_mut(100);
        b.include_mut(23);
        let sum = a.arith_sub::<8>(&b);
        dbg!(&sum);
        assert!(sum.includes(100));
        assert!(sum.includes(177));
        assert!(sum.includes(127));
        assert!(sum.includes(50));
    }

    #[test]
    pub fn test_divides() {
        let mut a: BitField<4> = BitField::default();
        a.include_mut(5);
        assert!(a.is_divisible_by_with_offset(5, 0));

        a.include_mut(10);
        assert!(a.is_divisible_by_with_offset(5, 0));

        a.include_mut(75);
        assert!(a.is_divisible_by_with_offset(5, 0));

        a.include_mut(150);
        assert!(a.is_divisible_by_with_offset(5, 0));

        a.include_mut(200);
        assert!(a.is_divisible_by_with_offset(5, 0));

        let a = a.arith_add_scalar(3);

        assert!(!a.is_divisible_by_with_offset(5, 0));
        assert!(a.is_divisible_by_with_offset(5, -3));
        assert!(a.is_divisible_by_with_offset(5, 2));
    }
    #[test]
    pub fn test_divides_large() {
        let mut a: BitField<4> = BitField::default();
        a.include_mut(240);
        assert!(a.is_divisible_by_with_offset(120, 0));

        let mut b: BitField<4> = BitField::default();
        b.include_mut(253);
        assert!(b.is_divisible_by_with_offset(253, 0));
        assert!(b.is_divisible_by_with_offset(248, 243));
    }

    #[test]
    pub fn test_get_range() {
        let mut a: BitField<4> = BitField::default();
        for x in 0..(64 * 4) {
            if x % 5 == 0 || x % 7 == 0 {
                a.include_mut(x);
            }
        }
        let val = a.get_range(10, 73);
        let expect = (1u64 << (10 - 10))
            | (1u64 << (15 - 10))
            | (1u64 << (20 - 10))
            | (1u64 << (25 - 10))
            | (1u64 << (30 - 10))
            | (1u64 << (35 - 10))
            | (1u64 << (40 - 10))
            | (1u64 << (45 - 10))
            | (1u64 << (50 - 10))
            | (1u64 << (55 - 10))
            | (1u64 << (60 - 10))
            | (1u64 << (65 - 10))
            | (1u64 << (70 - 10))
            | (1u64 << (14 - 10))
            | (1u64 << (21 - 10))
            | (1u64 << (28 - 10))
            | (1u64 << (35 - 10))
            | (1u64 << (42 - 10))
            | (1u64 << (49 - 10))
            | (1u64 << (56 - 10))
            | (1u64 << (63 - 10))
            | (1u64 << (70 - 10));
        assert_eq!(
            val, expect,
            "\ngot: {:#064b}
exp: {:#064b}",
            val, expect
        );

        assert_eq!(a.get_range(10, 15), 0b110001)
    }

    #[test]
    pub fn test_mod() {
        let mut a: BitField<4> = BitField::default();
        a.include_mut(8);
        a.mod_scalar(5);
        assert_eq!(a, BitField::default().include(3));
    }

    #[test]
    pub fn include_between() {
        let mut a: BitField<4> = BitField::default();
        a.include_between(0, 27);
        assert_eq!(
            a.iter_values_from(0, 0).collect::<Vec<_>>(),
            (0usize..=26usize).collect::<Vec<_>>()
        );
    }
}
