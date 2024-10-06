use crate::range::Range;

/// Operations that can be performed on sets
pub trait Set: SetOpIncludes<Self::ElementT> {
    type ElementT: Eq;
}

pub trait Bounded<ElementT: Ord> {
    fn lo(&self) -> &ElementT;
    fn hi(&self) -> &ElementT;

    fn bounds(&self) -> Range<&ElementT> {
        Range::new(self.lo(), self.hi())
    }
}

pub trait PartialBounded<ElementT: Ord> {
    fn partial_lo(&self) -> Option<&ElementT>;
    fn partial_hi(&self) -> Option<&ElementT>;

    fn partial_bounds(&self) -> Option<Range<&ElementT>> {
        match (self.partial_lo(), self.partial_hi()) {
            (Some(lo), Some(hi)) => Some(Range::new(lo, hi)),
            _ => None,
        }
    }
}
pub trait SetOpIncludes<ElementT: Eq> {
    fn includes(&self, element: ElementT) -> bool;
}

pub trait SetOpIncludeExclude<ElementT: Eq>
where
    Self: Sized,
{
    type Output;

    fn include(self, element: ElementT) -> Self::Output;
    fn include_mut(&mut self, element: ElementT);

    fn exclude_mut(&mut self, element: &ElementT);
}

pub trait Intersect<Rhs = Self> {
    type Output;

    /// A set operation: Return a new set containing all elements in both `self` and `rhs`
    fn intersect(self, rhs: Rhs) -> Self::Output;
}

pub trait IntersectMut<Rhs = Self> {
    /// A set operation: remove all elements of `self` not in `rhs`
    fn intersect_mut(&mut self, rhs: Rhs);
}

pub trait Union<Rhs = Self> {
    type Output;

    /// A set operation: Return a new set containing all elements in `self` and all elements in `rhs`
    fn union(self, rhs: Rhs) -> Self::Output;
}

pub trait UnionMut<Rhs = Self> {
    /// A set operation: add all elements of `rhs` not in `rhs`
    fn union_mut(&mut self, rhs: Rhs);
}

pub trait SetSubtract<Rhs = Self> {
    fn set_subtract_mut(&mut self, rhs: Rhs);
}

pub trait Subset<Rhs = Self> {
    /// A set operation: check if this set is subset of `rhs`
    fn subset_of(self, rhs: Rhs) -> bool;

    /// A set operation: check if this set is subset of `rhs`, but not equal to `rhs`
    fn strict_subset_of(self, rhs: Rhs) -> bool;
}

pub trait ArithmeticSet<RhsSetT: Set = Self, RhsElemT = <RhsSetT as Set>::ElementT> {
    fn add_all(&mut self, rhs: RhsSetT);
    fn mul_all(&mut self, rhs: RhsSetT);
    // fn div_all(&mut self, rhs: RhsSetT);
    fn add_scalar(&mut self, rhs: RhsElemT);
    fn mul_scalar(&mut self, rhs: RhsElemT);
    fn div_scalar(&mut self, rhs: RhsElemT);
}
