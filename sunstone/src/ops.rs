use crate::range::Range;

/// Operations that can be performed on sets
pub trait Set<ElementT: Eq>
where
    Self: Sized,
{
    fn includes(&self, element: ElementT) -> bool;
}

pub trait Bounded<ElementT: Ord> {
    fn lo(&self) -> &ElementT;
    fn hi(&self) -> &ElementT;

    fn bounds(&self) -> Range<&ElementT> {
        Range::new(self.lo(), self.hi())
    }
}

pub trait SetMut<ElementT: Eq>
where
    Self: Sized,
{
    type Output;

    fn include(self, element: ElementT) -> Self::Output;
    fn include_mut(&mut self, element: ElementT);

    fn exclude_mut(&mut self, element: &ElementT);
}

pub trait MutSet<ElementT: Eq, ResultT>: IntersectMut<Self> + UnionMut<Self>
where
    Self: Sized,
{
    fn include_mut(&mut self, element: ElementT);
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

pub trait Subset<Rhs = Self> {
    /// A set operation: check if this set is subset of `rhs`
    fn subset_of(self, rhs: Rhs) -> bool;

    /// A set operation: check if this set is subset of `rhs`, but not equal to `rhs`
    fn strict_subset_of(self, rhs: Rhs) -> bool;
}

pub trait RingOps<Rhs = Self> {
    fn ring_add(self, rhs: Rhs) -> Self;
    fn ring_mul(self, rhs: Rhs) -> Self;
}
