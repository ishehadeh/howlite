/// Operations that can be performed on sets
pub trait Set<ElementT: Eq>:
    Intersect<Self, Output = Self> + Union<Self, Output = Self> + Subset<Self>
where
    Self: Sized,
{
    fn includes(&self, element: ElementT) -> bool;
}

pub trait Intersect<Rhs = Self> {
    type Output;

    /// A set operation: Return a new set containing all elements in both `self` and `rhs`
    fn intersection(&self, rhs: &Rhs) -> Self::Output;
}

pub trait Union<Rhs = Self> {
    type Output;

    /// A set operation: Return a new set containing all elements in `self` and all elements in `rhs`
    fn union(&self, rhs: &Rhs) -> Self::Output;
}

pub trait Subset<Rhs = Self> {
    /// A set operation: check if this set is subset of `rhs`
    fn subset_of(&self, rhs: &Rhs) -> bool;

    /// A set operation: check if this set is subset of `rhs`, but not equal to `rhs`
    fn strict_subset_of(&self, rhs: &Rhs) -> bool;
}
