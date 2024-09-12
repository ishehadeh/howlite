pub mod bitfield;
pub mod ops;

#[derive(Clone, Debug)]
pub struct Range<T: std::cmp::Ord> {
    lo: T,
    hi: T,
}

impl<T: std::cmp::Ord> Range<T> {
    pub fn new(lo: T, hi: T) -> Self {
        Self::try_new(lo, hi).expect("range lower bound must be greater than upper bound")
    }

    pub fn try_new(lo: T, hi: T) -> Option<Self> {
        if hi < lo {
            None
        } else {
            Some(Range { lo, hi })
        }
    }

    pub fn lo(&self) -> &T {
        &self.lo
    }

    pub fn hi(&self) -> &T {
        &self.hi
    }

    pub fn into_tuple(self) -> (T, T) {
        (self.lo, self.hi)
    }
}

impl<'a, T: std::cmp::Ord> ops::Subset for &'a Range<T> {
    fn subset_of(self, rhs: Self) -> bool {
        rhs.lo() <= self.lo() && rhs.hi() >= self.hi()
    }

    fn strict_subset_of(self, rhs: Self) -> bool {
        rhs.lo() < self.lo() && rhs.hi() > self.hi()
    }
}

impl<T: Ord> ops::Intersect for Range<T> {
    type Output = Option<Range<T>>;

    fn intersect(self, rhs: Self) -> Self::Output {
        let (l_lo, l_hi) = self.into_tuple();
        let (r_lo, r_hi) = rhs.into_tuple();
        Range::try_new(T::max(l_lo, r_lo), T::min(l_hi, r_hi))
    }
}

impl<T: Ord> ops::Set<T> for Range<T> {
    fn includes(&self, element: T) -> bool {
        &element >= self.lo() && &element <= self.hi()
    }
}
impl<'a, T: Ord> ops::Set<&'a T> for Range<T> {
    fn includes(&self, element: &'a T) -> bool {
        element >= self.lo() && element <= self.hi()
    }
}
