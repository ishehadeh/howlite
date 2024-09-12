mod ops;

impl<T: std::cmp::Ord> ops::Subset for std::ops::RangeInclusive<T> {
    fn subset_of(&self, rhs: &Self) -> bool {
        rhs.start() <= self.start() && rhs.end() >= self.end()
    }

    fn strict_subset_of(&self, rhs: &Self) -> bool {
        rhs.start() < self.start() && rhs.end() > self.end()
    }
}
