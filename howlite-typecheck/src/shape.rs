use std::{
    num::NonZeroU8,
    ops::{BitOr, BitOrAssign},
};

macro_rules! ty_shape_n {
    ($name:ident => $bit_index:literal) => {
        // SAFETY
        //  assuming this shift doesn't overflow, it will be non-zero (since we're shifting 1)
        //  this assumtion is safe because a constant shifted by a constant will raise a compiler error if it will certainly overflow.
        pub const $name: TypeShape =
            TypeShape(unsafe { NonZeroU8::new_unchecked(1u8 << $bit_index) });
    };
}

#[derive(PartialEq, Eq, PartialOrd, Ord, std::hash::Hash, Copy, Clone)]
pub struct TypeShape(NonZeroU8);
impl TypeShape {
    ty_shape_n!(INTEGER => 1);
    ty_shape_n!(STRUCT => 2);
    ty_shape_n!(ARRAY => 3);
    ty_shape_n!(SLICE => 4);
    ty_shape_n!(REFERENCE => 5);
    ty_shape_n!(HOLE => 6);
    ty_shape_n!(UNIT => 7);

    pub const fn any() -> TypeShape {
        Self::union([
            Self::INTEGER,
            Self::STRUCT,
            Self::ARRAY,
            Self::SLICE,
            Self::REFERENCE,
            Self::HOLE,
            Self::UNIT,
        ])
    }

    /// produce a union of all the given shapes
    pub const fn union<const N: usize>(shapes: [TypeShape; N]) -> TypeShape {
        assert!(N > 0);
        let mut cumulative_shapes = shapes[0];
        let mut i = 1;
        while i < N {
            cumulative_shapes = cumulative_shapes.include(shapes[i]);
            i += 1;
        }

        cumulative_shapes
    }

    // get a mask of all bits that should never be set on a valid TypeShape
    const fn unused_bits() -> u8 {
        !Self::any().0.get()
    }

    /// The number of individual types in this TypeShape
    pub const fn count_members(&self) -> usize {
        self.0.get().count_ones() as usize
    }

    /// check that this shape is a superset of TypeShape
    pub const fn contains(&self, other: TypeShape) -> bool {
        (self.0.get() & other.0.get()) == other.0.get()
    }

    pub const fn include(self, new: TypeShape) -> TypeShape {
        // SAFETY:
        //   assuming new and self are non-zero, its safe to assume the union of the two is non-zero
        //   this assumtion is safe since they are both stored as NonZeroU8
        TypeShape(unsafe { NonZeroU8::new_unchecked(self.0.get() | new.0.get()) })
    }

    pub const fn intersect(self, rhs: TypeShape) -> Option<TypeShape> {
        match NonZeroU8::new(self.0.get() & rhs.0.get()) {
            Some(v) => Some(TypeShape(v)),
            None => None,
        }
    }

    pub const fn exclude(self, new: TypeShape) -> Option<TypeShape> {
        let values_not_excluded = match new.complement() {
            Some(v) => v,
            None => return None,
        };

        self.intersect(values_not_excluded)
    }

    pub const fn complement(self) -> Option<Self> {
        let valid_complement_bits = !self.0.get() & !Self::unused_bits();
        match NonZeroU8::new(valid_complement_bits) {
            Some(v) => Some(TypeShape(v)),
            None => None,
        }
    }
}

impl IntoIterator for TypeShape {
    type Item = Self;

    type IntoIter = TypeShapeIter;

    fn into_iter(self) -> Self::IntoIter {
        TypeShapeIter(self.0.get())
    }
}

impl<'a> IntoIterator for &'a TypeShape {
    type Item = TypeShape;

    type IntoIter = TypeShapeIter;

    fn into_iter(self) -> Self::IntoIter {
        TypeShapeIter(self.0.get())
    }
}

/// Iterate individual members of the type shape: E.G. "INTEGER", "UNION", etc.
pub struct TypeShapeIter(u8);

impl Iterator for TypeShapeIter {
    type Item = TypeShape;

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.0.trailing_zeros();
        if i == 8 {
            None
        } else {
            let bit = 1 << i;
            self.0 &= !bit;
            Some(TypeShape(
                NonZeroU8::new(bit).expect("bit did not exist in type shape"),
            ))
        }
    }
}

impl ExactSizeIterator for TypeShapeIter {
    fn len(&self) -> usize {
        self.0.count_ones() as usize
    }
}

impl DoubleEndedIterator for TypeShapeIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        let i = self.0.leading_zeros();
        if i == 8 {
            None
        } else {
            let bit = 1 << i;
            self.0 &= !bit;
            Some(TypeShape(
                NonZeroU8::new(bit).expect("bit did not exist in type shape"),
            ))
        }
    }
}

impl BitOrAssign for TypeShape {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = self.include(rhs);
    }
}
impl BitOr<Self> for TypeShape {
    type Output = TypeShape;
    fn bitor(mut self, rhs: Self) -> Self {
        self.bitor_assign(rhs);
        self
    }
}

impl std::fmt::Debug for TypeShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let count = self.count_members();
        for (i, t) in self.into_iter().enumerate() {
            match t {
                TypeShape::INTEGER => write!(f, "Integer"),
                TypeShape::ARRAY => write!(f, "Array"),
                TypeShape::REFERENCE => write!(f, "Reference"),
                TypeShape::SLICE => write!(f, "Slice"),
                TypeShape::STRUCT => write!(f, "Struct"),
                TypeShape::HOLE => write!(f, "Hole"),
                TypeShape::UNIT => write!(f, "Unit"),
                _ => unreachable!("invalid TypeShape returned by iterator"),
            }?;
            if i != count - 1 {
                write!(f, " | ")?
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for TypeShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let count = self.count_members();
        for (i, t) in self.into_iter().enumerate() {
            match t {
                TypeShape::INTEGER => write!(f, "Integer"),
                TypeShape::ARRAY => write!(f, "Array"),
                TypeShape::REFERENCE => write!(f, "Reference"),
                TypeShape::SLICE => write!(f, "Slice"),
                TypeShape::STRUCT => write!(f, "Struct"),
                TypeShape::HOLE => write!(f, "<Unknown>"),
                TypeShape::UNIT => write!(f, "Unit"),

                _ => unreachable!("invalid TypeShape returned by iterator"),
            }?;
            // no comma if theres only two, so we say "A or B"
            // also no common on the last element so we don't end up with "A, B, C, or D,"
            if i != count - 1 && count != 2 {
                write!(f, ", ")?
            }
            if count > 1 && i == count - 2 {
                write!(f, " or ")?
            }
        }
        Ok(())
    }
}
