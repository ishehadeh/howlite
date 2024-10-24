//! The Howlite Typechecker.
//!
//! This is a structural typechecker, where single scalar type, integers, are expressed as sets.
//! There is no type inference built in to this implementation.
//!
//! ## Implementation
//!
//! There are a few core kinds in this typechecker:
//! - Tuples (Arrays, Structs)
//! - Scalars (Integers)
//! - References (Slice, Reference)
//!
//!
use std::{hash::Hash, rc::Rc};

use errors::{IncompatibleError, OperationError, StructIncompatibility};
use preseli::integer::Scalar;
pub use preseli::IntegerSet;

mod access_path;
mod construct_macros;
pub mod errors;
pub mod types;
mod util;
use types::{StructField, TyInt, TyStruct};
mod typetable;
use sunstone::ops::{SetOpIncludes, Subset};
use util::try_collect::TryCollect;

pub use access_path::{AccessPath, AccessPathElem};
pub use errors::AccessError;
use smallvec::SmallVec;
pub mod constraints;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyArray<SymbolT: Symbol> {
    pub length: usize,
    pub element_ty: Rc<Ty<SymbolT>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TySlice<SymbolT: Symbol> {
    pub index_set: IntegerSet,
    pub element_ty: Rc<Ty<SymbolT>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyReference<SymbolT: Symbol> {
    pub referenced_ty: Rc<Ty<SymbolT>>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyUnion<SymbolT: Symbol> {
    pub tys: SmallVec<[Rc<Ty<SymbolT>>; 8]>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Ty<SymbolT: Symbol> {
    Hole,
    Int(TyInt),
    Struct(TyStruct<SymbolT>),
    Array(TyArray<SymbolT>),
    Slice(TySlice<SymbolT>),
    Reference(TyReference<SymbolT>),
    Union(TyUnion<SymbolT>),
}

impl<SymbolT: Symbol> Clone for Ty<SymbolT> {
    fn clone(&self) -> Self {
        match self {
            Self::Hole => Self::Hole,
            Self::Int(arg0) => Self::Int(arg0.clone()),
            Self::Struct(arg0) => Self::Struct(arg0.clone()),
            Self::Array(arg0) => Self::Array(arg0.clone()),
            Self::Slice(arg0) => Self::Slice(arg0.clone()),
            Self::Reference(arg0) => Self::Reference(arg0.clone()),
            Self::Union(arg0) => Self::Union(arg0.clone()),
        }
    }
}

macro_rules! _impl_as {
    ($fn_name:ident($variant:path) => $t:ty) => {
        pub fn $fn_name(&self) -> Option<$t> {
            match self {
                $variant(r) => Some(r.clone()),
                _ => None,
            }
        }
    };

    ($fn_name:ident(& $variant:path) => $t:ty) => {
        pub fn $fn_name(&self) -> Option<$t> {
            match self {
                $variant(r) => Some(&r),
                _ => None,
            }
        }
    };
}
impl<SymbolT: Symbol> Ty<SymbolT> {
    /// A type with a single value
    pub const fn unit() -> Self {
        Ty::Struct(TyStruct {
            fields: SmallVec::new_const(),
        })
    }

    _impl_as!(as_struct(&Ty::Struct) => &TyStruct<SymbolT>);
    _impl_as!(as_int(&Ty::Int) => &TyInt);
    _impl_as!(as_array(&Ty::Array) => &TyArray<SymbolT>);
    _impl_as!(as_slice(&Ty::Slice) => &TySlice<SymbolT>);
    _impl_as!(as_union(&Ty::Union) => &TyUnion<SymbolT>);
    _impl_as!(as_reference(&Ty::Reference) => &TyReference<SymbolT>);

    pub fn sizeof(&self) -> usize {
        // TODO: this assumes 32 bit, make some way to chance that...
        match self {
            Ty::Hole => 0,
            Ty::Int(_) => 4,
            Ty::Struct(s) => s.fields.iter().map(|f| f.ty.sizeof()).sum(),
            Ty::Array(arr) => arr.element_ty.sizeof() * arr.length,
            Ty::Slice(_) => 8,
            Ty::Reference(_) => 4,
            Ty::Union(u) => u.tys.iter().map(|f| f.sizeof()).max().unwrap_or(0),
        }
    }

    pub fn arith_add(&self, other: &Self) -> Result<Self, OperationError<SymbolT>> {
        match (self, other) {
            (Ty::Hole, _) => Ok(Ty::Hole),
            (_, Ty::Hole) => Ok(Ty::Hole),
            (Ty::Int(l), Ty::Int(r)) => Ok(Ty::Int(l.add(r))),
            (l, Ty::Int(_)) => Err(OperationError::ExpectedScalar {
                found: Box::new(l.clone()),
            }),
            (_, r) => Err(OperationError::ExpectedScalar {
                found: Box::new(r.clone()),
            }),
        }
    }

    pub fn arith_mul(&self, other: &Self) -> Result<Self, OperationError<SymbolT>> {
        match (self, other) {
            (Ty::Hole, _) => Ok(Ty::Hole),
            (_, Ty::Hole) => Ok(Ty::Hole),

            (Ty::Int(l), Ty::Int(r)) => Ok(Ty::Int(l.mul(r))),
            (l, Ty::Int(_)) => Err(OperationError::ExpectedScalar {
                found: Box::new(l.clone()),
            }),
            (_, r) => Err(OperationError::ExpectedScalar {
                found: Box::new(r.clone()),
            }),
        }
    }

    pub fn access_field(&self, access_symbol: SymbolT) -> Result<Rc<Ty<SymbolT>>, AccessError> {
        let strucs = match self {
            Ty::Struct(s) => vec![s],
            Ty::Union(u) => u
                .tys
                .iter()
                .map(|t| t.as_struct().ok_or(AccessError::NonStructUnionVariant))
                .try_collect_poly()?,
            Ty::Hole => return Ok(Rc::new(Ty::Hole)),
            _ => return Result::Err(AccessError::IllegalFieldAccess),
        };
        let fields_with_offset: Vec<(usize, &StructField<SymbolT>)> = strucs
            .iter()
            .map(|s| {
                let mut offset = 0;
                for field in &s.fields {
                    if field.name == access_symbol {
                        return Ok((offset, field));
                    }
                    offset += field.ty.sizeof();
                }

                Err(AccessError::FieldMissingInUnionVariant)
            })
            .try_collect_poly()?;

        let req_offset = fields_with_offset[0].0;
        let mut union_tys: SmallVec<[Rc<Ty<SymbolT>>; 8]> = SmallVec::new();
        for (offset, struc_field) in fields_with_offset {
            if offset != req_offset {
                return Err(AccessError::FieldMisaligned);
            }

            union_tys.push(struc_field.ty.clone())
        }

        if union_tys.len() == 1 {
            Ok(union_tys[0].clone())
        } else {
            Ok(Rc::new(Ty::Union(TyUnion { tys: union_tys })))
        }
    }

    pub fn access_index(&self, index: impl Into<Scalar>) -> Result<Rc<Ty<SymbolT>>, AccessError> {
        let index = index.into();

        let ty_index = match self {
            Ty::Array(s) => vec![(
                s.element_ty.clone(),
                IntegerSet::new_from_tuples(&[(0, s.length.saturating_sub(1) as i128)]),
            )],
            Ty::Slice(s) => vec![(s.element_ty.clone(), s.index_set.clone())],
            Ty::Union(u) => {
                // we can't mix slices and arrays here (slices have additional indirection)
                let req_arr = u.tys[0].as_array().is_some();

                let mut acc = Vec::new();
                for x in &u.tys {
                    match x.as_ref() {
                        Ty::Array(s) => {
                            if !req_arr {
                                return Result::Err(AccessError::MixedSliceAndArrayUnion);
                            }
                            acc.push((
                                s.element_ty.clone(),
                                IntegerSet::new_from_tuples(&[(
                                    0,
                                    s.length.saturating_sub(1) as i128,
                                )]),
                            ));
                        }
                        Ty::Slice(s) => {
                            if req_arr {
                                return Result::Err(AccessError::MixedSliceAndArrayUnion);
                            }
                            acc.push((s.element_ty.clone(), s.index_set.clone()));
                        }
                        _ => return Result::Err(AccessError::MixedSliceAndArrayUnion),
                    }
                }
                acc
            }
            Ty::Hole => return Ok(Rc::new(Ty::Hole)),
            _ => return Result::Err(AccessError::IllegalFieldAccess),
        };

        let req_size = ty_index[0].0.sizeof();
        let mut union_tys: SmallVec<[_; 8]> = SmallVec::with_capacity(ty_index.len());
        for (ty, index_set) in ty_index {
            if ty.sizeof() != req_size {
                return Err(AccessError::MixedSeriesElementSizeUnion);
            }

            if !index_set.includes(index) {
                return Err(AccessError::OutOfRange);
            }

            union_tys.push(ty);
        }

        if union_tys.len() == 1 {
            Ok(union_tys[0].clone())
        } else {
            Ok(Rc::new(Ty::Union(TyUnion { tys: union_tys })))
        }
    }

    pub fn is_assignable_to(&self, other: &Ty<SymbolT>) -> Result<(), IncompatibleError<SymbolT>> {
        match (self, other) {
            (Ty::Int(superset), Ty::Int(subset)) => {
                if subset.values.subset_of(&superset.values) {
                    Ok(())
                } else {
                    Err(IncompatibleError::IntegerSubsetError {
                        subset: subset.clone(),
                        superset: superset.clone(),
                    })
                }
            }
            (Ty::Array(superset), Ty::Array(subset)) => {
                if subset.element_ty.sizeof() != superset.element_ty.sizeof() {
                    return Err(IncompatibleError::SeriesElementsWrongSize {
                        subset_size: subset.element_ty.sizeof(),
                        superset_size: superset.element_ty.sizeof(),
                    });
                }
                subset
                    .element_ty
                    .is_assignable_to(&superset.element_ty)
                    .map_err(|error| IncompatibleError::IncompatibleElement {
                        error: Box::new(error),
                    })?;
                if subset.length <= superset.length {
                    Err(IncompatibleError::IncompatibleIndices {
                        subset_indicies: IntegerSet::new_from_tuples(&[(0, subset.length as i128)]),
                        superset_indicies: IntegerSet::new_from_tuples(&[(
                            0,
                            superset.length as i128,
                        )]),
                    })
                } else {
                    Ok(())
                }
            }
            (Ty::Slice(superset), Ty::Slice(subset)) => {
                if subset.element_ty.sizeof() != superset.element_ty.sizeof() {
                    return Err(IncompatibleError::SeriesElementsWrongSize {
                        subset_size: subset.element_ty.sizeof(),
                        superset_size: superset.element_ty.sizeof(),
                    });
                }
                subset
                    .element_ty
                    .is_assignable_to(&superset.element_ty)
                    .map_err(|error| IncompatibleError::IncompatibleElement {
                        error: Box::new(error),
                    })?;
                if subset.index_set.subset_of(&superset.index_set) {
                    Err(IncompatibleError::IncompatibleIndices {
                        subset_indicies: subset.index_set.clone(),
                        superset_indicies: superset.index_set.clone(),
                    })
                } else {
                    Ok(())
                }
            }

            (Ty::Struct(superset), Ty::Struct(subset)) => {
                // forall field in subset there exists a field in superset with the same offset, size, and a compatible type.

                let mut superset_fields_by_offset =
                    SmallVec::<[(usize, StructField<SymbolT>); 8]>::with_capacity(
                        superset.fields.len(),
                    );
                for field in &superset.fields {
                    superset_fields_by_offset.push((
                        superset_fields_by_offset
                            .last()
                            .map(|(offset, field)| offset + field.ty.sizeof())
                            .unwrap_or(0),
                        field.clone(),
                    ))
                }

                let mut subset_offset = 0;
                let mut errors = Vec::new();
                for sub_field in &subset.fields {
                    let super_field_offset = superset_fields_by_offset
                        .iter()
                        .find(|(_, field)| field.name == sub_field.name);
                    match super_field_offset {
                        Some((super_offset, super_field)) => {
                            if *super_offset != subset_offset {
                                errors.push((
                                    super_field.name.clone(),
                                    StructIncompatibility::BadOffset {
                                        found_offset: *super_offset,
                                        expected_offset: subset_offset,
                                    },
                                ))
                            } else if let Err(error) =
                                super_field.ty.is_assignable_to(&sub_field.ty)
                            {
                                errors.push((
                                    super_field.name.clone(),
                                    StructIncompatibility::IncompatibleField { error },
                                ))
                            }
                        }
                        None => {
                            errors.push((
                                sub_field.name.clone(),
                                StructIncompatibility::MissingField,
                            ));
                        }
                    }

                    subset_offset += sub_field.ty.sizeof();
                }
                if !errors.is_empty() {
                    Err(IncompatibleError::StructIncompatibility {
                        subset_struct: subset.clone(),
                        superset_struct: superset.clone(),
                        bad_fields: errors,
                    })
                } else {
                    Ok(())
                }
            }

            _ => Err(IncompatibleError::UnexpectedTyKind {
                expected: other.clone(),
                found: self.clone(),
            }),
        }
    }
}

pub trait IntRepr {
    fn sizeof(i: &TyInt) -> usize;
}
pub trait Symbol: Eq + std::fmt::Debug + Clone + Hash {}

impl<T> Symbol for T where T: Eq + std::fmt::Debug + Clone + Hash {}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::{errors::IncompatibleError, t_array, t_int, t_slice, t_struct, t_union, Ty};

    #[test]
    fn field_access() {
        let s1 = t_struct! {
            "a" => t_int!(0)
        };
        let s2 = t_struct! {
            "a" => t_int!(2)
        };

        let a1 = s1.access_field("a").unwrap();
        assert_eq!(a1, t_int!(0));

        let a2 = s2.access_field("a").unwrap();
        assert_eq!(a2, t_int!(2));

        let u1 = t_union!(s1, s2);

        let a3 = u1.access_field("a").unwrap();
        assert_eq!(a3, t_union!(t_int!(0), t_int!(2)));
    }

    #[test]
    fn array_access() {
        let a1: Rc<Ty<()>> = t_array! [ t_int!(0..10); 10 ];
        let a2: Rc<Ty<()>> = t_array! [ t_int!(20..25); 15 ];

        let x1 = a1.access_index(1).unwrap();
        assert_eq!(x1, t_int!(0..10));

        let x2 = a2.access_index(2).unwrap();
        assert_eq!(x2, t_int!(20..25));

        let u1 = t_union!(a1, a2);

        let x3 = u1.access_index(9).unwrap();
        assert_eq!(x3, t_union!(t_int!(0..10), t_int!(20..25)));
    }

    #[test]
    fn compat_struct() {
        let s1 = t_struct! {
            "a" => t_int!(0),
            "b" => t_int!(0)
        };
        let s2 = t_struct! {
            "b" => t_int!(0),
            "a" => t_int!(0)
        };

        assert!(matches!(
            s1.is_assignable_to(&s2).unwrap_err(),
            IncompatibleError::StructIncompatibility { .. }
        ));
    }

    #[test]
    fn compat_array() {
        let a1: Rc<Ty<()>> = t_array![t_int!(0..10); 10];
        let a2 = t_array![t_int!(0..5); 5];

        a2.is_assignable_to(&a1).unwrap();

        let a3: Rc<Ty<()>> = t_array![t_int!(0..10); 9];

        a1.is_assignable_to(&a3).unwrap_err();
    }

    #[test]
    fn compat_slice() {
        let a1: Rc<Ty<()>> = t_slice![t_int!(0..10); 0..10];
        let a2 = t_slice![t_int!(0..5); 0..5];

        a2.is_assignable_to(&a1).unwrap();

        let a3 = t_array![t_int!(0..10); 9];

        a1.is_assignable_to(&a3).unwrap_err();
    }
}
