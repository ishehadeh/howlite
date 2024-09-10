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
use std::rc::Rc;

use preseli::integer::num_bigint::BigInt;
pub use preseli::IntegerSet;

mod access_path;
mod construct_macros;
mod errors;
mod util;

use ty_struct::StructField;
use util::try_collect::TryCollect;

pub use errors::AccessError;
pub mod ty_struct;
pub use access_path::{AccessPath, AccessPathElem};
use smallvec::SmallVec;
pub use ty_struct::TyStruct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
/// A TyId is a reference to a single type.
pub struct TyId {
    index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyInt {
    pub values: IntegerSet,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyArray<SymbolT: Eq> {
    pub length: usize,
    pub element_ty: Ty<SymbolT>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TySlice<SymbolT: Eq> {
    pub index_set: IntegerSet,
    pub element_ty: Ty<SymbolT>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyReference<SymbolT: Eq> {
    pub referenced_ty: Ty<SymbolT>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyUnion<SymbolT: Eq> {
    pub tys: SmallVec<[Ty<SymbolT>; 16]>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Ty<SymbolT: Eq> {
    Int(TyInt),
    Struct(Rc<TyStruct<SymbolT>>),
    Array(Rc<TyArray<SymbolT>>),
    Slice(Rc<TySlice<SymbolT>>),
    Reference(Rc<TyReference<SymbolT>>),
    Union(Rc<TyUnion<SymbolT>>),
}

impl<SymbolT: Eq> Clone for Ty<SymbolT> {
    fn clone(&self) -> Self {
        match self {
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
impl<SymbolT: Eq> Ty<SymbolT> {
    _impl_as!(as_struct(Ty::Struct) => Rc<TyStruct<SymbolT>>);
    _impl_as!(as_int(&Ty::Int) => &TyInt);
    _impl_as!(as_array(Ty::Array) => Rc<TyArray<SymbolT>>);
    _impl_as!(as_slice(Ty::Slice) => Rc<TySlice<SymbolT>>);
    _impl_as!(as_union(Ty::Union) => Rc<TyUnion<SymbolT>>);
    _impl_as!(as_reference(Ty::Reference) => Rc<TyReference<SymbolT>>);

    pub fn sizeof(&self) -> usize {
        // TODO: this assumes 32 bit, make some way to chance that...
        match self {
            Ty::Int(_) => 4,
            Ty::Struct(s) => s.fields.iter().map(|f| f.ty.sizeof()).sum(),
            Ty::Array(arr) => arr.element_ty.sizeof() * arr.length,
            Ty::Slice(_) => 8,
            Ty::Reference(_) => 4,
            Ty::Union(u) => u.tys.iter().map(|f| f.sizeof()).max().unwrap_or(0),
        }
    }

    pub fn access_field(&self, access_symbol: SymbolT) -> Result<Ty<SymbolT>, AccessError> {
        let strucs = match self {
            Ty::Struct(s) => vec![s.clone()],
            Ty::Union(u) => u
                .tys
                .iter()
                .map(|t| {
                    t.as_struct()
                        .clone()
                        .ok_or(AccessError::NonStructUnionVariant)
                })
                .try_collect_poly()?,
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
        let mut union_tys: SmallVec<[Ty<SymbolT>; 16]> = SmallVec::new();
        for (offset, struc_field) in fields_with_offset {
            if offset != req_offset {
                return Err(AccessError::FieldMisaligned);
            }

            union_tys.push(struc_field.ty.clone())
        }

        if union_tys.len() == 1 {
            Ok(union_tys[0].clone())
        } else {
            Ok(Ty::Union(Rc::new(TyUnion { tys: union_tys })))
        }
    }

    pub fn access_index(&self, index: impl Into<BigInt>) -> Result<Ty<SymbolT>, AccessError> {
        let index = index.into();

        let ty_index = match self {
            Ty::Array(s) => vec![(
                s.element_ty.clone(),
                IntegerSet::new_from_tuples(&[(0, s.length.saturating_sub(1))]),
            )],
            Ty::Slice(s) => vec![(s.element_ty.clone(), s.index_set.clone())],
            Ty::Union(u) => {
                // we can't mix slices and arrays here (slices have additional indirection)
                let req_arr = u.tys[0].as_array().is_some();

                let mut acc = Vec::new();
                for x in &u.tys {
                    match x {
                        Ty::Array(s) => {
                            if !req_arr {
                                return Result::Err(AccessError::MixedSliceAndArrayUnion);
                            }
                            acc.push((
                                s.element_ty.clone(),
                                IntegerSet::new_from_tuples(&[(0, s.length.saturating_sub(1))]),
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
            _ => return Result::Err(AccessError::IllegalFieldAccess),
        };

        let req_size = ty_index[0].0.sizeof();
        let mut union_tys: SmallVec<[_; 16]> = SmallVec::with_capacity(ty_index.len());
        for (ty, index_set) in ty_index {
            if ty.sizeof() != req_size {
                return Err(AccessError::MixedSeriesElementSizeUnion);
            }

            if !index_set.contains(&index) {
                return Err(AccessError::OutOfRange);
            }

            union_tys.push(ty);
        }

        if union_tys.len() == 1 {
            Ok(union_tys[0].clone())
        } else {
            Ok(Ty::Union(Rc::new(TyUnion { tys: union_tys })))
        }
    }
}

pub trait IntRepr {
    fn sizeof(i: &TyInt) -> usize;
}
#[cfg(test)]
mod test {
    use crate::{t_array, t_int, t_struct, t_union, Ty};

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
        let a1: Ty<()> = t_array! [ t_int!(0..10); 10 ];
        let a2: Ty<()> = t_array! [ t_int!(20..25); 15 ];

        let x1 = a1.access_index(1).unwrap();
        assert_eq!(x1, t_int!(0..10));

        let x2 = a2.access_index(2).unwrap();
        assert_eq!(x2, t_int!(20..25));

        let u1 = t_union!(a1, a2);

        let x3 = u1.access_index(9).unwrap();
        assert_eq!(x3, t_union!(t_int!(0..10), t_int!(20..25)));
    }
}
