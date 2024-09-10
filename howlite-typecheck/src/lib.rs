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
    pub length_ty: Ty<SymbolT>,
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

impl<SymbolT: Eq> Ty<SymbolT> {
    pub fn as_struct(&self) -> Option<Rc<TyStruct<SymbolT>>> {
        match self {
            Ty::Struct(r) => Some(r.clone()),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<&TyInt> {
        match self {
            Ty::Int(r) => Some(r),
            _ => None,
        }
    }

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
}

pub trait IntRepr {
    fn sizeof(i: &TyInt) -> usize;
}
#[cfg(test)]
mod test {
    use crate::{t_int, t_struct, t_union};

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
}
