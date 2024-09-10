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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty<SymbolT: Eq> {
    Int(TyInt),
    Struct(Rc<TyStruct<SymbolT>>),
    Array(Rc<TyArray<SymbolT>>),
    Slice(Rc<TySlice<SymbolT>>),
    Reference(Rc<TyReference<SymbolT>>),
    Union(Rc<TyUnion<SymbolT>>),
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
}

pub trait IntRepr {
    fn sizeof(i: &TyInt) -> usize;
}
