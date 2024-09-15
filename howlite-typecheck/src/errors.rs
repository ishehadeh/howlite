use std::rc::Rc;

use preseli::IntegerSet;

use crate::{Symbol, Ty, TyInt, TyStruct};

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum AccessError {
    #[error("cannot access field on non-struct type")]
    IllegalFieldAccess,

    #[error("cannot access fields on unions which contain a non-struct type")]
    NonStructUnionVariant,

    #[error("All union variants must contain a field to access it")]
    FieldMissingInUnionVariant,

    #[error("Although the field exists on all union variants, it cannot be accessed since it is at different offsets.")]
    FieldMisaligned,

    #[error("cannot access index on non-slice, non-array type")]
    IllegalIndexAccess,

    #[error("unions may only be indexed if they contain only slices, or only arrays, not both")]
    MixedSliceAndArrayUnion,

    #[error("unions may not be indexed if they do not contain only arrays, or only slices")]
    NonIndexableUnion,

    #[error(
        "union of arrays and slices must all have elements of the same size in order to index them"
    )]
    MixedSeriesElementSizeUnion,

    #[error("index out of range")]
    OutOfRange,
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum IncompatibleError<SymbolT: Symbol> {
    #[error("expected {expected:?}, found {found:?}")]
    UnexpectedTyKind {
        expected: Ty<SymbolT>,
        found: Ty<SymbolT>,
    },

    #[error("integer set {:?} is not a subset of {:?}. (not a in superset: {:?})", subset.values, superset.values, { let mut excl = subset.values.clone(); excl.subtract(&superset.values); excl} )]
    IntegerSubsetError { subset: TyInt, superset: TyInt },

    #[error("series indicies are incompatible: expected indicies {:?}, got {:?}, (missing: {:?})", subset_indicies, superset_indicies, { let mut excl = subset_indicies.clone(); excl.subtract(superset_indicies); excl})]
    IncompatibleIndices {
        subset_indicies: IntegerSet,
        superset_indicies: IntegerSet,
    },

    #[error("collection element types are incompatible: {:?}", error)]
    IncompatibleElement {
        error: Box<IncompatibleError<SymbolT>>,
    },

    #[error(
        "Series elements are not the same size, expected {:?}, found {:?}",
        subset_size,
        superset_size
    )]
    SeriesElementsWrongSize {
        subset_size: usize,
        superset_size: usize,
    },

    #[error("incompatible structures, field mismatches: {bad_fields:?}. When comparing {subset_struct:?} and {superset_struct:?}")]
    StructIncompatibility {
        subset_struct: Rc<TyStruct<SymbolT>>,
        superset_struct: Rc<TyStruct<SymbolT>>,
        bad_fields: Vec<(SymbolT, StructIncompatibility<SymbolT>)>,
    },
}

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum StructIncompatibility<SymbolT: Symbol> {
    #[error("expected at offset {expected_offset}, found at offset {found_offset}")]
    BadOffset {
        found_offset: usize,
        expected_offset: usize,
    },

    #[error("field missing")]
    MissingField,

    #[error("incompatible field types: {error:?}")]
    IncompatibleField { error: IncompatibleError<SymbolT> },
}
