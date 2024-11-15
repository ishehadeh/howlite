//! Module for conventient syntax for constructing type trees.

#[allow(unused_imports, reason = "we use this import in the module's macros")]
pub use preseli;

#[macro_export]
macro_rules! t_struct {
    ($($field:expr => $ty:expr),* ,) => {
        t_struct! {
            $($field => $ty),*
        }
    };

    (
        $($field:expr => $ty:expr),*
    ) => {
        std::rc::Rc::new($crate::Ty::Struct($crate::TyStruct {
            fields: smallvec::smallvec![
                $($crate::types::StructField {
                    name: $field,
                    ty: $ty
                }),*
            ]
        }))
    };
}

#[macro_export]
macro_rules! t_int {
    ($($toks:tt)*) => {
        std::rc::Rc::new($crate::Ty::Int({
            $crate::types::TyInt::from_set($crate::preseli::iset!($($toks)*))
        }))
    };
}

#[macro_export]
macro_rules! t_array {
    ($ty:expr ; $len:expr) => {
        std::rc::Rc::new($crate::Ty::Array($crate::TyArray {
            length: $len,
            element_ty: $ty,
        }))
    };
}

#[macro_export]
macro_rules! t_union {
    ($($ty:expr),*) => {
        std::rc::Rc::new($crate::Ty::Union($crate::TyUnion {
            tys: smallvec::smallvec![$($ty),*]
        }))
    };
}

#[macro_export]
macro_rules! t_slice {
    ($ty:expr ; $($toks:tt)*) => {
        std::rc::Rc::new($crate::Ty::Slice($crate::TySlice {
            index_set: $crate::t_int!($($toks)*),
            element_ty: $ty,
        }))
    };
}
