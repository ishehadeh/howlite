//! Module for conventient syntax for constructing type trees.
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
        $crate::Ty::Struct(std::rc::Rc::new($crate::TyStruct {
            fields: smallvec::smallvec![
                $($crate::ty_struct::StructField {
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
        $crate::Ty::Int({
            $crate::TyInt {
                values: $crate::construct_macros::preseli::iset!($($toks)*)
            }
        })
    };
}

#[macro_export]
macro_rules! t_array {
    ($ty:expr ; $len:expr) => {
        $crate::Ty::Array(std::rc::Rc::new($crate::TyArray {
            length: $len,
            element_ty: $ty,
        }))
    };
}

#[macro_export]
macro_rules! t_union {
    ($($ty:expr),*) => {
        $crate::Ty::Union(std::rc::Rc::new($crate::TyUnion {
            tys: smallvec::smallvec![$($ty),*]
        }))
    };
}
