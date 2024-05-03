use std::collections::BTreeSet;

pub mod typetree;

mod errors;
pub mod types;
pub use errors::*;

use types::{IntegerType, RecordCell, RecordType};

use self::types::{ArrayType, TyRef};
pub use self::typetree::TypeTree;
pub struct TypeContext {}
pub trait IntoTypedNode<T> {
    fn into_typed_node(self, ctx: TypeContext) -> T;
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScalarType {
    Float64,
    Float32,
    Integer(IntegerType),
    Character,

    /// booleans may be used as a true/false unit type
    Boolean(Option<bool>),
}

impl ScalarType {
    /// apply an operation to both sides of an integer range type, return none if self or rhs is not an integer type.
    fn int_op(&self, map: fn(l: i32, r: i32) -> i32, rhs: &ScalarType) -> Option<ScalarType> {
        match (self, rhs) {
            (ScalarType::Integer(a), ScalarType::Integer(b)) => {
                Some(ScalarType::Integer(IntegerType {
                    lo: map(a.lo, b.lo),
                    hi: map(a.hi, b.hi),
                }))
            }
            (a, b) if a == b => Some(a.clone()),
            _ => None,
        }
    }

    pub fn add(&self, rhs: &ScalarType) -> Option<ScalarType> {
        self.int_op(|a, b| a + b, rhs)
    }
    pub fn sub(&self, rhs: &ScalarType) -> Option<ScalarType> {
        self.int_op(|a, b| a - b, rhs)
    }
    pub fn mul(&self, rhs: &ScalarType) -> Option<ScalarType> {
        self.int_op(|a, b| a * b, rhs)
    }
    pub fn div(&self, rhs: &ScalarType) -> Option<ScalarType> {
        self.int_op(|a, b| a / b, rhs)
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnionType {
    pub types: Vec<TypeInfo>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntersectionType {
    pub types: Vec<TypeInfo>,
}

impl UnionType {
    pub fn intersect(&self) -> TypeInfo {
        self.types
            .iter()
            .cloned()
            .reduce(|intersect, next| intersect.intersect(&next))
            .unwrap_or(TypeInfo::Unit)
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeInfo {
    /// A type with its only value being 'unit'
    Unit,
    // TODO tuples?
    Scalar(ScalarType),
    Union(UnionType),
    Record(RecordType),
    Array(ArrayType),

    /// A reference to another type or type parameter
    TyRef(TyRef),
}

impl TypeInfo {
    pub fn integer(lo: i32, hi: i32) -> TypeInfo {
        TypeInfo::Scalar(ScalarType::Integer(IntegerType::new(lo, hi)))
    }

    pub fn bool() -> TypeInfo {
        TypeInfo::Scalar(ScalarType::Boolean(None))
    }

    pub fn bool_valued(value: bool) -> TypeInfo {
        TypeInfo::Scalar(ScalarType::Boolean(Some(value)))
    }

    pub fn union(types: impl Into<Vec<TypeInfo>>) -> TypeInfo {
        TypeInfo::Union(UnionType {
            types: types.into(),
        })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, TypeInfo::Scalar(ScalarType::Integer(_)))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, TypeInfo::Scalar(ScalarType::Boolean(_)))
    }

    pub fn is_record(&self) -> bool {
        matches!(self, TypeInfo::Record(_))
    }

    pub fn record(fields: impl Into<BTreeSet<RecordCell>>) -> TypeInfo {
        TypeInfo::Record(RecordType {
            fields: fields.into(),
        })
    }

    pub fn get_size(&self) -> usize {
        match self {
            TypeInfo::Scalar(ScalarType::Integer(_)) => 4,
            TypeInfo::Scalar(ScalarType::Boolean(_)) => 1,
            TypeInfo::Scalar(_) => todo!(),
            TypeInfo::Unit => 0,
            TypeInfo::Union(u) => u.types.iter().map(|x| x.get_size()).max().unwrap_or(0),
            TypeInfo::Record(r) => r.fields.iter().map(|x| x.length).sum::<usize>(),
            TypeInfo::Array(a) => a.element_ty.get_size() * a.length as usize,
            TypeInfo::TyRef(_) => panic!("type references cannot be sized before being resolved"),
        }
    }

    pub fn access<S: AsRef<str>>(&self, symbol: S) -> Result<TypeInfo, TypeError> {
        match self {
            TypeInfo::Scalar(_) | TypeInfo::Array(_) | TypeInfo::TyRef(_) | TypeInfo::Unit => {
                Err(TypeError::InvalidFieldAccess {
                    symbol: symbol.as_ref().to_string(),
                    object: self.clone(),
                })
            }
            TypeInfo::Union(u) => {
                let union_intersect = u.intersect();
                union_intersect.access(symbol)
            }
            TypeInfo::Record(r) => {
                let s_ref = symbol.as_ref();
                r.fields
                    .iter()
                    .find(|f| f.name == s_ref)
                    .map(|f| f.type_info.clone())
                    .ok_or(TypeError::InvalidFieldAccess {
                        symbol: s_ref.to_string(),
                        object: self.clone(),
                    })
            }
        }
    }

    pub fn is_subset(&self, superset: &TypeInfo) -> bool {
        // to avoid bugs with forgotten cases DO NOT use an _ => false pattern in this match
        match (self, superset) {
            // trivial case
            (a, b) if a == b => true,

            // if two type references aren't exactly equal, then they aren't subsets
            // for a proper comparison they should be resolved by the typechecker, then
            // compared with is_subset
            (_, TypeInfo::TyRef(_)) | (TypeInfo::TyRef(_), _) => false,

            // unit only matches unit
            (TypeInfo::Unit, TypeInfo::Unit) => true,

            // integer range intersect
            (
                TypeInfo::Scalar(ScalarType::Integer(a)),
                TypeInfo::Scalar(ScalarType::Integer(b)),
            ) => a.is_subset(b),

            (TypeInfo::Scalar(ScalarType::Float32), TypeInfo::Scalar(ScalarType::Float32)) => true,
            (TypeInfo::Scalar(ScalarType::Float64), TypeInfo::Scalar(ScalarType::Float64)) => true,
            // we can losslessly convert float32 -> float64, but not the reverse
            (TypeInfo::Scalar(ScalarType::Float64), TypeInfo::Scalar(ScalarType::Float32)) => false,
            (TypeInfo::Scalar(ScalarType::Float32), TypeInfo::Scalar(ScalarType::Float64)) => true,
            (TypeInfo::Scalar(ScalarType::Float32 | ScalarType::Float64), _)
            | (_, TypeInfo::Scalar(ScalarType::Float32 | ScalarType::Float64)) => false,

            // the value within a boolean is for value tracking only, it doesn't have any effect on typechecking
            (
                TypeInfo::Scalar(ScalarType::Boolean(_)),
                TypeInfo::Scalar(ScalarType::Boolean(_)),
            ) => true,
            (TypeInfo::Scalar(ScalarType::Boolean(_)), _)
            | (_, TypeInfo::Scalar(ScalarType::Boolean(_))) => false,

            // the value within a boolean is for value tracking only, it doesn't have any effect on typechecking
            (TypeInfo::Scalar(ScalarType::Character), TypeInfo::Scalar(ScalarType::Character)) => {
                true
            }
            (TypeInfo::Scalar(ScalarType::Character), _)
            | (_, TypeInfo::Scalar(ScalarType::Character)) => false,

            (TypeInfo::Union(union), other) => {
                for t in &union.types {
                    if !t.is_subset(other) {
                        return false;
                    }
                }
                true
            }

            (TypeInfo::Record(a), TypeInfo::Record(b)) => {
                for field in &a.fields {
                    if !b.fields.contains(field) {
                        return false;
                    }
                }
                true
            }

            (TypeInfo::Array(a), TypeInfo::Array(b)) => {
                // 1) subetset must be indexable by a subset of supersets index set
                // 2) element type must be a subset
                // 3) (for practicle purposes) element types must be the same size
                a.length <= b.length
                    && a.element_ty.is_subset(b.element_ty.as_ref())
                    && self.get_size() == superset.get_size()
            }

            (me, TypeInfo::Union(union)) => {
                for t in &union.types {
                    if me.is_subset(t) {
                        return true;
                    }
                }
                false
            }

            // fundamentally incompatible types
            (TypeInfo::Unit, TypeInfo::Scalar(_) | TypeInfo::Record(_) | TypeInfo::Array(_)) => {
                false
            }
            (TypeInfo::Scalar(_), TypeInfo::Unit | TypeInfo::Record(_) | TypeInfo::Array(_)) => {
                false
            }
            (TypeInfo::Record(_), TypeInfo::Unit | TypeInfo::Scalar(_) | TypeInfo::Array(_)) => {
                false
            }
            (TypeInfo::Array(_), TypeInfo::Unit | TypeInfo::Scalar(_) | TypeInfo::Record(_)) => {
                false
            }
        }
    }

    pub fn intersect(&self, other: &TypeInfo) -> TypeInfo {
        // TODO no intersect and unit are different things, intersect should return some kind of None or 0 type.

        match (self, other) {
            // trivial case
            (a, b) if a == b => a.clone(),

            // () intersect a = ()
            (TypeInfo::Unit, _) | (_, TypeInfo::Unit) => TypeInfo::Unit,

            // Intersect cannot be called with references, this should be an error of some kind
            // it usually indicates a bug in the compiler
            (TypeInfo::TyRef(_), _) | (_, TypeInfo::TyRef(_)) => TypeInfo::Unit,

            // integer range intersect
            (
                TypeInfo::Scalar(ScalarType::Integer(a)),
                TypeInfo::Scalar(ScalarType::Integer(b)),
            ) => a
                .intersect(b)
                .map(|i| TypeInfo::Scalar(ScalarType::Integer(i)))
                .unwrap_or(TypeInfo::Unit),

            // except for integers scalars have no non-trivial intersect
            (TypeInfo::Scalar(_), TypeInfo::Scalar(_)) => TypeInfo::Unit,

            (TypeInfo::Union(union), other) | (other, TypeInfo::Union(union)) => {
                TypeInfo::Union(UnionType {
                    types: union.types.iter().map(|t| other.intersect(t)).collect(),
                })
            }
            (TypeInfo::Record(a), TypeInfo::Record(b)) => {
                let fields: BTreeSet<RecordCell> =
                    a.fields.intersection(&b.fields).cloned().collect();
                if !fields.is_empty() {
                    TypeInfo::Record(RecordType { fields })
                } else {
                    TypeInfo::Unit
                }
            }

            (TypeInfo::Array(a), TypeInfo::Array(b)) => {
                if self.get_size() != other.get_size() {
                    TypeInfo::Unit
                } else {
                    let length = a.length.min(b.length);
                    let element_ty = a.element_ty.intersect(&b.element_ty);
                    TypeInfo::Array(ArrayType {
                        length,
                        element_ty: Box::new(element_ty),
                    })
                }
            }

            (TypeInfo::Record(_) | TypeInfo::Array(_), TypeInfo::Scalar(_))
            | (TypeInfo::Scalar(_), TypeInfo::Record(_) | TypeInfo::Array(_)) => TypeInfo::Unit,

            (TypeInfo::Record(_), TypeInfo::Array(_))
            | (TypeInfo::Array(_), TypeInfo::Record(_)) => TypeInfo::Unit,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub paramaters: Vec<(String, TypeInfo)>,
    pub returns: TypeInfo,
}

impl Default for FunctionDeclaration {
    fn default() -> Self {
        FunctionDeclaration {
            paramaters: Vec::new(),
            returns: TypeInfo::Unit,
        }
    }
}
