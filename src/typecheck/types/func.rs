use crate::typecheck::TypeInfo;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncType {
    pub return_ty: Box<TypeInfo>,
    pub param_ty: Vec<TypeInfo>,
}
