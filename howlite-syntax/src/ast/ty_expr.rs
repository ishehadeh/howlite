use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
use smol_str::SmolStr;

use crate::{gen_node_impls, tree::DefaultLinearTreeId};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]

pub struct TyArray<ChildT = DefaultLinearTreeId> {
    pub element_ty: ChildT,
    pub length: i128,
}
gen_node_impls!(TyArray { &element_ty, length });

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TySlice<ChildT = DefaultLinearTreeId> {
    pub element_ty: ChildT,
    pub length_ty: ChildT,
}
gen_node_impls!(TySlice { &element_ty, &length_ty });

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TyExprUnion<ChildT = DefaultLinearTreeId> {
    pub lhs: ChildT,
    pub rhs: ChildT,
}
gen_node_impls!(TyExprUnion { &lhs, &rhs });
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyStructMember<ChildT = DefaultLinearTreeId> {
    pub mutable: bool,
    pub name: SmolStr,
    pub ty: ChildT,
}
gen_node_impls!(TyStructMember { name, mutable, &ty });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound(
        serialize = "ChildT: serde::Serialize, A: Allocator",
        deserialize = "ChildT: serde::Deserialize<'de>, A: Allocator + Default"
    ))
)]
pub struct TyNamed<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub name: SmolStr,
    pub parameters: Vec<ChildT, A>,
}
gen_node_impls!(TyNamed<A> { name, &parameters* });

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TyRef<ChildT = DefaultLinearTreeId> {
    pub referenced_ty: ChildT,
}
gen_node_impls!(TyRef { &referenced_ty });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound(
        serialize = "ChildT: serde::Serialize, A: Allocator",
        deserialize = "ChildT: serde::Deserialize<'de>, A: Allocator + Default"
    ))
)]
pub struct TyStruct<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub members: Vec<ChildT, A>,
}
gen_node_impls!(TyStruct<A> { &members* });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TyNumberRange<ChildT = DefaultLinearTreeId> {
    pub lo: ChildT,
    pub hi: ChildT,
}
gen_node_impls!(TyNumberRange { &lo, &hi });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TyParam<ChildT = DefaultLinearTreeId> {
    pub name: SmolStr,
    pub super_ty: ChildT,

    pub default_ty: Option<ChildT>,
}
gen_node_impls!(TyParam { name, &super_ty, &default_ty? });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TyUnit {}

gen_node_impls!(TyUnit {});
