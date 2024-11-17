use crate::{gen_node_impls, tree::DefaultLinearTreeId};
use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
use smol_str::SmolStr;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound(
        serialize = "ChildT: serde::Serialize, A: Allocator",
        deserialize = "ChildT: serde::Deserialize<'de>, A: Allocator + Default"
    ))
)]
pub struct DefType<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub name: SmolStr,
    pub alias: bool,
    pub ty: ChildT,
    pub ty_params: Vec<ChildT, A>,
}
gen_node_impls!(DefType<A> { name, alias, &ty_params*, &ty });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound(
        serialize = "ChildT: serde::Serialize, A: Allocator",
        deserialize = "ChildT: serde::Deserialize<'de>, A: Allocator + Default"
    ))
)]
pub struct DefExternFunc<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub name: SmolStr,
    pub ty_params: Vec<ChildT, A>,
    pub params: Vec<ChildT, A>,
    pub return_ty: ChildT,
}
gen_node_impls!(DefExternFunc<A> { name, &ty_params*, &params*, &return_ty });

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct DefExternVar<ChildT = DefaultLinearTreeId> {
    pub name: SmolStr,
    pub mutable: bool,
    pub ty: ChildT,
}
gen_node_impls!(DefExternVar { name, mutable, &ty });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct DefImport {
    pub file: SmolStr,
}
gen_node_impls!(DefImport { file });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound(
        serialize = "ChildT: serde::Serialize, A: Allocator",
        deserialize = "ChildT: serde::Deserialize<'de>, A: Allocator + Default"
    ))
)]
pub struct DefFunc<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub name: SmolStr,
    pub ty_params: Vec<ChildT, A>,
    pub params: Vec<ChildT, A>,
    pub return_ty: ChildT,
    pub body: ChildT,
}
gen_node_impls!(DefFunc<A> { name, &ty_params*, &params*, &return_ty, &body });

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct DefParam<ChildT = DefaultLinearTreeId> {
    pub mutable: bool,
    pub name: SmolStr,
    pub ty: ChildT,
}
gen_node_impls!(DefParam { name, mutable, &ty });
