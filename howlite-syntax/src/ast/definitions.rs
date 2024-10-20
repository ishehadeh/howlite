use crate::{gen_node_impls, tree::DefaultLinearTreeId};
use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
use lrpar::Span;

#[derive(Debug, Clone)]
pub struct DefType<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub name: Span,
    pub alias: bool,
    pub ty: ChildT,
    pub ty_params: Vec<ChildT, A>,
}
gen_node_impls!(DefType<A> { name, alias, &ty_params*, &ty });

#[derive(Debug, Clone)]
pub struct DefExternFunc<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub name: Span,
    pub ty_params: Vec<ChildT, A>,
    pub params: Vec<ChildT, A>,
    pub return_ty: ChildT,
}
gen_node_impls!(DefExternFunc<A> { name, &ty_params*, &params*, &return_ty });

#[derive(Debug, Clone, PartialEq)]
pub struct DefExternVar<ChildT = DefaultLinearTreeId> {
    pub name: Span,
    pub mutable: bool,
    pub ty: ChildT,
}
gen_node_impls!(DefExternVar { name, mutable, &ty });

#[derive(Debug, Clone)]
pub struct DefImport {
    pub file: Span,
}
gen_node_impls!(DefImport { file });

#[derive(Debug, Clone)]
pub struct DefFunc<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub name: Span,
    pub ty_params: Vec<ChildT, A>,
    pub params: Vec<ChildT, A>,
    pub return_ty: ChildT,
    pub body: ChildT,
}
gen_node_impls!(DefFunc<A> { name, &ty_params*, &params*, &return_ty, &body });

#[derive(Debug, Clone, PartialEq)]
pub struct DefParam<ChildT = DefaultLinearTreeId> {
    pub mutable: bool,
    pub name: Span,
    pub ty: ChildT,
}
gen_node_impls!(DefParam { name, mutable, &ty });
