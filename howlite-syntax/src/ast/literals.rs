use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
use lrpar::Span;
use smol_str::SmolStr;

use crate::{gen_node_impls, tree::DefaultLinearTreeId};

#[derive(Debug, Clone)]
pub struct LiteralInteger {
    pub value: i128,
}
gen_node_impls!(LiteralInteger { value });

#[derive(Debug, Clone)]
pub struct LiteralChar {
    pub value: char,
}
gen_node_impls!(LiteralChar { value });

#[derive(Debug, Clone)]
pub struct LiteralString {
    pub value: SmolStr,
}
gen_node_impls!(LiteralString { value });

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralStructMember<ChildT = DefaultLinearTreeId> {
    pub field: SmolStr,
    pub value: ChildT,
}
gen_node_impls!(LiteralStructMember { field, &value });

#[derive(Debug, Clone)]
pub struct LiteralStruct<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub members: Vec<ChildT, A>,
}
gen_node_impls!(LiteralStruct<A> { &members* });

#[derive(Debug, Clone)]
pub struct LiteralArray<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub values: Vec<ChildT, A>,
}
gen_node_impls!(LiteralArray<A> { &values* });
