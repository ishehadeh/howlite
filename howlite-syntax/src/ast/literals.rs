use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
#[cfg(feature = "proptest")]
use proptest::prelude::*;
#[cfg(feature = "proptest")]
use proptest_derive::Arbitrary;
use smol_str::SmolStr;

use crate::{gen_node_impls, tree::DefaultLinearTreeId};

use super::AstNode;
#[derive(Debug, Clone)]
#[cfg_attr(feature = "proptest", derive(Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LiteralInteger {
    #[cfg_attr(feature = "proptest", proptest(strategy = "(0..u64::MAX as i128)"))]
    pub value: i128,
}
gen_node_impls!(LiteralInteger { value });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "proptest", derive(Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]

pub struct LiteralChar {
    pub value: char,
}
gen_node_impls!(LiteralChar { value });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "proptest", derive(Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LiteralString {
    #[cfg_attr(
        feature = "proptest",
        proptest(
            params = "proptest::string::StringParam",
            strategy = "any_with::<String>(params).prop_map(SmolStr::from)"
        )
    )]
    pub value: SmolStr,
}
gen_node_impls!(LiteralString { value });

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LiteralStructMember<ChildT = DefaultLinearTreeId> {
    pub field: SmolStr,
    pub value: ChildT,
}
gen_node_impls!(LiteralStructMember { field, &value });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound(
        serialize = "ChildT: serde::Serialize, A: Allocator",
        deserialize = "ChildT: serde::Deserialize<'de>, A: Allocator + Default"
    ))
)]
pub struct LiteralStruct<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub members: Vec<AstNode<LiteralStructMember<ChildT>>, A>,
}
gen_node_impls!(LiteralStruct<A> { %members* });

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound(
        serialize = "ChildT: serde::Serialize, A: Allocator",
        deserialize = "ChildT: serde::Deserialize<'de>, A: Allocator + Default"
    ))
)]
pub struct LiteralArray<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub values: Vec<ChildT, A>,
}
gen_node_impls!(LiteralArray<A> { &values* });
