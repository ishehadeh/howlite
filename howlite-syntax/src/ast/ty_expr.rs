use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
use lrpar::Span;

use crate::{gen_node_impls, tree::NodeId};

use super::AstNode;

#[derive(Debug, Clone, PartialEq)]
pub struct TyArray {
    pub element_ty: NodeId<AstNode>,
    pub length: i128,
}
gen_node_impls!(TyArray { &element_ty, length });

#[derive(Debug, Clone, PartialEq)]
pub struct TySlice {
    pub element_ty: NodeId<AstNode>,
    pub length_ty: NodeId<AstNode>,
}
gen_node_impls!(TySlice { &element_ty, &length_ty });

#[derive(Debug, Clone, PartialEq)]
pub struct TyExprUnion {
    pub lhs: NodeId<AstNode>,
    pub rhs: NodeId<AstNode>,
}
gen_node_impls!(TyExprUnion { &lhs, &rhs });

#[derive(Debug, Clone, PartialEq)]
pub struct TyStructMember {
    pub mutable: bool,
    pub name: Span,
    pub ty: NodeId<AstNode>,
}
gen_node_impls!(TyStructMember { name, mutable, &ty });

#[derive(Debug, Clone)]
pub struct TyNamed<A: Allocator = Global> {
    pub name: NodeId<AstNode>,
    pub parameters: Vec<NodeId<AstNode>, A>,
}
gen_node_impls!(TyNamed<A> { &name, &parameters* });

#[derive(Debug, Clone, PartialEq)]
pub struct TyRef {
    pub referenced_ty: NodeId<AstNode>,
}
gen_node_impls!(TyRef { &referenced_ty });

#[derive(Debug, Clone)]
pub struct TyStruct<A: Allocator> {
    pub members: Vec<NodeId<AstNode>, A>,
}
gen_node_impls!(TyStruct<A> { &members* });

#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberRange {
    pub lo: NodeId<AstNode>,
    pub hi: NodeId<AstNode>,
}
gen_node_impls!(TyNumberRange { &lo, &hi });

#[derive(Debug, Clone, PartialEq)]
pub struct TyParam {
    pub name: Span,
    pub super_ty: NodeId<AstNode>,

    pub default_ty: Option<NodeId<AstNode>>,
}
gen_node_impls!(TyParam { name, &super_ty, &default_ty? });

#[derive(Debug, Clone, PartialEq)]
pub struct TyUnit {}

gen_node_impls!(TyUnit {});
