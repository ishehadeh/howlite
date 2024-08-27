use crate::treeslab::{Node, NodeId};

use lrpar::Span;
use num_bigint::BigInt;

use std::fmt::Debug;

mod infix;
mod literals;
pub use infix::*;
pub use literals::*;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
// TODO split this into several enum types "ValueNode", "DefinitionNode", "Statement"
#[derive(Debug, Clone, PartialEq)]
pub enum AstNodeData {
    // TODO convert this to usize
    LiteralInteger(LiteralInteger),
    LiteralArray(LiteralArray),
    StructLiteral(StructLiteral),
    StructLiteralMember(StructLiteralMember),

    Ident(Ident),
    FieldAccess(FieldAccess),
    ArrayAccess(ArrayAccess),

    /// A repaired node is one where an error occured but parsing was still able to be completed
    /// This is typically used for non-critical errors like 1 + 1 + 1 instead of 1 + (1 + 1)
    Repaired(Repaired),

    DefFunction(DefFunction),
    Param(Param),
    Block(Block),
    StmtIf(StmtIf),
    ExprCall(ExprCall),
    Expr(ExprInfix),

    StmtLet(StmtLet),
    StmtWhile(StmtWhile),

    DefType(DefType),
    DefExtern(DefExtern),

    Program(Program),

    // Types
    TyRef(TyRef),
    TyStruct(TyStruct),
    StructMember(StructMember),
    TyNumberRange(TyNumberRange),
    TyArray(TyArray),
    TyBool(TyBool),
    TyUnit(TyUnit),
    TyParam(TyParam),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct AstNode {
    pub span: Span,
    pub data: AstNodeData,
}

impl AstNode {
    pub fn new<S: Into<Span>, T: Into<AstNodeData>>(span: S, data: T) -> AstNode {
        AstNode {
            span: span.into(),
            data: data.into(),
        }
    }
}

// TODO make StructMember and AnonType a proper part of the AST

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub typ: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyParam {
    pub name: String,
    pub super_ty: NodeId<AstNode>,

    pub default_ty: Option<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyArray {
    pub element_ty: NodeId<AstNode>,
    pub length: u32,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructMember {
    pub mutable: bool,
    pub name: String,
    pub ty: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyRef {
    pub name: String,
    pub parameters: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyStruct {
    pub members: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberRange {
    pub inclusive_low: String,
    pub inclusive_high: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyBool {}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyUnit {}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
pub enum Ty {
    TyRef(TyRef),
    Struct(TyStruct),
    NumberRange(TyNumberRange),
    Array(TyArray),
    Bool(TyBool),
    Unit(TyUnit),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Repaired {
    pub tree: Option<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub field: Ident,
    pub object: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess {
    pub index: NodeId<AstNode>,
    pub object: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall {
    pub function_name: String,
    pub paramaters: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtLet {
    pub name: String,
    pub ty: NodeId<AstNode>,
    pub mutable: bool,
    pub value: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub definitions: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefType {
    pub name: String,
    pub ty: NodeId<AstNode>,
    pub ty_params: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefExtern {
    pub name: String,
    pub params: Vec<NodeId<AstNode>>,
    pub return_ty: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtIf {
    pub condition: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
    pub else_: Option<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// Indicates that the value of the final value in `statements` should be the value of this block.
    /// For example:
    /// ```txt
    /// {
    ///     1 + 1
    /// }
    /// // Evaluates to `2`, `returns = true`
    /// ```
    ///
    /// ```txt
    /// {
    ///     1 + 1;
    /// }
    /// // evaluates to `unit`, `returns = false`
    /// ```
    pub returns: bool,
    pub statements: Vec<NodeId<AstNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub symbol: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefFunction {
    pub name: String,
    pub params: Vec<NodeId<AstNode>>,
    pub return_ty: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtWhile {
    pub condition: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
}

macro_rules! impl_ast_intos {
    ($($enum_node:ident($node_name:ident)),*) => {
        $(
            impl From<$node_name> for AstNodeData {
                fn from(n: $node_name) -> AstNodeData {
                    AstNodeData::$enum_node(n)
                }
            }
        )*
    };
}

impl_ast_intos!(
    LiteralInteger(LiteralInteger),
    LiteralArray(LiteralArray),
    Ident(Ident),
    FieldAccess(FieldAccess),
    ArrayAccess(ArrayAccess),
    Repaired(Repaired),
    DefFunction(DefFunction),
    Param(Param),
    Block(Block),
    StmtIf(StmtIf),
    ExprCall(ExprCall),
    Expr(ExprInfix),
    StructLiteral(StructLiteral),
    StmtLet(StmtLet),
    StmtWhile(StmtWhile),
    DefType(DefType),
    DefExtern(DefExtern),
    Program(Program),
    TyRef(TyRef),
    TyStruct(TyStruct),
    StructLiteralMember(StructLiteralMember),
    StructMember(StructMember),
    TyNumberRange(TyNumberRange),
    TyArray(TyArray),
    TyBool(TyBool),
    TyUnit(TyUnit),
    TyParam(TyParam)
);

impl Node for AstNode {}
