use crate::treeslab::{Node, NodeId};

use lrpar::Span;

use std::fmt::Debug;

mod infix;
mod literals;
mod prefix;
mod ty_expr;
pub use infix::*;
pub use literals::*;
pub use prefix::*;
pub use ty_expr::*;

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
    ExprInfix(ExprInfix),
    ExprPrefix(ExprPrefix),

    StmtLet(StmtLet),
    StmtWhile(StmtWhile),

    DefType(DefType),
    DefExtern(DefExtern),

    Program(Program),

    // Types
    TyRef(TyRef),
    TyExprUnion(TyExprUnion),
    TyStruct(TyStruct),
    StructMember(TyStructMember),
    TyNumberRange(TyNumberRange),
    TyArray(TyArray),
    TyUnit(TyUnit),
    TyParam(TyParam),
    TySlice(TySlice),
    TyNamed(TyNamed),
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
    pub name: NodeId<AstNode>,
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
    pub symbol: Span,
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
    ExprInfix(ExprInfix),
    ExprPrefix(ExprPrefix),
    StructLiteral(StructLiteral),
    StmtLet(StmtLet),
    StmtWhile(StmtWhile),
    DefType(DefType),
    DefExtern(DefExtern),
    Program(Program),
    TyRef(TyRef),
    TyStruct(TyStruct),
    StructLiteralMember(StructLiteralMember),
    StructMember(TyStructMember),
    TyNumberRange(TyNumberRange),
    TyArray(TyArray),
    TyUnit(TyUnit),
    TyParam(TyParam),
    TyExprUnion(TyExprUnion),
    TySlice(TySlice),
    TyNamed(TyNamed)
);

impl Node for AstNode {}
