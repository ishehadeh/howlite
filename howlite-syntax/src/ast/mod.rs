use crate::tree::NodeId;

use lrpar::Span;

use std::fmt::Debug;

mod definitions;
mod infix;
mod literals;
mod prefix;
mod ty_expr;
pub use definitions::*;
pub use infix::*;
pub use literals::*;
pub use prefix::*;
pub use ty_expr::*;



#[derive(Debug, Clone, PartialEq)]
pub enum AstNodeData {
    LiteralInteger(LiteralInteger),
    LiteralChar(LiteralChar),
    LiteralString(LiteralString),
    LiteralArray(LiteralArray),
    LiteralStruct(LiteralStruct),
    LiteralStructMember(LiteralStructMember),

    Ident(Ident),
    FieldAccess(FieldAccess),
    ArrayAccess(ArrayAccess),

    /// FIXME: with the new parser generator (grmtools) we may not need this
    Repaired(Repaired),

    DefFunc(DefFunc),
    DefParam(DefParam),
    DefImport(DefImport),
    Block(Block),
    ExprIf(ExprIf),
    ExprCall(ExprCall),
    ExprInfix(ExprInfix),
    ExprPrefix(ExprPrefix),
    ExprTypeConstruction(ExprTypeConstruction),

    ExprLet(ExprLet),
    StmtWhile(ExprWhile),

    DefType(DefType),
    DefExternFunc(DefExternFunc),
    DefExternVar(DefExternVar),

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


#[derive(Debug, Clone, PartialEq)]
pub struct Repaired {
    pub tree: Option<NodeId<AstNode>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub field: Span,
    pub lhs: NodeId<AstNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess {
    pub index: NodeId<AstNode>,
    pub lhs: NodeId<AstNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall {
    pub callee: NodeId<AstNode>,
    pub ty_params: Vec<NodeId<AstNode>>,
    pub params: Vec<NodeId<AstNode>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ExprLet {
    pub name: Span,
    pub ty: NodeId<AstNode>,
    pub mutable: bool,
    pub value: NodeId<AstNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub definitions: Vec<NodeId<AstNode>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ExprIf {
    pub condition: NodeId<AstNode>,
    pub success: NodeId<AstNode>,
    pub failure: Option<NodeId<AstNode>>,
}


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


#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub symbol: Span,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ExprWhile {
    pub condition: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ExprTypeConstruction {
    pub ty: NodeId<AstNode>,
    pub value: NodeId<AstNode>,
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
    LiteralChar(LiteralChar),
    LiteralString(LiteralString),
    LiteralArray(LiteralArray),
    Ident(Ident),
    FieldAccess(FieldAccess),
    ArrayAccess(ArrayAccess),
    Repaired(Repaired),
    DefFunc(DefFunc),
    DefParam(DefParam),
    Block(Block),
    ExprIf(ExprIf),
    ExprCall(ExprCall),
    ExprInfix(ExprInfix),
    ExprPrefix(ExprPrefix),
    LiteralStruct(LiteralStruct),
    ExprLet(ExprLet),
    StmtWhile(ExprWhile),
    DefType(DefType),
    DefExternFunc(DefExternFunc),
    Program(Program),
    TyRef(TyRef),
    TyStruct(TyStruct),
    LiteralStructMember(LiteralStructMember),
    ExprTypeConstruction(ExprTypeConstruction),
    StructMember(TyStructMember),
    TyNumberRange(TyNumberRange),
    TyArray(TyArray),
    TyUnit(TyUnit),
    TyParam(TyParam),
    TyExprUnion(TyExprUnion),
    TySlice(TySlice),
    TyNamed(TyNamed),
    DefImport(DefImport),
    DefExternVar(DefExternVar)
);
