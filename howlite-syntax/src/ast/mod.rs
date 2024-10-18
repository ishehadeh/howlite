use crate::{tree::NodeId, TreeChildren};

use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
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

#[derive(Debug, Clone)]
pub enum AstNodeData<A: Allocator = Global> {
    LiteralInteger(LiteralInteger),
    LiteralChar(LiteralChar),
    LiteralString(LiteralString),
    LiteralArray(LiteralArray<A>),
    LiteralStruct(LiteralStruct<A>),
    LiteralStructMember(LiteralStructMember),

    Ident(Ident),
    FieldAccess(FieldAccess),
    ArrayAccess(ArrayAccess),

    Repaired(Repaired),

    DefFunc(DefFunc<A>),
    DefParam(DefParam),
    DefImport(DefImport<A>),
    Block(Block),
    ExprIf(ExprIf),
    ExprCall(ExprCall<A>),
    ExprInfix(ExprInfix),
    ExprPrefix(ExprPrefix),
    ExprTypeConstruction(ExprTypeConstruction),

    ExprLet(ExprLet),
    StmtWhile(ExprWhile),

    DefType(DefType<A>),
    DefExternFunc(DefExternFunc<A>),
    DefExternVar(DefExternVar),

    Program(Program<A>),

    // Types
    TyRef(TyRef),
    TyExprUnion(TyExprUnion),
    TyStruct(TyStruct<A>),
    StructMember(TyStructMember),
    TyNumberRange(TyNumberRange),
    TyArray(TyArray),
    TyUnit(TyUnit),
    TyParam(TyParam),
    TySlice(TySlice),
    TyNamed(TyNamed<A>),
}

#[derive(Debug, Clone)]
pub struct AstNode<A: Allocator = Global> {
    pub span: Span,
    pub data: AstNodeData<A>,
}

impl<A: Allocator> PartialEq for AstNode<A> {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.data == other.data
    }
}

impl<A: Allocator> AstNode<A> {
    pub fn new<S: Into<Span>, T: Into<AstNodeData<A>>>(span: S, data: T) -> Self {
        AstNode {
            span: span.into(),
            data: data.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Repaired {
    pub tree: Option<NodeId<AstNode>>,
}

impl TreeChildren<AstNode> for Repaired {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.tree.iter().copied()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub field: Span,
    pub lhs: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for FieldAccess {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.lhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess {
    pub lhs: NodeId<AstNode>,
    pub index: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for ArrayAccess {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.lhs).chain(std::iter::once(self.index))
    }
}

#[derive(Debug, Clone)]
pub struct ExprCall<A: Allocator> {
    pub callee: NodeId<AstNode>,
    pub ty_params: Vec<NodeId<AstNode>, A>,
    pub params: Vec<NodeId<AstNode>, A>,
}

impl<A: Allocator> TreeChildren<AstNode> for ExprCall<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.callee)
            .chain(self.ty_params.iter().copied())
            .chain(self.params.iter().copied())
    }
}

impl<A: Allocator> PartialEq for ExprCall<A> {
    fn eq(&self, other: &Self) -> bool {
        self.callee == other.callee
            && self.ty_params == other.ty_params
            && self.params == other.params
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLet {
    pub name: Span,
    pub ty: NodeId<AstNode>,
    pub mutable: bool,
    pub value: NodeId<AstNode>,
}

impl TreeChildren<AstNode> for ExprLet {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.ty).chain(std::iter::once(self.value))
    }
}

#[derive(Debug, Clone)]
pub struct Program<A: Allocator> {
    pub definitions: Vec<NodeId<AstNode>, A>,
}

impl<A: Allocator> TreeChildren<AstNode> for Program<A> {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.definitions.iter().copied()
    }
}

impl<A: Allocator> PartialEq for Program<A> {
    fn eq(&self, other: &Self) -> bool {
        self.definitions == other.definitions
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIf {
    pub condition: NodeId<AstNode>,
    pub success: NodeId<AstNode>,
    pub failure: Option<NodeId<AstNode>>,
}

impl TreeChildren<AstNode> for ExprIf {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        std::iter::once(self.condition)
            .chain(std::iter::once(self.success))
            .chain(self.failure.into_iter())
    }
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
    ($($a:tt($($b:tt)*)),+ ) => {
        $(
            impl_ast_intos!(@imp $a ($($b)*));
        )*
    };

    (@imp $enum_node:ident($node_name:ident<A>)) => {
        impl<A: Allocator> From<$node_name<A>> for AstNodeData<A> {
            fn from(n: $node_name<A>) -> Self {
                AstNodeData::$enum_node(n)
            }
        }
    };

    (@imp $enum_node:ident($node_name:ident)) => {
            impl<A: Allocator> From<$node_name> for AstNodeData<A> {
                fn from(n: $node_name) -> Self {
                    AstNodeData::$enum_node(n)
                }
            }
    };


}

impl_ast_intos!(
    LiteralInteger(LiteralInteger),
    LiteralChar(LiteralChar),
    LiteralString(LiteralString),
    LiteralArray(LiteralArray<A>),
    Ident(Ident),
    FieldAccess(FieldAccess),
    ArrayAccess(ArrayAccess),
    Repaired(Repaired),
    DefFunc(DefFunc<A>),
    DefParam(DefParam),
    Block(Block),
    ExprIf(ExprIf),
    ExprCall(ExprCall<A>),
    ExprInfix(ExprInfix),
    ExprPrefix(ExprPrefix),
    LiteralStruct(LiteralStruct<A>),
    ExprLet(ExprLet),
    StmtWhile(ExprWhile),
    DefType(DefType<A>),
    DefExternFunc(DefExternFunc<A>),
    Program(Program<A>),
    TyRef(TyRef),
    TyStruct(TyStruct<A>),
    LiteralStructMember(LiteralStructMember),
    ExprTypeConstruction(ExprTypeConstruction),
    StructMember(TyStructMember),
    TyNumberRange(TyNumberRange),
    TyArray(TyArray),
    TyUnit(TyUnit),
    TyParam(TyParam),
    TyExprUnion(TyExprUnion),
    TySlice(TySlice),
    TyNamed(TyNamed<A>),
    DefImport(DefImport<A>),
    DefExternVar(DefExternVar)
);

macro_rules! on_ast_node_pair {
    (($lhs:expr => $p0:ident, $rhs:expr => $p1:ident) { $($action:expr);* } else $otherwise:expr) => {
        match ($lhs, $rhs) {
            (Self::LiteralInteger($p0), Self::LiteralInteger($p1)) => { $($action);* }
            (Self::LiteralChar($p0), Self::LiteralChar($p1)) => { $($action);* }
            (Self::LiteralString($p0), Self::LiteralString($p1)) => { $($action);* }
            (Self::LiteralArray($p0), Self::LiteralArray($p1)) => { $($action);* }
            (Self::LiteralStruct($p0), Self::LiteralStruct($p1)) => { $($action);* }
            (Self::LiteralStructMember($p0), Self::LiteralStructMember($p1)) => { $($action);* }
            (Self::Ident($p0), Self::Ident($p1)) => { $($action);* }
            (Self::FieldAccess($p0), Self::FieldAccess($p1)) => { $($action);* }
            (Self::ArrayAccess($p0), Self::ArrayAccess($p1)) => { $($action);* }
            (Self::Repaired($p0), Self::Repaired($p1)) => { $($action);* }
            (Self::DefFunc($p0), Self::DefFunc($p1)) => { $($action);* }
            (Self::DefParam($p0), Self::DefParam($p1)) => { $($action);* }
            (Self::DefImport($p0), Self::DefImport($p1)) => { $($action);* }
            (Self::Block($p0), Self::Block($p1)) => { $($action);* }
            (Self::ExprIf($p0), Self::ExprIf($p1)) => { $($action);* }
            (Self::ExprCall($p0), Self::ExprCall($p1)) => { $($action);* }
            (Self::ExprInfix($p0), Self::ExprInfix($p1)) => { $($action);* }
            (Self::ExprPrefix($p0), Self::ExprPrefix($p1)) => { $($action);* }
            (Self::ExprTypeConstruction($p0), Self::ExprTypeConstruction($p1)) => { $($action);* }
            (Self::ExprLet($p0), Self::ExprLet($p1)) => { $($action);* }
            (Self::StmtWhile($p0), Self::StmtWhile($p1)) => { $($action);* }
            (Self::DefType($p0), Self::DefType($p1)) => { $($action);* }
            (Self::DefExternFunc($p0), Self::DefExternFunc($p1)) => { $($action);* }
            (Self::DefExternVar($p0), Self::DefExternVar($p1)) => { $($action);* }
            (Self::Program($p0), Self::Program($p1)) => { $($action);* }
            (Self::TyRef($p0), Self::TyRef($p1)) => { $($action);* }
            (Self::TyExprUnion($p0), Self::TyExprUnion($p1)) => { $($action);* }
            (Self::TyStruct($p0), Self::TyStruct($p1)) => { $($action);* }
            (Self::StructMember($p0), Self::StructMember($p1)) => { $($action);* }
            (Self::TyNumberRange($p0), Self::TyNumberRange($p1)) => { $($action);* }
            (Self::TyArray($p0), Self::TyArray($p1)) => { $($action);* }
            (Self::TyUnit($p0), Self::TyUnit($p1)) => { $($action);* }
            (Self::TyParam($p0), Self::TyParam($p1)) => { $($action);* }
            (Self::TySlice($p0), Self::TySlice($p1)) => { $($action);* }
            (Self::TyNamed($p0), Self::TyNamed($p1)) => { $($action);* }
            _ => $otherwise,
        }
    };
}

impl<A: Allocator> PartialEq for AstNodeData<A> {
    fn eq(&self, other: &Self) -> bool {
        on_ast_node_pair! {
            (self => l, other => r) { l == r } else { false }
        }
    }
}
