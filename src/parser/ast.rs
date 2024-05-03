use crate::{
    span::{SourceSpan, Spanned},
    util::idvec::{Id, IdVec, IdVecToken},
};

use std::fmt::Debug;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfixOp {
    Add,
    Sub,
    Div,
    Mul,

    Assign,

    CmpNe,
    CmpEq,
    CmpGt,
    CmpLt,
    CmpGe,
    CmpLe,
}

// TODO make StructMember and AnonType a proper part of the AST

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Param<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub typ: Ty<X>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyParam<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub super_ty: Ty<X>,

    pub default_ty: Option<Ty>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyArray<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub element_ty: Box<Ty<X>>,
    pub length: u32,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructMember<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub mutable: bool,
    pub name: String,
    pub ty: Ty<X>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyRef<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub parameters: Vec<Ty<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyStruct<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub members: Vec<StructMember<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberRange<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub inclusive_low: String,
    pub inclusive_high: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyBool<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyUnit<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
pub enum Ty<X: Debug + Clone = ()> {
    TyRef(TyRef<X>),
    Struct(TyStruct<X>),
    NumberRange(TyNumberRange<X>),
    Array(TyArray<X>),
    Bool(TyBool<X>),
    Unit(TyUnit<X>),
}

/// Trait to access extension data on an ast node
pub trait XData<X> {
    fn xdata(&self) -> &X;
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
// TODO split this into several enum types "ValueNode", "DefinitionNode", "Statement"
#[derive(Debug, Clone, PartialEq)]
pub enum Ast<X: Debug + Clone = ()> {
    // TODO convert this to usize
    LiteralInteger(LiteralInteger<X>),
    LiteralBool(LiteralBool<X>),
    LiteralArray(LiteralArray<X>),
    Ident(Ident<X>),
    FieldAccess(FieldAccess<X>),
    ArrayAccess(ArrayAccess<X>),

    /// A repaired node is one where an error occured but parsing was still able to be completed
    /// This is typically used for non-critical errors like 1 + 1 + 1 instead of 1 + (1 + 1)
    Repaired(Repaired<X>),

    DefFunction(DefFunction<X>),
    Block(Block<X>),
    StmtIf(StmtIf<X>),
    ExprCall(ExprCall<X>),
    Expr(Expr<X>),

    StructLiteral(StructLiteral<X>),
    StmtLet(StmtLet<X>),
    StmtWhile(StmtWhile<X>),

    DefType(DefType<X>),
    DefExtern(DefExtern<X>),

    Program(Program<X>),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Repaired<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub tree: Option<Box<Ast<X>>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub field: Ident,
    pub object: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub index: Box<Ast<X>>,
    pub object: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Expr<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub lhs: Box<Ast<X>>,
    pub op: InfixOp,
    pub rhs: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub function_name: String,
    pub paramaters: Vec<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtLet<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub ty: Ty<X>,
    pub mutable: bool,
    pub value: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Program<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub definitions: Vec<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefType<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub ty: Ty<X>,
    pub ty_params: Vec<TyParam<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefExtern<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub params: Vec<Param<X>>,
    pub return_ty: Ty<X>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtIf<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub condition: Box<Ast<X>>,
    pub body: Box<Ast<X>>,
    pub else_: Option<Box<Ast<X>>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Block<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

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
    pub statements: Vec<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralInteger<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub value: i32,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralBool<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub value: bool,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Ident<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub symbol: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefFunction<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub params: Vec<Param<X>>,
    pub return_ty: Ty<X>,
    pub body: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralMember<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub field: Ident,
    pub value: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub members: Vec<StructLiteralMember<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralArray<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub values: Vec<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtWhile<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub condition: Box<Ast<X>>,
    pub body: Box<Ast<X>>,
}

/// Implement Spanned for a struct, with the given member of type SourceSpan
macro_rules! impl_ast_node {
    ($t:ident) => {
        impl<X: Debug + Clone> Spanned for $t<X> {
            fn span(&self) -> &SourceSpan {
                &self.span
            }
        }

        impl<X: Debug + Clone> XData<X> for $t<X> {
            fn xdata(&self) -> &X {
                &self.xdata
            }
        }
    };
}

macro_rules! impl_ast_traits {
    ($ast_ty:ident : $($member:ident ($member_ty:ident)),*) => {
        $(impl_ast_node!{ $member_ty })*

        impl<X: Debug + Clone> Spanned for $ast_ty<X> {
            fn span(&self) -> &SourceSpan {
                match self {
                    $($ast_ty::$member(a) => a.span()),*
                }
            }
        }

        impl<X: Debug + Clone> XData<X> for $ast_ty<X> {
            fn xdata(&self) -> &X {
                match self {
                    $($ast_ty::$member(a) => a.xdata()),*
                }
            }
        }
    };
    ($ast_ty:ident : $($member:ident),*) => {
        impl_ast_traits!($ast_ty : $($member ($member)),*);
    };
}

impl_ast_traits!(Ast :
    Program,
    Expr,
    DefType,
    DefExtern,
    ExprCall,
    StmtIf,
    StmtLet,
    Block,
    DefFunction,
    Ident,
    LiteralInteger,
    LiteralBool,
    Repaired,
    FieldAccess,
    StructLiteral,
    LiteralArray,
    ArrayAccess,
    StmtWhile
);

impl_ast_node! { StructLiteralMember }
impl_ast_node! { Param }
impl_ast_node! { TyParam }

impl_ast_traits!(Ty :
    TyRef (TyRef),
    Struct (TyStruct),
    Array (TyArray),
    NumberRange (TyNumberRange),
    Bool (TyBool),
    Unit (TyUnit)
);

pub type SyntaxNodeRef = Id<Ast>;

#[derive(Clone, Debug, Default)]
pub struct SyntaxTreeBuffer {
    nodes: IdVec<Ast>,
}

impl SyntaxTreeBuffer {
    pub fn new() -> SyntaxTreeBuffer {
        SyntaxTreeBuffer::default()
    }

    pub fn add<NodeT: Into<Ast>>(&mut self, node: NodeT) -> SyntaxNodeRef {
        self.nodes.push(node.into())
    }

    pub fn get(&self, node: SyntaxNodeRef) -> &Ast {
        self.nodes.get(node)
    }

    pub fn with_root(self, root: SyntaxNodeRef) -> SyntaxTree {
        SyntaxTree {
            nodes: self.nodes,
            root,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxTree {
    nodes: IdVec<Ast>,
    root: SyntaxNodeRef,
}

impl SyntaxTree {
    pub fn root(&self) -> SyntaxNodeRef {
        self.root.clone()
    }

    // iterate through the tree in insertion order (i.e. bottom up)
    pub fn iter(&self) -> impl Iterator<Item = (SyntaxNodeRef, &Ast)> {
        self.nodes.iter()
    }

    pub fn get(&self, node: SyntaxNodeRef) -> &Ast {
        self.nodes.get(node)
    }

    pub fn map<T, F>(&self, f: F) -> SyntaxData<T>
    where
        F: Fn(SyntaxNodeRef, &Ast) -> T,
    {
        let mut data = Vec::with_capacity(self.nodes.len());
        for (r, node) in self.iter() {
            data.push(f(r, node))
        }

        SyntaxData {
            data,
            token: self.nodes.token(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxData<T> {
    data: Vec<T>,
    token: IdVecToken<Ast>,
}

impl<T> SyntaxData<T> {
    pub fn get(&self, node: SyntaxNodeRef) -> &T {
        assert!(self.token.owns(node.clone()));
        &self.data[node.inner()]
    }
}
