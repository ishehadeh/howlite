use crate::{
    span::{SourceSpan, Spanned},
    util::idvec::{Id, IdVec, IdVecToken},
};

use std::fmt::Debug;

// temp use until stabalized in std:
use concat_idents::concat_idents;

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
    pub typ: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyParam<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub super_ty: SyntaxNodeRef,

    pub default_ty: Option<SyntaxNodeRef>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyArray<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub element_ty: SyntaxNodeRef,
    pub length: u32,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructMember<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub mutable: bool,
    pub name: String,
    pub ty: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyRef<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub parameters: Vec<SyntaxNodeRef>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TyStruct<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub members: Vec<SyntaxNodeRef>,
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
    Param(Param<X>),
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

    // Types
    TyRef(TyRef<X>),
    TyStruct(TyStruct<X>),
    StructLiteralMember(StructLiteralMember<X>),
    StructMember(StructMember<X>),
    TyNumberRange(TyNumberRange<X>),
    TyArray(TyArray<X>),
    TyBool(TyBool<X>),
    TyUnit(TyUnit<X>),
    TyParam(TyParam<X>),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Repaired<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub tree: Option<SyntaxNodeRef>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub field: Ident,
    pub object: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub index: SyntaxNodeRef,
    pub object: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Expr<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub lhs: SyntaxNodeRef,
    pub op: InfixOp,
    pub rhs: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub function_name: String,
    pub paramaters: Vec<SyntaxNodeRef>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtLet<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub ty: SyntaxNodeRef,
    pub mutable: bool,
    pub value: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Program<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub definitions: Vec<SyntaxNodeRef>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefType<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub ty: SyntaxNodeRef,
    pub ty_params: Vec<SyntaxNodeRef>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefExtern<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub params: Vec<SyntaxNodeRef>,
    pub return_ty: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtIf<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub condition: SyntaxNodeRef,
    pub body: SyntaxNodeRef,
    pub else_: Option<SyntaxNodeRef>,
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
    pub statements: Vec<SyntaxNodeRef>,
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
    pub params: Vec<SyntaxNodeRef>,
    pub return_ty: SyntaxNodeRef,
    pub body: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralMember<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub field: Ident,
    pub value: SyntaxNodeRef,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub members: Vec<SyntaxNodeRef>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralArray<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub values: Vec<SyntaxNodeRef>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtWhile<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub condition: SyntaxNodeRef,
    pub body: SyntaxNodeRef,
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
    ($ast_ty:ident : $($member:ident ($member_ty:ident) [$member_snake:ident]),*) => {
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

        $(
            impl<X: Debug + Clone> TryInto<$member_ty<X>> for $ast_ty<X> {
                type Error = ();

                fn try_into(self) -> Result<$member_ty<X>, Self::Error> {
                    match self {
                        $ast_ty::$member(a) => Ok(a),
                        _ => Err(())
                    }
                }
            }
        )*

        impl<X: Debug + Clone> $ast_ty<X> {
            $(  concat_idents!(fn_name = as_, $member_snake
                    {
                        pub fn fn_name(&self) -> Option<&$member_ty<X>> {
                            match self {
                                $ast_ty::$member(a) => Some(a),
                                _ => None
                            }
                        }
                    });

                concat_idents!(fn_name = into_, $member_snake
                    {
                        pub fn fn_name(self) -> Option<$member_ty<X>> {
                            match self {
                                $ast_ty::$member(a) => Some(a),
                                _ => None
                            }
                        }
                    });
            )*
        }
    };
    ($ast_ty:ident : $($member:ident),*) => {
        impl_ast_traits!($ast_ty : $($member ($member) [$member]),*);
    };
}

impl_ast_traits!(Ast :
    Program(Program) [program],
    Expr(Expr) [expr],
    DefType(DefType) [def_type],
    DefExtern(DefExtern) [def_extern],
    ExprCall(ExprCall) [expr_call],
    StmtIf(StmtIf) [stmt_if],
    StmtLet(StmtLet) [stmt_let],
    Block(Block) [block],
    DefFunction(DefFunction) [def_function],
    Ident(Ident) [ident],
    LiteralInteger(LiteralInteger) [literal_integer],
    LiteralBool(LiteralBool) [literal_bool],
    Repaired(Repaired) [repaired],
    FieldAccess(FieldAccess) [field_access],
    StructLiteral(StructLiteral) [struct_literal],
    LiteralArray(LiteralArray) [literal_array],
    ArrayAccess(ArrayAccess) [array_access],
    StmtWhile(StmtWhile) [stmt_while],
    TyRef(TyRef) [ty_ref],
    TyStruct(TyStruct) [ty_struct],
    TyArray(TyArray) [ty_array],
    StructMember(StructMember) [struct_member],
    TyNumberRange(TyNumberRange) [ty_number_range],
    TyBool(TyBool) [ty_bool],
    TyUnit(TyUnit) [ty_unit],
    TyParam(TyParam) [ty_param],
    StructLiteralMember(StructLiteralMember) [struct_literal_member],
    Param(Param) [param]
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
        F: Fn(&SyntaxData<T>, SyntaxNodeRef, &Ast) -> T,
    {
        let mut data = SyntaxData {
            data: Vec::with_capacity(self.nodes.len()),
            token: self.nodes.token(),
        };

        for (r, node) in self.iter() {
            let val = f(&data, r, node);
            data.data.push(val);
        }

        data
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
