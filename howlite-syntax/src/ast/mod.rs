use crate::{gen_node_impls, tree::NodeId};

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
    DefImport(DefImport),
    Block(Block<A>),
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

gen_node_impls!(Repaired { &tree?, });

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub field: Span,
    pub lhs: NodeId<AstNode>,
}
gen_node_impls!(FieldAccess { field, &lhs, });

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess {
    pub lhs: NodeId<AstNode>,
    pub index: NodeId<AstNode>,
}
gen_node_impls!(ArrayAccess { &lhs, &index, });

#[derive(Debug, Clone)]
pub struct ExprCall<A: Allocator> {
    pub callee: NodeId<AstNode>,
    pub ty_params: Vec<NodeId<AstNode>, A>,
    pub params: Vec<NodeId<AstNode>, A>,
}
gen_node_impls!(ExprCall<A> { &callee, &ty_params*, &params*, });

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLet {
    pub name: Span,
    pub ty: NodeId<AstNode>,
    pub mutable: bool,
    pub value: NodeId<AstNode>,
}
gen_node_impls!(ExprLet { name, &ty, mutable, &value, });

#[derive(Debug, Clone)]
pub struct Program<A: Allocator> {
    pub definitions: Vec<NodeId<AstNode>, A>,
}
gen_node_impls!(Program<A> { &definitions*, });

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIf {
    pub condition: NodeId<AstNode>,
    pub success: NodeId<AstNode>,
    pub failure: Option<NodeId<AstNode>>,
}
gen_node_impls!(ExprIf { &condition, &success, &failure?, });

#[derive(Debug, Clone, PartialEq)]
pub struct Block<A: Allocator> {
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
    pub statements: Vec<NodeId<AstNode>, A>,
}

gen_node_impls!(Block<A> { returns, &statements*, });

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub symbol: Span,
}
gen_node_impls!(Ident { symbol });

#[derive(Debug, Clone, PartialEq)]
pub struct ExprWhile {
    pub condition: NodeId<AstNode>,
    pub body: NodeId<AstNode>,
}
gen_node_impls!(ExprWhile { &condition, &body });

#[derive(Debug, Clone, PartialEq)]
pub struct ExprTypeConstruction {
    // TODO: rename this "Assume Operator" or "Type Judgement" or something
    pub ty: NodeId<AstNode>,
    pub value: NodeId<AstNode>,
}
gen_node_impls!(ExprTypeConstruction { &ty, &value });

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
    Block(Block<A>),
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
    DefImport(DefImport),
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

#[macro_export]
macro_rules! gen_node_impls {
    /* #region gen_node_impls - Destructure Self */
    (@field_unwrap $rhs:expr => ($($unwrapped:tt)*) &$field:ident*, $($rest:tt)*  ) => {
        gen_node_impls!(@field_unwrap $rhs => ($($unwrapped)* $field,) $($rest)*)
    };
    (@field_unwrap $rhs:expr => ($($unwrapped:tt)*) &$field:ident?, $($rest:tt)*  ) => {
        gen_node_impls!(@field_unwrap $rhs => ($($unwrapped)* $field,) $($rest)*)
    };
    (@field_unwrap $rhs:expr => ($($unwrapped:tt)*) &$field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@field_unwrap $rhs => ($($unwrapped)* $field,) $($rest)*)
    };
    (@field_unwrap $rhs:expr => ($($unwrapped:tt)*) $field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@field_unwrap $rhs => ($($unwrapped)* $field,) $($rest)*)
    };
    (@field_unwrap $rhs:expr => ($($unwrapped:tt)*) $($field:tt)+) => {
        gen_node_impls!(@field_unwrap $rhs => ($($unwrapped)*) $($field)*,)
    };
    (@field_unwrap $rhs:expr => ($($unwrapped:tt)*) ) => {
        #[allow(unused, reason = "we only destructure here to ensure all fields have been accounted for, some may be unused for various reasons")]
        let Self { $($unwrapped)* } = $rhs;
    };
    /* #endregion */


    /* #region gen_node_impls - Generate Child Iterators */


    (@gen_iters ($($iters:expr);*) &$field:ident*, $($rest:tt)* ) => {
        gen_node_impls!(@gen_iters ($($iters);*; $field.iter().copied()) $($rest)*)
    };
    (@gen_iters ($($iters:expr);*) &$field:ident?, $($rest:tt)*  ) => {
        gen_node_impls!(@gen_iters ($($iters);* ; $field.into_iter().copied()) $($rest)*)
    };
    (@gen_iters ($($iters:expr);*) &$field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@gen_iters ($($iters);* ; std::iter::once(*$field)) $($rest)*)
    };
    (@gen_iters ($($iters:expr);*) $field:ident, $($rest:tt)*  ) => {
        // ignore non-ref fields
        gen_node_impls!(@gen_iters ($($iters);*) $($rest)*)
    };
    (@gen_iters ($($iters:expr);*) $($rest:tt)+  ) => {
        gen_node_impls!(@gen_iters ($($iters);*) $($rest)*,)
    };
    (@gen_iters ($($iters:expr);*) ) => {
        gen_node_impls!(@chain_iters $($iters);* )
    };
    /* #endregion */

    /* #region gen_node_impls - Chain a series of iterators */
    (@chain_iters $_root:expr; $chain:expr; $next:expr; $($rest:expr);+ ) => {
        gen_node_impls!(@chain_iters $_root; $chain.chain($next); $($rest);+)
    };
    (@chain_iters $_root:expr; $chain:expr; $last:expr $(;)? ) => {
        $chain.chain($last)
    };
    (@chain_iters $_root:expr; $chain:expr $(;)? ) => {
        $chain
    };
    /* #endregion */


    /* #region gen_node_impls - Generate Local Comparison */
    (@gen_cmp $other:ident ($bools:expr) &$field:ident*, $($rest:tt)* ) => {
        gen_node_impls!(@gen_cmp $other ($bools && $field.len() == $other.$field.len()) $($rest)*)
    };
    (@gen_cmp $other:ident ($bools:expr) &$field:ident?, $($rest:tt)* ) => {
        gen_node_impls!(@gen_cmp $other ($bools && $field.is_some() == $other.$field.is_some()) $($rest)*)
    };
    (@gen_cmp $other:ident ($bools:expr) &$field:ident, $($rest:tt)* ) => {
        // ignore children which are required, without a reference resolver these are meaningless
        gen_node_impls!(@gen_cmp $other ($bools) $($rest)*)
    };
    (@gen_cmp $other:ident ($bools:expr) $field:ident, $($rest:tt)* ) => {
        gen_node_impls!(@gen_cmp $other ($bools && $field == &$other.$field) $($rest)*)
    };
    (@gen_cmp $other:ident ($bools:expr) $($rest:tt)+ ) => {
        gen_node_impls!(@gen_cmp $other ($bools) $($rest)*,)
    };
    (@gen_cmp $other:ident ($bools:expr)) => {
        $bools
    };
    /* #endregion */


    (@impl_children { $($field:ident),* $(,)? }) => {
        fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
            std::iter::empty()
        }
    };

    (@impl_children { $($field:tt)* }) => {
        fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
            gen_node_impls!(@field_unwrap self => () $($field)*);
            gen_node_impls!(@gen_iters (()) $($field)*)
        }
    };

    // maybe a special marker trait when they just wrap references?
    (@impl_local_eq { $(&$field:ident),* $(,)? }) => {
        fn local_eq(&self, _: &Self) -> bool {
            true
        }
    };

    (@impl_local_eq { $($field:tt)* }) => {
        fn local_eq(&self, other: &Self) -> bool {
            gen_node_impls!(@field_unwrap self => () $($field)*);
            gen_node_impls!(@gen_cmp other (true) $($field)*)
        }
    };

    ($t:ident<A> { $($field:tt)*  }) => {
        impl<A: Allocator> $crate::TreeChildren<AstNode> for $t<A> {
            gen_node_impls!(@impl_children { $($field)* });
        }

        impl<A: Allocator> $crate::NodeLocalEquality for $t<A> {
            gen_node_impls!(@impl_local_eq { $($field)* });
        }
    };

    ($t:ident { $($field:tt)*  }) => {
        impl $crate::TreeChildren<AstNode> for $t {
            gen_node_impls!(@impl_children { $($field)* });
        }

        impl $crate::NodeLocalEquality for $t {
            gen_node_impls!(@impl_local_eq { $($field)* });
        }
    };


}
