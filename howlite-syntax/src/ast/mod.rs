use crate::{gen_node_impls, tree::NodeId, NodeLocalEquality, TreeChildren};

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
    ExprWhile(ExprWhile),

    DefType(DefType<A>),
    DefExternFunc(DefExternFunc<A>),
    DefExternVar(DefExternVar),

    Program(Program<A>),

    // Types
    TyRef(TyRef),
    TyExprUnion(TyExprUnion),
    TyStruct(TyStruct<A>),
    TyStructMember(TyStructMember),
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

macro_rules! fold_tree_impl {
    (@method $fname:ident($node:ident<L>)) => {
        fn $fname(&self, id: NodeId<AstNode>, node: &$node) -> Self::Value;
    };
    (@method $fname:ident($node:ident<A>)) => {
        fn $fname<A: allocator_api2::alloc::Allocator>(&self, id: NodeId<AstNode>, node: &$node<A>, children: paste::paste!([<$node Children>]<Self::Value>)) -> Self::Value;
    };
    (@method $fname:ident($node:ident)) => {
        fn $fname(&self, id: NodeId<AstNode>, node: &$node, children: paste::paste!([<$node Children>]<Self::Value>)) -> Self::Value;
    };

    (@fold ($n:ident, $tree:ident, $root:ident, $fold:ident) $fname:ident$(<A>)?) => {{
        let __children = $n.map_children(|__child_node_id| {
            fold_tree_recursive($tree, __child_node_id, $fold)
        });
        $fold.$fname($root, $n, __children)
    }};
    (@fold ($n:ident, $tree:ident, $root:ident, $fold:ident) $fname:ident<L>) => { $fold.$fname($root, $n) };

    ($($fname:ident($node:ident $(<$generic:ident>)?));+) => {
        pub trait FoldTree {
            type Value;
            $(fold_tree_impl!(@method $fname($node $(<$generic>)?));)+
        }

        pub fn fold_tree_recursive<A: Allocator, Fold: FoldTree<Value = Value>, Value>(tree: &$crate::Tree<AstNode, A>, root: NodeId<AstNode>, fold: &Fold) -> Value {
            let __node = tree.get(root);
            match &__node.data {
                $(
                    AstNodeData::$node(n) => {
                        fold_tree_impl!(@fold (n, tree, root, fold) $fname $(<$generic>)?)
                    }
                )+
            }
        }
    }
}

fold_tree_impl! {
    literal_integer(LiteralInteger<L>);
    literal_char(LiteralChar<L>);
    literal_string(LiteralString<L>);
    literal_array(LiteralArray<A>);
    literal_struct(LiteralStruct<A>);
    literal_struct_member(LiteralStructMember);

    ident(Ident<L>);
    field_access(FieldAccess);
    array_access(ArrayAccess);

    repaired(Repaired);

    def_func(DefFunc<A>);
    def_param(DefParam);
    def_import(DefImport<L>);
    block(Block<A>);
    expr_if(ExprIf);
    expr_call(ExprCall<A>);
    expr_infix(ExprInfix);
    expr_prefix(ExprPrefix);
    expr_type_construction(ExprTypeConstruction);

    expr_let(ExprLet);
    expr_while(ExprWhile);

    def_type(DefType<A>);
    def_extern_func(DefExternFunc<A>);
    def_extern_var(DefExternVar);

    program(Program<A>);

    // Types
    ty_ref(TyRef);
    ty_expr_union(TyExprUnion);
    ty_struct(TyStruct<A>);
    ty_struct_member(TyStructMember);
    ty_number_range(TyNumberRange);
    ty_array(TyArray);
    ty_unit(TyUnit<L>);
    ty_param(TyParam);
    ty_slice(TySlice);
    ty_named(TyNamed<A>)
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
    Block(Block<A>),
    ExprIf(ExprIf),
    ExprCall(ExprCall<A>),
    ExprInfix(ExprInfix),
    ExprPrefix(ExprPrefix),
    LiteralStruct(LiteralStruct<A>),
    ExprLet(ExprLet),
    ExprWhile(ExprWhile),
    DefType(DefType<A>),
    DefExternFunc(DefExternFunc<A>),
    Program(Program<A>),
    TyRef(TyRef),
    TyStruct(TyStruct<A>),
    LiteralStructMember(LiteralStructMember),
    ExprTypeConstruction(ExprTypeConstruction),
    TyStructMember(TyStructMember),
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
            (Self::ExprWhile($p0), Self::ExprWhile($p1)) => { $($action);* }
            (Self::DefType($p0), Self::DefType($p1)) => { $($action);* }
            (Self::DefExternFunc($p0), Self::DefExternFunc($p1)) => { $($action);* }
            (Self::DefExternVar($p0), Self::DefExternVar($p1)) => { $($action);* }
            (Self::Program($p0), Self::Program($p1)) => { $($action);* }
            (Self::TyRef($p0), Self::TyRef($p1)) => { $($action);* }
            (Self::TyExprUnion($p0), Self::TyExprUnion($p1)) => { $($action);* }
            (Self::TyStruct($p0), Self::TyStruct($p1)) => { $($action);* }
            (Self::TyStructMember($p0), Self::TyStructMember($p1)) => { $($action);* }
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

macro_rules! on_any_node_variant {
    ($node:expr => $p0:ident { $($action:expr);* }) => {
        match ($node) {
            Self::LiteralInteger($p0) => { $($action);* }
            Self::LiteralChar($p0) => { $($action);* }
            Self::LiteralString($p0) => { $($action);* }
            Self::LiteralArray($p0) => { $($action);* }
            Self::LiteralStruct($p0) => { $($action);* }
            Self::LiteralStructMember($p0) => { $($action);* }
            Self::Ident($p0) => { $($action);* }
            Self::FieldAccess($p0) => { $($action);* }
            Self::ArrayAccess($p0) => { $($action);* }
            Self::Repaired($p0) => { $($action);* }
            Self::DefFunc($p0) => { $($action);* }
            Self::DefParam($p0) => { $($action);* }
            Self::DefImport($p0) => { $($action);* }
            Self::Block($p0) => { $($action);* }
            Self::ExprIf($p0) => { $($action);* }
            Self::ExprCall($p0) => { $($action);* }
            Self::ExprInfix($p0) => { $($action);* }
            Self::ExprPrefix($p0) => { $($action);* }
            Self::ExprTypeConstruction($p0) => { $($action);* }
            Self::ExprLet($p0) => { $($action);* }
            Self::ExprWhile($p0) => { $($action);* }
            Self::DefType($p0) => { $($action);* }
            Self::DefExternFunc($p0) => { $($action);* }
            Self::DefExternVar($p0) => { $($action);* }
            Self::Program($p0) => { $($action);* }
            Self::TyRef($p0) => { $($action);* }
            Self::TyExprUnion($p0) => { $($action);* }
            Self::TyStruct($p0) => { $($action);* }
            Self::TyStructMember($p0) => { $($action);* }
            Self::TyNumberRange($p0) => { $($action);* }
            Self::TyArray($p0) => { $($action);* }
            Self::TyUnit($p0) => { $($action);* }
            Self::TyParam($p0) => { $($action);* }
            Self::TySlice($p0) => { $($action);* }
            Self::TyNamed($p0) => { $($action);* }
        }
    };
}

impl TreeChildren<AstNode> for AstNodeData {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        let boxed_iter: Box<dyn Iterator<Item = NodeId<AstNode>>> =
            on_any_node_variant!(self => data  { Box::new(data.children()) });
        boxed_iter
    }
}

impl TreeChildren<AstNode> for AstNode {
    fn children(&self) -> impl Iterator<Item = NodeId<AstNode>> {
        self.data.children()
    }
}

impl NodeLocalEquality for AstNodeData {
    fn local_eq(&self, other: &Self) -> bool {
        on_ast_node_pair!((self => lhs, other => rhs)  { lhs.local_eq(rhs) } else { false })
    }
}

impl NodeLocalEquality for AstNode {
    fn local_eq(&self, other: &Self) -> bool {
        other.span == self.span && self.data.local_eq(&other.data)
    }
}

pub trait HigherOrderNode {
    type Child;
    type MappedChildren<U>;

    fn map_children<F: Fn(Self::Child) -> U, U>(&self, op: F) -> Self::MappedChildren<U>;
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

    /* #region gen_node_impls - Children Only Structure */
    (@child_struct $name:ident $t:ident => ($($unwrapped:tt)*) &$field:ident*, $($rest:tt)*  ) => {
        gen_node_impls!(@child_struct $name $t => ($($unwrapped)* pub $field: Vec<$t>,) $($rest)*);
    };
    (@child_struct $name:ident $t:ident => ($($unwrapped:tt)*) &$field:ident?, $($rest:tt)*  ) => {
        gen_node_impls!(@child_struct $name $t => ($($unwrapped)* pub $field: Option<$t>,) $($rest)*);
    };
    (@child_struct $name:ident $t:ident => ($($unwrapped:tt)*) &$field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@child_struct $name $t => ($($unwrapped)* pub $field: $t,) $($rest)*);
    };
    (@child_struct $name:ident $t:ident => ($($unwrapped:tt)*) $field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@child_struct $name $t => ($($unwrapped)*) $($rest)*);
    };
    (@child_struct $name:ident $t:ident => ($($unwrapped:tt)*) $($field:tt)+) => {
        gen_node_impls!(@child_struct $name $t => ($($unwrapped)*) $($field)*,);
    };
    (@child_struct $name:ident $t:ident => ($($unwrapped:tt)*) ) => {
        pub struct $name<$t> {
            $($unwrapped)*
        }
    };
    /* #endregion */

    /* #region gen_node_impls - Map Child Struct */
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) &$field:ident*, $($rest:tt)*  ) => {
        gen_node_impls!(@map_children $name $t, $mapper => ($($unwrapped)* $field: $name.$field.iter().copied().map(&$mapper).collect(),) $($rest)*)
    };
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) &$field:ident?, $($rest:tt)*  ) => {
        gen_node_impls!(@map_children $name $t, $mapper => ($($unwrapped)* $field: $name.$field.map(&$mapper),) $($rest)*)
    };
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) &$field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@map_children $name $t, $mapper => ($($unwrapped)* $field: $mapper($name.$field),) $($rest)*)
    };
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) $field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@map_children $name $t, $mapper => ($($unwrapped)*) $($rest)*)
    };
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) $($field:tt)+) => {
        gen_node_impls!(@map_children $name $t, $mapper => ($($unwrapped)*) $($field)*,)
    };
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) ) => {
        $t {
            $($unwrapped)*
        }
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


    (@impl_map_children $t:ident { $($field:ident),* $(,)? }) => {
        impl $crate::ast::HigherOrderNode for $t {
            type Child = NodeId<AstNode>;
            type MappedChildren<_U> = ();

            fn map_children<F: Fn(Self::Child) -> U, U>(&self, _op: F) -> () {}
        }
    };

    (@impl_map_children $t:ident { $($field:tt)* }) => {
        paste::paste! {
            gen_node_impls!(@child_struct [<$t Children>] T => () $($field)*);

            impl $crate::ast::HigherOrderNode for $t {
                type Child = NodeId<AstNode>;
                type MappedChildren<U> = [<$t Children>]<U>;

                fn map_children<F: Fn(Self::Child) -> U, U>(&self, __op: F) -> Self::MappedChildren<U> {
                    gen_node_impls!(@map_children self [<$t Children>], __op => () $($field)*)
                }
            }
        }
    };

    (@impl_map_children $t:ident<A> { $($field:tt)* }) => {
        paste::paste! {
            gen_node_impls!(@child_struct [<$t Children>] T => () $($field)*);

            impl<A: Allocator> $crate::ast::HigherOrderNode for $t<A> {
                type Child = NodeId<AstNode>;
                type MappedChildren<U> = [<$t Children>]<U>;

                fn map_children<F: Fn(Self::Child) -> U, U>(&self, __op: F) -> Self::MappedChildren<U> {
                    gen_node_impls!(@map_children self [<$t Children>], __op => () $($field)*)
                }
            }
        }
    };



    ($t:ident<A> { $($field:tt)*  }) => {
        impl<A: Allocator> $crate::TreeChildren<AstNode> for $t<A> {
            gen_node_impls!(@impl_children { $($field)* });
        }

        impl<A: Allocator> $crate::NodeLocalEquality for $t<A> {
            gen_node_impls!(@impl_local_eq { $($field)* });
        }
        gen_node_impls!(@impl_map_children $t<A> { $($field)* });
    };

    ($t:ident { $($field:tt)*  }) => {
        impl $crate::TreeChildren<AstNode> for $t {
            gen_node_impls!(@impl_children { $($field)* });
        }

        impl $crate::NodeLocalEquality for $t {
            gen_node_impls!(@impl_local_eq { $($field)* });
        }

        gen_node_impls!(@impl_map_children $t { $($field)* });
    };


}
