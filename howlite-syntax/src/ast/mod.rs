use crate::{gen_node_impls, tree::DefaultLinearTreeId, NodeLocalEquality};

use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};
use lrpar::Span;
use smol_str::SmolStr;

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
pub enum AstNodeData<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    LiteralInteger(LiteralInteger),
    LiteralChar(LiteralChar),
    LiteralString(LiteralString),
    LiteralArray(LiteralArray<ChildT, A>),
    LiteralStruct(LiteralStruct<ChildT, A>),
    LiteralStructMember(LiteralStructMember<ChildT>),

    Ident(Ident),
    FieldAccess(FieldAccess<ChildT>),
    ArrayAccess(ArrayAccess<ChildT>),

    Repaired(Repaired<ChildT>),

    DefFunc(DefFunc<ChildT, A>),
    DefParam(DefParam<ChildT>),
    DefImport(DefImport),
    Block(Block<ChildT, A>),
    ExprIf(ExprIf<ChildT>),
    ExprCall(ExprCall<ChildT, A>),
    ExprInfix(ExprInfix<ChildT>),
    ExprPrefix(ExprPrefix<ChildT>),
    ExprTypeConstruction(ExprTypeConstruction<ChildT>),

    ExprLet(ExprLet<ChildT>),
    ExprWhile(ExprWhile<ChildT>),

    DefType(DefType<ChildT, A>),
    DefExternFunc(DefExternFunc<ChildT, A>),
    DefExternVar(DefExternVar<ChildT>),

    Program(Program<ChildT, A>),

    // Types
    TyRef(TyRef<ChildT>),
    TyExprUnion(TyExprUnion<ChildT>),
    TyStruct(TyStruct<ChildT, A>),
    TyStructMember(TyStructMember<ChildT>),
    TyNumberRange(TyNumberRange<ChildT>),
    TyArray(TyArray<ChildT>),
    TyUnit(TyUnit),
    TyParam(TyParam<ChildT>),
    TySlice(TySlice<ChildT>),
    TyNamed(TyNamed<ChildT, A>),
}

#[derive(Debug, Clone)]
pub struct AstNode<Data = AstNodeData<DefaultLinearTreeId>> {
    pub span: Span,
    pub data: Data,
}

impl<ChildT, A: Allocator> AstNode<AstNodeData<ChildT, A>> {
    pub fn new<S: Into<Span>, T: Into<AstNodeData<ChildT, A>>>(span: S, data: T) -> Self {
        AstNode {
            span: span.into(),
            data: data.into(),
        }
    }
}

impl<T> AstNode<T> {
    pub fn new_narrow<S: Into<Span>>(span: S, data: T) -> Self {
        AstNode {
            span: span.into(),
            data,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Repaired<ChildT = DefaultLinearTreeId> {
    pub tree: Option<ChildT>,
}

gen_node_impls!(Repaired { &tree?, });

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess<ChildT = DefaultLinearTreeId> {
    pub field: SmolStr,
    pub lhs: ChildT,
}
gen_node_impls!(FieldAccess { field, &lhs, });

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayAccess<ChildT = DefaultLinearTreeId> {
    pub lhs: ChildT,
    pub index: ChildT,
}
gen_node_impls!(ArrayAccess { &lhs, &index, });

#[derive(Debug, Clone)]
pub struct ExprCall<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub callee: ChildT,
    pub ty_params: Vec<ChildT, A>,
    pub params: Vec<ChildT, A>,
}
gen_node_impls!(ExprCall<A> { &callee, &ty_params*, &params*, });

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLet<ChildT = DefaultLinearTreeId> {
    pub name: SmolStr,
    pub ty: ChildT,
    pub mutable: bool,
    pub value: ChildT,
}
gen_node_impls!(ExprLet { name, &ty, mutable, &value, });

#[derive(Debug, Clone)]
pub struct Program<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
    pub definitions: Vec<ChildT, A>,
}
gen_node_impls!(Program<A> { &definitions*, });

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIf<ChildT = DefaultLinearTreeId> {
    pub condition: ChildT,
    pub success: ChildT,
    pub failure: Option<ChildT>,
}
gen_node_impls!(ExprIf { &condition, &success, &failure?, });

#[derive(Debug, Clone, PartialEq)]
pub struct Block<ChildT = DefaultLinearTreeId, A: Allocator = Global> {
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
    pub statements: Vec<ChildT, A>,
}

gen_node_impls!(Block<A> { returns, &statements*, });

#[derive(Debug, Clone)]
pub struct Ident {
    pub symbol: SmolStr,
}
gen_node_impls!(Ident { symbol });

#[derive(Debug, Clone)]
pub struct ExprWhile<ChildT = DefaultLinearTreeId> {
    pub condition: ChildT,
    pub body: ChildT,
}
gen_node_impls!(ExprWhile { &condition, &body });

#[derive(Debug, Clone)]
pub struct ExprTypeConstruction<ChildT = DefaultLinearTreeId> {
    // TODO: rename this "Assume Operator" or "Type Judgement" or something
    pub ty: ChildT,
    pub value: ChildT,
}
gen_node_impls!(ExprTypeConstruction { &ty, &value });

macro_rules! on_ast_node_pair {
    (($lhs:expr => $p0:ident, $rhs:expr => $p1:ident) $action:block else $otherwise:expr) => {
        match ($lhs, $rhs) {
            (Self::LiteralInteger($p0), Self::LiteralInteger($p1)) => $action,
            (Self::LiteralChar($p0), Self::LiteralChar($p1)) => $action,
            (Self::LiteralString($p0), Self::LiteralString($p1)) => $action,
            (Self::LiteralArray($p0), Self::LiteralArray($p1)) => $action,
            (Self::LiteralStruct($p0), Self::LiteralStruct($p1)) => $action,
            (Self::LiteralStructMember($p0), Self::LiteralStructMember($p1)) => $action,
            (Self::Ident($p0), Self::Ident($p1)) => $action,
            (Self::FieldAccess($p0), Self::FieldAccess($p1)) => $action,
            (Self::ArrayAccess($p0), Self::ArrayAccess($p1)) => $action,
            (Self::Repaired($p0), Self::Repaired($p1)) => $action,
            (Self::DefFunc($p0), Self::DefFunc($p1)) => $action,
            (Self::DefParam($p0), Self::DefParam($p1)) => $action,
            (Self::DefImport($p0), Self::DefImport($p1)) => $action,
            (Self::Block($p0), Self::Block($p1)) => $action,
            (Self::ExprIf($p0), Self::ExprIf($p1)) => $action,
            (Self::ExprCall($p0), Self::ExprCall($p1)) => $action,
            (Self::ExprInfix($p0), Self::ExprInfix($p1)) => $action,
            (Self::ExprPrefix($p0), Self::ExprPrefix($p1)) => $action,
            (Self::ExprTypeConstruction($p0), Self::ExprTypeConstruction($p1)) => $action,
            (Self::ExprLet($p0), Self::ExprLet($p1)) => $action,
            (Self::ExprWhile($p0), Self::ExprWhile($p1)) => $action,
            (Self::DefType($p0), Self::DefType($p1)) => $action,
            (Self::DefExternFunc($p0), Self::DefExternFunc($p1)) => $action,
            (Self::DefExternVar($p0), Self::DefExternVar($p1)) => $action,
            (Self::Program($p0), Self::Program($p1)) => $action,
            (Self::TyRef($p0), Self::TyRef($p1)) => $action,
            (Self::TyExprUnion($p0), Self::TyExprUnion($p1)) => $action,
            (Self::TyStruct($p0), Self::TyStruct($p1)) => $action,
            (Self::TyStructMember($p0), Self::TyStructMember($p1)) => $action,
            (Self::TyNumberRange($p0), Self::TyNumberRange($p1)) => $action,
            (Self::TyArray($p0), Self::TyArray($p1)) => $action,
            (Self::TyUnit($p0), Self::TyUnit($p1)) => $action,
            (Self::TyParam($p0), Self::TyParam($p1)) => $action,
            (Self::TySlice($p0), Self::TySlice($p1)) => $action,
            (Self::TyNamed($p0), Self::TyNamed($p1)) => $action,
            _ => $otherwise,
        }
    };
}

macro_rules! on_any_node_variant {
    ($node:expr => $p0:ident $action:block ) => {
        match ($node) {
            Self::LiteralInteger($p0) => $action,
            Self::LiteralChar($p0) => $action,
            Self::LiteralString($p0) => $action,
            Self::LiteralArray($p0) => $action,
            Self::LiteralStruct($p0) => $action,
            Self::LiteralStructMember($p0) => $action,
            Self::Ident($p0) => $action,
            Self::FieldAccess($p0) => $action,
            Self::ArrayAccess($p0) => $action,
            Self::Repaired($p0) => $action,
            Self::DefFunc($p0) => $action,
            Self::DefParam($p0) => $action,
            Self::DefImport($p0) => $action,
            Self::Block($p0) => $action,
            Self::ExprIf($p0) => $action,
            Self::ExprCall($p0) => $action,
            Self::ExprInfix($p0) => $action,
            Self::ExprPrefix($p0) => $action,
            Self::ExprTypeConstruction($p0) => $action,
            Self::ExprLet($p0) => $action,
            Self::ExprWhile($p0) => $action,
            Self::DefType($p0) => $action,
            Self::DefExternFunc($p0) => $action,
            Self::DefExternVar($p0) => $action,
            Self::Program($p0) => $action,
            Self::TyRef($p0) => $action,
            Self::TyExprUnion($p0) => $action,
            Self::TyStruct($p0) => $action,
            Self::TyStructMember($p0) => $action,
            Self::TyNumberRange($p0) => $action,
            Self::TyArray($p0) => $action,
            Self::TyUnit($p0) => $action,
            Self::TyParam($p0) => $action,
            Self::TySlice($p0) => $action,
            Self::TyNamed($p0) => $action,
        }
    };
}

impl<ChildT> NodeLocalEquality for AstNodeData<ChildT> {
    fn local_eq(&self, other: &Self) -> bool {
        on_ast_node_pair!((self => lhs, other => rhs)  { lhs.local_eq(rhs) } else { false })
    }
}

impl<Data: NodeLocalEquality> NodeLocalEquality for AstNode<Data> {
    fn local_eq(&self, other: &Self) -> bool {
        other.span == self.span && self.data.local_eq(&other.data)
    }
}

pub trait HigherOrderNode<ChildT>
where
    for<'a> ChildT: 'a,
{
    type Mapped<T>;

    fn children(&self) -> impl Iterator<Item = &'_ ChildT>;
    fn map<F: Fn(ChildT) -> T, T>(self, op: F) -> Self::Mapped<T>;
}

#[derive(Clone, Debug)]
pub struct BoxAstNode(Box<AstNode<AstNodeData<BoxAstNode>>>);
impl BoxAstNode {
    pub fn new(span: Span, data: impl Into<AstNodeData<BoxAstNode>>) -> BoxAstNode {
        BoxAstNode(Box::new(AstNode::new(span, data)))
    }

    pub fn fold<F: Fn(AstNode<AstNodeData<V>>) -> V, V>(self, op: &F) -> V {
        op(self.map(|c| c.fold(op)))
    }

    pub fn into_inner(self) -> AstNode<AstNodeData<BoxAstNode>> {
        let BoxAstNode(inner) = self;
        *inner
    }
}

impl std::ops::Deref for BoxAstNode {
    type Target = AstNode<AstNodeData<BoxAstNode>>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl HigherOrderNode<BoxAstNode> for BoxAstNode {
    type Mapped<T> = AstNode<AstNodeData<T>>;

    fn children(&self) -> impl Iterator<Item = &BoxAstNode> {
        self.0.data.children()
    }

    fn map<F: Fn(BoxAstNode) -> T, T>(self, op: F) -> Self::Mapped<T> {
        let AstNode { span, data } = *self.0;
        AstNode::new(span, data.map(op))
    }
}

impl<ChildT, A: Allocator> HigherOrderNode<ChildT> for AstNodeData<ChildT, A>
where
    for<'a> ChildT: 'a,
{
    type Mapped<T> = AstNodeData<T, A>;

    fn children<'a>(&'a self) -> impl Iterator<Item = &'a ChildT> {
        on_any_node_variant!(self => n
        {
            let iter: Box<dyn Iterator<Item = &'a ChildT>> = Box::new(HigherOrderNode::<ChildT>::children(n));
            iter
        })
    }

    fn map<F: Fn(ChildT) -> T, T>(self, op: F) -> Self::Mapped<T> {
        on_any_node_variant!(self => n
        {
            let mapped_n = n.map(op);
            mapped_n.into()
        })
    }
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


    /* #region gen_node_impls - Map Child Struct */
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) &$field:ident*, $($rest:tt)*  ) => {
        gen_node_impls!{
            @map_children $name $t, $mapper  => ($($unwrapped)*
                $field: {
                    let ( ptr, len, cap, alloc) = $name.$field.into_raw_parts_with_alloc();
                    if let Some(ptr) = std::ptr::NonNull::new(ptr) {
                        let mut curr_elem_ptr = ptr;
                        let layout = allocator_api2::alloc::Layout::from_size_align(cap, std::mem::align_of_val(unsafe { ptr.as_ref() } ));
                        let mut new_data = allocator_api2::vec::Vec::with_capacity_in(len, alloc);
                        for _ in 0..len {
                            new_data.push($mapper(unsafe { curr_elem_ptr.read() }));
                            curr_elem_ptr = unsafe { curr_elem_ptr.add(1) }
                        }
                        unsafe { new_data.allocator().deallocate(ptr.cast::<u8>(), layout.expect("failed to construct layout")) };
                        new_data
                    } else {
                        // TODO: warn here
                        allocator_api2::vec::Vec::new_in(alloc)
                    }
                },)
            $($rest)*
        }
    };
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) &$field:ident?, $($rest:tt)*  ) => {
        gen_node_impls!(@map_children $name $t, $mapper => ($($unwrapped)* $field: $name.$field.map(&$mapper),) $($rest)*)
    };
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) &$field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@map_children $name $t, $mapper => ($($unwrapped)* $field: $mapper($name.$field),) $($rest)*)
    };
    (@map_children $name:ident $t:ident, $mapper:ident => ($($unwrapped:tt)*) $field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@map_children $name $t, $mapper => ($($unwrapped)* $field: $name.$field,) $($rest)*)
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
        gen_node_impls!(@gen_iters ($($iters);*; $field.iter()) $($rest)*)
    };
    (@gen_iters ($($iters:expr);*) &$field:ident?, $($rest:tt)*  ) => {
        gen_node_impls!(@gen_iters ($($iters);* ; $field.iter()) $($rest)*)
    };
    (@gen_iters ($($iters:expr);*) &$field:ident, $($rest:tt)*  ) => {
        gen_node_impls!(@gen_iters ($($iters);* ; std::iter::once($field)) $($rest)*)
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

    (@impl_children { $($field:tt)* }) => {
        fn children(&self) -> impl Iterator<Item = &ChildT> {
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


    (@impl_traits $t:ident { $($field:ident),* $(,)? }) => {
        impl<ChildT> $crate::ast::HigherOrderNode<ChildT> for $t where for<'a> ChildT: 'a {
            type Mapped<_T> = Self;

            fn children(&self) -> impl Iterator<Item = &ChildT> {
                std::iter::empty()
            }
            fn map<F: Fn(ChildT) -> U, U>(self, _op: F) -> Self { self }
        }

        impl $crate::NodeLocalEquality for $t {
            gen_node_impls!(@impl_local_eq { $($field)* });
        }

        impl std::cmp::PartialEq<Self> for $t {
            fn eq(&self, other: &Self) -> bool { $crate::NodeLocalEquality::local_eq(self, other) }
        }

        impl Eq for $t { }

        impl<ChildT, A: Allocator> Into<$crate::ast::AstNodeData<ChildT, A>> for $t  {
            fn into(self) -> $crate::ast::AstNodeData<ChildT, A> {
                $crate::ast::AstNodeData::$t(self)
            }
        }
    };

    (@impl_traits $t:ident { $($field:tt)* }) => {
        impl<ChildT> $crate::ast::HigherOrderNode<ChildT> for $t<ChildT> where for<'a> ChildT: 'a {
            type Mapped<U> = $t<U>;

            gen_node_impls!(@impl_children { $($field)* });
            fn map<F: Fn(ChildT) -> U, U>(self, __op: F) -> $t<U> {
                gen_node_impls!(@map_children self $t, __op => () $($field)*)
            }
        }


        impl<ChildT> $crate::NodeLocalEquality for $t<ChildT> {
            gen_node_impls!(@impl_local_eq { $($field)* });
        }

        impl<ChildT, A: allocator_api2::alloc::Allocator> Into<$crate::ast::AstNodeData<ChildT, A>> for $t<ChildT>  {
            fn into(self) -> $crate::ast::AstNodeData<ChildT, A> {
                $crate::ast::AstNodeData::$t(self)
            }
        }
    };

    (@impl_traits $t:ident<A> { $($field:tt)* }) => {
        impl<ChildT, A: allocator_api2::alloc::Allocator> $crate::ast::HigherOrderNode<ChildT> for $t<ChildT, A> where for<'a> ChildT: 'a {
            type Mapped<U> = $t<U, A>;

            gen_node_impls!(@impl_children { $($field)* });
            fn map<F: Fn(ChildT) -> U, U>(self, __op: F) -> $t<U, A> {
                gen_node_impls!(@map_children self $t, __op => () $($field)*)
            }
        }


        impl<ChildT, A: allocator_api2::alloc::Allocator> $crate::NodeLocalEquality for $t<ChildT, A> {
            gen_node_impls!(@impl_local_eq { $($field)* });
        }

        impl<ChildT, A: allocator_api2::alloc::Allocator> Into<$crate::ast::AstNodeData<ChildT, A>> for $t<ChildT, A>  {
            fn into(self) -> $crate::ast::AstNodeData<ChildT, A> {
                $crate::ast::AstNodeData::$t(self)
            }
        }
    };



    ($t:ident<A> { $($field:tt)*  }) => {
        gen_node_impls!(@impl_traits $t<A> { $($field)* });
    };

    ($t:ident { $($field:tt)*  }) => {
        gen_node_impls!(@impl_traits $t { $($field)* });
    };


}
