use std::{
    cell::RefCell,
    rc::Rc,
    sync::{
        atomic::{AtomicU64, Ordering},
        RwLock,
    },
};

use allocator_api2::alloc::Allocator;
use dashmap::DashMap;
use howlite_syntax::{
    ast::{
        Block, BoxAstNode, ExprInfix, ExprLet, HigherOrderNode, InfixOp, LiteralInteger,
        LiteralString,
    },
    tree::DefaultLinearTreeId,
    AstNode, AstNodeData, Span,
};
use howlite_typecheck::{
    errors::{IncompatibleError, OperationError},
    types::{StorageClass, TyInt},
    Ty, TyArray,
};
use preseli::IntegerSet;
use slotmap::{new_key_type, Key, SlotMap};
use thiserror::Error;

use crate::symtab::{OwnedSymbolTable, Symbol, SymbolTable, SyncSymbolTable};

new_key_type! { struct ErrorRef; }
new_key_type! { struct ScopeRef; }

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(u64);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorId(u64);

#[derive(Clone, Debug)]
pub struct CompilationError<SourceLocationT> {
    location: SourceLocationT,
    kind: CompilationErrorKind,
}

#[derive(Error, Debug, Clone)]
pub enum CompilationErrorKind {
    #[error("invalid arithmetic operation: {}", _0)]
    InvalidArithmetic(#[from] OperationError<Symbol>),
}

pub struct LangCtx<SourceLocationT> {
    symbols: SyncSymbolTable,
    scopes: DashMap<ScopeId, Scope2>,
    errors: DashMap<ErrorId, CompilationError<SourceLocationT>>,
    scope_parent: RwLock<Vec<(ScopeId, ScopeId)>>,
    root_scope_id: ScopeId,
}

/* #region impl LangCtx: ID Helpers */
impl<L> LangCtx<L> {
    /// create a new ScopeId, these identifiers are globally unique within the process
    fn mint_scope_id() -> ScopeId {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        ScopeId(NEXT.fetch_add(1, Ordering::Relaxed))
    }

    /// create a new ErrorId, these identifiers are globally unique within the process
    fn mint_error_id() -> ErrorId {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        ErrorId(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}
/* #endregion */

/* #region impl LangCtx: General Functionality */
impl<L> LangCtx<L> {
    pub fn new() -> Self {
        let scopes = DashMap::new();
        let root_scope_id = Self::mint_scope_id();
        scopes.insert(root_scope_id, Scope2::default());

        Self {
            symbols: Default::default(),
            scope_parent: Default::default(),
            errors: Default::default(),
            scopes,
            root_scope_id,
        }
    }

    /// Get the parent scope, return None if `scope` is the root scope.
    ///
    /// # Panics
    /// This function panics if `scope` does not belong to this `LangCtx``
    pub fn scope_parent(&self, child: ScopeId) -> Option<ScopeId> {
        if child == self.root_scope_id {
            None
        } else {
            let (child_from_vec, parent) = {
                let scope_parent = self
                    .scope_parent
                    .read()
                    .expect("LangCtx::scope_parent(): RwLock is poisoned!");
                debug_assert!(
                    scope_parent.is_sorted(),
                    "LangCtx::scope_parent(): parent-child map isn't sorted, but we depend on binary search to find parents."
                );
                let pair_index = scope_parent
                .binary_search_by_key(&child, |&(child, _parent)| child)
                .expect("LangCtx::scope_parent(): could not find a parent for scope. There is either a bug in LangCtx's scope-create logic, or this scope does not belong to this instance of LangCtx");
                scope_parent[pair_index]
            };
            debug_assert_eq!(child, child_from_vec, "LangCtx::scope_parent(): binary search found the wrong value, this should be impossible");
            Some(parent)
        }
    }

    pub fn scope_new(&self, parent: ScopeId) -> ScopeId {
        let new_scope_id = Self::mint_scope_id();
        {
            let mut scope_parent = self
                .scope_parent
                .write()
                .expect("LangCtx::scope_new(): RwLock is poisoned!");
            debug_assert!(
                scope_parent.is_sorted(),
                "LangCtx::scope_new(): parent-child map isn't sorted, but we depend on binary search to find parents."
            );
            scope_parent.push((new_scope_id, parent));
            debug_assert!(
                scope_parent.is_sorted(),
                "LangCtx::scope_new(): new scope IDs should always be increasing, but inserting at the end of the array did not maintain sort order"
            );
        }

        debug_assert!(
            self.scopes
                .insert(new_scope_id, Scope2::default())
                .is_some(),
            "LangCtx::scope_new(): scope exists, this should be impossible"
        );

        new_scope_id
    }

    /// Recursively search for a variable up the scope heirarchy
    ///
    /// # Panics
    /// - Scope does not belong to this LangCtx
    /// - Any panics associated with `self.scope_parent()`
    pub fn var_get(&self, scope_id: ScopeId, var: Symbol) -> Option<VarDef> {
        if let Some(scope) = self.scopes.get(&scope_id) {
            if let Some(def) = scope.get_local(var) {
                Some(def.clone())
            } else if let Some(parent) = self.scope_parent(scope_id) {
                self.var_get(parent, var)
            } else {
                // we're at the root scope
                None
            }
        } else {
            panic!(
                "LangCtx::VarGet(): Scope is not associated with this LangCtx (scope={:?})",
                scope_id
            )
        }
    }

    /// Define a variable in the given scope
    ///
    /// # Panics
    /// - Scope does not belong to this LangCtx
    pub fn var_def(&self, scope_id: ScopeId, var: Symbol, def: VarDef) {
        if let Some(mut scope) = self.scopes.get_mut(&scope_id) {
            scope.locals.push((var, def));
        } else {
            panic!(
                "LangCtx::VarGet(): Scope is not associated with this LangCtx (scope={:?})",
                scope_id
            )
        }
    }

    /// Define a variable in the given scope
    pub fn error(&self, err: CompilationError<L>) -> ErrorId {
        let id = Self::mint_error_id();
        debug_assert!(
            self.errors.insert(id, err).is_some(),
            "LangCtx::error(): error exists, this should be impossible"
        );
        id
    }
}
/* #endregion */

#[derive(Debug, Default)]
pub struct Scope2 {
    /// List of local variable definitions.
    /// There can be duplicate symbols if a symbol is redefined
    /// the list MUST be in the order variables are defined
    locals: Vec<(Symbol, VarDef)>,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    assumed_ty: Rc<Ty<Symbol>>,
    last_assignment: Rc<Ty<Symbol>>,
}
#[derive(Debug, Clone)]
pub struct Scope {
    parent: ScopeRef,

    /// List of local variable definitions.
    /// There can be duplicate symbols if a symbol is redefined
    /// the list MUST be in the order variables are defined
    locals: Vec<(Symbol, VarDef)>,
}

impl Scope2 {
    pub fn get_local(&self, name: Symbol) -> Option<&VarDef> {
        self.locals
            .iter()
            .rev()
            .find(|(l_name, _)| *l_name == name)
            .map(|(_, var)| var)
    }

    pub fn get_local_mut(&mut self, name: Symbol) -> Option<&mut VarDef> {
        self.locals
            .iter_mut()
            .rev()
            .find(|(l_name, _)| *l_name == name)
            .map(|(_, var)| var)
    }
}

/// Trait implemented on AST nodes to perform type synthesis, within the context of a program.
/// Type synthesis is the process of determining the smallest possible type that encapsulates all possible values of an expression.
/// For example:
///     synthesize_ty(`1 + 1`) -> `{2}`
///     synthesize_ty(`let x: Uint32; x + 1`) -> `Uint32`
trait SynthesizeTy<L> {
    fn synthesize_ty(&self, ctx: &LangCtx<L>) -> Rc<Ty<Symbol>>;
}

/// Trait implemented on AST nodes that don't need any outer context to perform type synthesis.
trait SynthesizeTyPure {
    fn synthesize_ty_pure(&self) -> Rc<Ty<Symbol>>;
}

impl<T: SynthesizeTyPure, L> SynthesizeTy<L> for T {
    fn synthesize_ty(&self, _: &LangCtx<L>) -> Rc<Ty<Symbol>> {
        self.synthesize_ty_pure()
    }
}

impl<A: Allocator> SynthesizeTy<Span> for AstNode<AstNodeData<Rc<Ty<Symbol>>, A>> {
    fn synthesize_ty(&self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        match &self.data {
            AstNodeData::LiteralInteger(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralString(n) => AstNode::new_narrow(self.span, n).synthesize_ty(ctx),
            AstNodeData::ExprInfix(n) => AstNode::new_narrow(self.span, n).synthesize_ty(ctx),
            // AstNodeData::Block(n) => n.synthesize_ty(ctx),
            _ => todo!(),
        }
    }
}

impl SynthesizeTyPure for LiteralInteger {
    fn synthesize_ty_pure(&self) -> Rc<Ty<Symbol>> {
        Rc::new(Ty::Int(TyInt::single(self.value)))
    }
}

impl SynthesizeTyPure for AstNode<&LiteralString> {
    fn synthesize_ty_pure(&self) -> Rc<Ty<Symbol>> {
        let bytes = self.data.value.as_bytes();
        let values = IntegerSet::new_from_individual_generic(bytes);
        let element_ty = Rc::new(Ty::Int(TyInt {
            values,
            storage: StorageClass::unsigned(8),
        }));

        Rc::new(Ty::Array(TyArray {
            length: self.data.value.len(),
            element_ty,
        }))
    }
}

impl SynthesizeTy<Span> for AstNode<&ExprInfix<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(&self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        match self.data.op {
            InfixOp::Add => match self.data.lhs.arith_add(&self.data.rhs) {
                Ok(v) => Rc::new(v),
                Err(e) => {
                    ctx.error(CompilationError {
                        location: self.span.clone(),
                        kind: e.into(),
                    });
                    Rc::new(Ty::Hole)
                }
            },
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::typetree::SynthesizeTy;

    use super::LangCtx;
    use howlite_syntax::{
        ast::{BoxAstNode, HigherOrderNode, InfixOp, LiteralString},
        gen::{expr_infix, infix_op, literal_integer},
        AstNode, Span,
    };
    use howlite_typecheck::{
        types::{StorageClass, TyInt},
        Ty,
    };
    use proptest::prelude::*;
    use smol_str::ToSmolStr;
    use sunstone::ops::SetOpIncludes;
    pub(crate) fn fold_tree_recursive<V, Tree, F>(t: Tree, op: F) -> V
    where
        F: Fn(Tree::Mapped<V>) -> V,
        for<'a> Tree: 'a + HigherOrderNode<Box<Tree>>,
    {
        op(t.map::<_, V>(|ch| fold_tree_recursive(*ch, &op)))
    }

    prop_compose! {
        fn any_lit_int()(val in literal_integer(0..u64::MAX as i128)) -> BoxAstNode {
            val
        }
    }

    proptest! {
        #[test]
        fn synthesize_literal_int(program in expr_infix(any_lit_int(), any_lit_int(), Just(InfixOp::Add))) {
            let lang = LangCtx::<Span>::new();

            let ty = program.fold(&|s| s.synthesize_ty(&lang));
            assert!(ty.as_int().is_some(), "expected int, got {:?}", ty);
        }

        #[test]
        fn synthesize_literal_string(s in any::<String>()) {
            let lang = LangCtx::<Span>::new();
            let program = BoxAstNode::new(Span::new(0,0), LiteralString { value: s.to_smolstr() });
            let ty = program.fold(&|s| s.synthesize_ty(&lang));
            let arr = ty.as_array().expect("string didn't synthesize to array");
            assert_eq!(arr.length, s.len());
            let elem_ty = arr.element_ty.as_int().expect("element ty was not an int");
            assert_eq!(elem_ty.storage, StorageClass::unsigned(8));
            for byte in s.bytes() {
                assert!(elem_ty.values.includes(byte as i128), "string char type not include {:#02x}", byte);
            }
        }
    }
}
