mod scope;

use std::{
    fmt::Debug,
    sync::{
        atomic::{AtomicU64, Ordering},
        RwLock,
    },
};

use crate::{
    errors::ErrorSet,
    symtab::{Symbol, SyncSymbolTable},
    CompilationError,
};

pub mod lexicalctx;

use dashmap::DashMap;
use howlite_syntax::{
    tree::{DefaultLinearTreeId, Tree},
    AstNode,
};
use lexicalctx::LexicalContext;
pub use scope::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(u64);

pub struct LangCtx<'a> {
    pub symbols: SyncSymbolTable,
    scopes: DashMap<ScopeId, Scope>,
    pub errors: ErrorSet,
    scope_parent: RwLock<Vec<(ScopeId, ScopeId)>>,
    pub root_scope_id: ScopeId,
    pub ast: &'a Tree<AstNode>,
}

impl<'a> std::fmt::Debug for LangCtx<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LangCtx")
            .field("scopes", &self.scopes)
            .field("errors", &self.errors)
            .field("scope_parent", &self.scope_parent)
            .field("root_scope_id", &self.root_scope_id)
            .finish()
    }
}

/* #region impl LangCtx: ID Helpers */
impl<'a> LangCtx<'a> {
    /// create a new ScopeId, these identifiers are globally unique within the process
    fn mint_scope_id() -> ScopeId {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        ScopeId(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}
/* #endregion */

/* #region impl LangCtx: General Functionality */
impl<'a> LangCtx<'a> {
    pub fn new(ast: &'a Tree<AstNode>) -> Self {
        let scopes = DashMap::new();
        let root_scope_id = Self::mint_scope_id();
        scopes.insert(root_scope_id, Scope::default());

        Self {
            symbols: Default::default(),
            scope_parent: Default::default(),
            errors: Default::default(),
            scopes,
            ast,
            root_scope_id,
        }
    }

    pub fn make_lexical_context<'b>(
        &'b self,
        scope: ScopeId,
        node: DefaultLinearTreeId,
    ) -> LexicalContext<'a, 'b> {
        LexicalContext {
            scope,
            node,
            parent: self,
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
            !self.scopes.insert(new_scope_id, Scope::default()).is_some(),
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

    /// Recursively search for a type up the scope heirarchy
    ///
    /// # Panics
    /// - Scope does not belong to this LangCtx
    /// - Any panics associated with `self.scope_parent()`
    pub fn ty_get(&self, scope_id: ScopeId, ty: Symbol) -> Option<TyDef> {
        if let Some(scope) = self.scopes.get(&scope_id) {
            if let Some(def) = scope.get_ty(ty) {
                Some(def.clone())
            } else if let Some(parent) = self.scope_parent(scope_id) {
                self.ty_get(parent, ty)
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

    /// Define a named type in the given scope
    ///
    /// # Panics
    /// - Scope does not belong to this LangCtx
    pub fn ty_def(&self, scope_id: ScopeId, def: TyDef) {
        if let Some(mut scope) = self.scopes.get_mut(&scope_id) {
            scope.tys.push(def);
        } else {
            panic!(
                "LangCtx::ty_def(): Scope is not associated with this LangCtx (scope={:?})",
                scope_id
            )
        }
    }

    pub fn error(&self, err: CompilationError) {
        self.errors.insert(err);
    }
}
/* #endregion */
