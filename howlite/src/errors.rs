use std::{
    rc::Rc,
    sync::atomic::{AtomicU64, Ordering},
};

use dashmap::DashMap;
use howlite_syntax::Span;
use howlite_typecheck::{
    errors::{IncompatibleError, OperationError},
    BindError, Ty,
};
use smol_str::SmolStr;
use thiserror::Error;

use crate::symtab::Symbol;

/* #region ErrorSet */
#[derive(Debug)]
pub struct ErrorSet {
    pub errors: DashMap<ErrorId, CompilationError>,
}

impl Default for ErrorSet {
    fn default() -> Self {
        Self {
            errors: Default::default(),
        }
    }
}

impl ErrorSet {
    pub fn mint_error_id() -> ErrorId {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        ErrorId(NEXT.fetch_add(1, Ordering::Relaxed))
    }

    pub fn new() -> Self {
        Self::default()
    }

    /// Define a variable in the given scope
    pub fn insert(&self, err: CompilationError) -> ErrorId {
        let id = Self::mint_error_id();
        debug_assert!(
            self.errors.insert(id, err).is_none(),
            "LangCtx::error(): error ({}) exists, this should be impossible",
            id
        );
        id
    }

    pub fn iter(&self) -> ErrorIter<'_> {
        ErrorIter {
            inner: self.errors.iter(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
}

pub struct ErrorIter<'a> {
    inner: dashmap::iter::Iter<'a, ErrorId, CompilationError>,
}

pub struct ErrorEntry<'a> {
    inner: dashmap::mapref::multiple::RefMulti<'a, ErrorId, CompilationError>,
}

impl<'a> ErrorEntry<'a> {
    pub fn id(&self) -> ErrorId {
        self.inner.key().clone()
    }

    pub fn error(&self) -> &CompilationError {
        self.inner.value()
    }
}

impl<'a> Iterator for ErrorIter<'a> {
    type Item = ErrorEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|inner| ErrorEntry { inner })
    }
}
/* #endregion */

#[derive(Clone, Debug)]
pub struct CompilationError {
    pub location: Span,
    pub kind: CompilationErrorKind,
}

#[derive(Error, Debug, Clone)]
pub enum CompilationErrorKind {
    #[error("invalid arithmetic operation: {}", _0)]
    InvalidArithmetic(#[from] OperationError<Symbol>),

    #[error("Type {:?}: expcted {} type parameters, got {}", ty, expected, got)]
    IncorrectTyParamCount {
        expected: usize,
        got: usize,
        ty: Symbol,
    },

    #[error("Type {:?}, parameter {:?}: {source}", ty, param)]
    InvalidTyParam {
        param: Symbol,
        source: IncompatibleError<Symbol>,
        ty: Symbol,
    },

    #[error("failed while trying to instantiate type: {source}")]
    TypeInstantiateError {
        #[from]
        source: BindError<Symbol>,
    },

    #[error("unknown variable '{name}'")]
    UnknownVariable { name: SmolStr },

    #[error("unknown type '{name}'")]
    UnknownType { name: SmolStr },

    #[error("expected integer bound to be a single Int, found: {got:?}")]
    InvalidIntegerBound { got: Rc<Ty<Symbol>> },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorId(u64);

impl std::fmt::Display for ErrorId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Error:{:04}>", self.0)
    }
}
