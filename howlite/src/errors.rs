use std::{
    rc::Rc,
    sync::atomic::{AtomicU64, Ordering},
};

use dashmap::DashMap;
use howlite_typecheck::{
    errors::{IncompatibleError, OperationError},
    BindError, Ty,
};
use smol_str::SmolStr;
use thiserror::Error;

use crate::symtab::Symbol;

/* #region ErrorSet */
#[derive(Debug)]
pub struct ErrorSet<SourceLocationT> {
    pub errors: DashMap<ErrorId, CompilationError<SourceLocationT>>,
}

impl<SourceLocationT> Default for ErrorSet<SourceLocationT> {
    fn default() -> Self {
        Self {
            errors: Default::default(),
        }
    }
}

impl<SourceLocationT> ErrorSet<SourceLocationT> {
    pub fn mint_error_id() -> ErrorId {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        ErrorId(NEXT.fetch_add(1, Ordering::Relaxed))
    }

    pub fn new() -> Self {
        Self::default()
    }

    /// Define a variable in the given scope
    pub fn insert(&self, err: CompilationError<SourceLocationT>) -> ErrorId {
        let id = Self::mint_error_id();
        debug_assert!(
            self.errors.insert(id, err).is_none(),
            "LangCtx::error(): error ({}) exists, this should be impossible",
            id
        );
        id
    }

    pub fn iter(&self) -> ErrorIter<'_, SourceLocationT> {
        ErrorIter {
            inner: self.errors.iter(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
}

pub struct ErrorIter<'a, SourceLocationT> {
    inner: dashmap::iter::Iter<'a, ErrorId, CompilationError<SourceLocationT>>,
}

pub struct ErrorEntry<'a, SourceLocationT> {
    inner: dashmap::mapref::multiple::RefMulti<'a, ErrorId, CompilationError<SourceLocationT>>,
}

impl<'a, SourceLocationT> ErrorEntry<'a, SourceLocationT> {
    pub fn id(&self) -> ErrorId {
        self.inner.key().clone()
    }

    pub fn error(&self) -> &CompilationError<SourceLocationT> {
        self.inner.value()
    }
}

impl<'a, SourceLocationT> Iterator for ErrorIter<'a, SourceLocationT> {
    type Item = ErrorEntry<'a, SourceLocationT>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|inner| ErrorEntry { inner })
    }
}
/* #endregion */

#[derive(Clone, Debug)]
pub struct CompilationError<SourceLocationT> {
    pub location: SourceLocationT,
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

    #[error("Unknown type: {source}")]
    UnknownTyName {
        #[from]
        source: BindError<Symbol>,
    },

    #[error("unknown variable '{name}'")]
    UnknownVariable { name: SmolStr },

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
