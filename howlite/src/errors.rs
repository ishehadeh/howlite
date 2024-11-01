use std::{
    rc::Rc,
    sync::atomic::{AtomicU64, Ordering},
};

use howlite_typecheck::{
    errors::{IncompatibleError, OperationError},
    BindError, Ty,
};
use thiserror::Error;

use crate::symtab::Symbol;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorId(u64);

impl std::fmt::Display for ErrorId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Error:{:04}>", self.0)
    }
}

impl ErrorId {
    pub fn mint_error_id() -> ErrorId {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        ErrorId(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

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

    #[error("expected integer bound to be a single Int, found: {got:?}")]
    InvalidIntegerBound { got: Rc<Ty<Symbol>> },
}
