use super::lexer::Token;
use crate::span::SourceSpan;
use lalrpop_util::ParseError as LalrpopError;
use thiserror::Error;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: SourceSpan,
}

impl ParseError {
    pub fn new(start: usize, end: usize, kind: ParseErrorKind) -> ParseError {
        ParseError {
            span: (start, end).into(),
            kind,
        }
    }
}

impl<'i> From<LalrpopError<usize, Token<'i>, ParseError>> for ParseError {
    fn from(value: LalrpopError<usize, Token<'i>, ParseError>) -> Self {
        match value {
            LalrpopError::InvalidToken { location } => {
                Self::new(location, location, ParseErrorKind::InvalidToken)
            }
            LalrpopError::UnrecognizedEof { location, expected } => Self::new(
                location,
                location,
                ParseErrorKind::UnexpectedEof { expected },
            ),
            LalrpopError::UnrecognizedToken {
                token: (start, tok, end),
                expected,
            } => Self::new(
                start,
                end,
                ParseErrorKind::UnexpectedToken {
                    token: format!("{:?}", tok),
                    expected,
                },
            ),
            LalrpopError::ExtraToken {
                token: (start, tok, end),
            } => Self::new(
                start,
                end,
                ParseErrorKind::ExtraToken {
                    token: format!("{:?}", tok),
                },
            ),
            LalrpopError::User { error } => error,
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Error, Clone)]
pub enum ParseErrorKind {
    // Parser Generator Errors
    //
    // NOTE: converted from LalrpopError::InvalidToken
    /// producted on completely unknown input.
    #[error("unexpected character found")]
    InvalidToken,

    // NOTE converted from LalrpopError::UnrecognizedEof
    /// Produced when EOF is found, but more input is expected
    // TODO: map tokens to pretty names
    #[error("unexpected EOF. (expecting {expected:?})")]
    UnexpectedEof {
        // a list of expected tokens, these are directly taken to the grammar they should be mapped to user-facing token names.
        expected: Vec<String>,
    },

    // NOTE: converted from LalrpopError::UnrecognizedToken
    /// Produced when a known token is found, but it didn't match any of the expected tokens
    // TODO: map tokens to pretty names
    #[error("unexpected token {token}. (expected {expected:?})")]
    UnexpectedToken {
        /// the unexpected token found
        token: String,

        /// a list of expected tokens, these are directly taken to the grammar they should be mapped to user-facing token names.
        expected: Vec<String>,
    },

    // NOTE: converted from LalrpopError::ExtraToken
    /// EOF was expected, but another token was found instead
    #[error("expecting EOF, found {token}")]
    ExtraToken { token: String },

    // production errors
    //   These errors are produced by dedicated productions
    //
    /// producted when associativity/precedence is left up to inference (e.g. 1 + 1 + 1)
    #[error(
        "implicit operator precedence is disallowed, please use explicit grouping in expressions."
    )]
    NoExplicitExprGrouping,

    #[error(
        "invalid infix operator (got '{}', expected: ['+', '-', '*', '/'])",
        op
    )]
    BadInfixOp { op: String },
}
