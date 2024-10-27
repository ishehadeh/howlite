mod string_codec;

/* #region Re-exports  */
pub mod ast;
pub mod span;
pub mod tree;

pub use ast::{AstNode, AstNodeData};
pub use lrpar::Span;
pub use string_codec::*;

#[cfg(feature = "proptest")]
pub mod gen;
/* #endregion */

use allocator_api2::alloc::Allocator;
use allocator_api2::alloc::Global;
use allocator_api2::vec::Vec;
use howlite_y::AstRef;
use std::{env, error::Error};
use tree::{Tree, TreeBuilder};

use lrpar::LexParseError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NewlineKind {
    Lf,
    CrLf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    Line,
    MultiLine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriviaData {
    Space,
    Tab,
    Comment(CommentKind),
    Newline(NewlineKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TriviaPeice {
    pub span: Span,
    pub data: TriviaData,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trivia<A: Allocator = Global> {
    pub peices: Vec<TriviaPeice, A>,
}

impl TriviaPeice {
    pub fn new(span: Span, data: TriviaData) -> TriviaPeice {
        TriviaPeice { span, data }
    }
}

// mod errors;
// pub use errors::{ParseError, ParseErrorKind};

// use lalrpop_util::lalrpop_mod;

// lalrpop_mod!(
//     #[allow(clippy::all)]
//     #[allow(unused)]
//     pub grammar, "/grammar.rs");
use lrlex::{lrlex_mod, DefaultLexerTypes};
use lrpar::lrpar_mod;

// Using `lrlex_mod!` brings the lexer for `calc.l` into scope. By default the
// module name will be `calc_l` (i.e. the file name, minus any extensions,
// with a suffix of `_l`).
lrlex_mod!("howlite.l");
// Using `lrpar_mod!` brings the parser for `calc.y` into scope. By default the
// module name will be `calc_y` (i.e. the file name, minus any extensions,
// with a suffix of `_y`).
lrpar_mod!("howlite.y");

pub use howlite_l::lexerdef;
pub use howlite_y::parse;
pub use howlite_y::token_epp;

pub trait NodeLocalEquality {
    /// Check if two tree nodes are equal, ignoring any attributes of their children.
    /// Note this still takes into account number of children.
    fn local_eq(&self, other: &Self) -> bool;
}

pub fn lex_and_parse(
    text: &str,
) -> (
    Tree<AstNode>,
    Result<AstRef, Box<dyn Error>>,
    std::vec::Vec<LexParseError<u32, DefaultLexerTypes>>,
) {
    let lexerdef: lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexerTypes> = howlite_l::lexerdef();
    let lexer = lexerdef.lexer(text);
    let tree_builder: TreeBuilder<_> = TreeBuilder::new();
    let (res, errs) = howlite_y::parse(&lexer, &tree_builder);
    let tree = tree_builder.finalize();
    match res {
        Some(Err(e)) => (tree, Err(e), errs),
        Some(Ok(v)) => (tree, Ok(v), errs),
        None => (tree, Err("no tree root".to_string().into()), errs),
    }
}
