// pub mod ast;
// pub mod lexer;
pub mod ast;
pub mod span;
pub mod treeslab;
pub use ast::{AstNode, AstNodeData};
use std::env;
use treeslab::TreeSlab;

use lrpar::Span;

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
pub struct Trivia {
    pub peices: Vec<TriviaPeice>,
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
use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

// Using `lrlex_mod!` brings the lexer for `calc.l` into scope. By default the
// module name will be `calc_l` (i.e. the file name, minus any extensions,
// with a suffix of `_l`).
lrlex_mod!("howlite.l");
// Using `lrpar_mod!` brings the parser for `calc.y` into scope. By default the
// module name will be `calc_y` (i.e. the file name, minus any extensions,
// with a suffix of `_y`).
lrpar_mod!("howlite.y");

#[test]
fn main() {
    // Get the `LexerDef` for the `calc` language.
    let lexerdef = howlite_l::lexerdef();
    // Now we create a lexer with the `lexer` method with which we can lex an
    // input.
    let lexer = lexerdef.lexer("-/* hello  */0b1001");
    // Pass the lexer to the parser and lex and parse the input.
    let tree: TreeSlab<_> = TreeSlab::default();
    let (res, errs) = howlite_y::parse(&lexer, &tree);

    dbg!(&tree);

    for e in errs {
        println!("{}", e.pp(&lexer, &howlite_y::token_epp));
    }
    match res {
        Some(r) => println!("Result: {:?}", r),
        _ => eprintln!("Unable to evaluate expression."),
    }
    panic!("");
}
