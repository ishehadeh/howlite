pub mod ast;
pub mod span;
pub mod tree;
pub use ast::{AstNode, AstNodeData};
use howlite_y::AstRef;
use std::{env, error::Error};
use tree::{Tree, TreeBuilder};

use lrpar::{LexParseError, Span};

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
pub fn lex_and_parse(
    text: &str,
) -> (
    Tree<AstNode>,
    Result<AstRef, Box<dyn Error>>,
    Vec<LexParseError<u32, DefaultLexerTypes>>,
) {
    let lexerdef: lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexerTypes> = howlite_l::lexerdef();
    let lexer = lexerdef.lexer(text);
    let tree_builder: TreeBuilder<_> = TreeBuilder::default();
    let (res, errs) = howlite_y::parse(&lexer, &tree_builder);
    let tree = tree_builder.finalize();
    match res {
        Some(Err(e)) => (tree, Err(e), errs),
        Some(Ok(v)) => (tree, Ok(v), errs),
        None => (tree, Err("no tree root".to_string().into()), errs),
    }
}

#[test]
fn main() {
    use tree::TreeBuilder;

    // Get the `LexerDef` for the `calc` language.
    let lexerdef = howlite_l::lexerdef();
    // Now we create a lexer with the `lexer` method with which we can lex an
    // input.
    let lexer = lexerdef.lexer("func main(): unit { let a: 100 = 100 } ");
    // Pass the lexer to the parser and lex and parse the input.
    let tree_builder: TreeBuilder<_> = TreeBuilder::default();
    let (res, errs) = howlite_y::parse(&lexer, &tree_builder);
    let tree = tree_builder.finalize();
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
