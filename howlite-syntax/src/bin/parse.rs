use clap::Parser;
use howlite_syntax::{
    ast::{BoxAstNode, HigherOrderNode},
    tree::{DefaultLinearTreeId, Tree, TreeBuilder},
    AstNode,
};
use lrpar::LexError;
use lrpar::Lexeme;
use lrpar::NonStreamingLexer;
use serde::Serialize;
use std::io::Read;
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(help = "input Howlite source file")]
    file: String,
}

pub struct ToRefTree<'a> {
    tree: &'a Tree<AstNode>,
}

impl<'a> ToRefTree<'a> {
    pub fn to_boxed(&'a self, id: DefaultLinearTreeId) -> BoxAstNode {
        let node = self.tree.get(id);
        node.clone().map(|c_id| self.to_boxed(c_id)).into()
    }
}

pub fn to_boxed<'a>(tree: &'a Tree<AstNode>, node: DefaultLinearTreeId) -> BoxAstNode {
    ToRefTree { tree }.to_boxed(node)
}

#[derive(Serialize)]
pub enum Repair {
    Insert(String),
    Delete(String),
    Shift(String),
}

#[derive(Serialize)]
pub struct ParseError {
    token: String,
    line: usize,
    column: usize,
    repairs: Vec<Vec<Repair>>,
}

#[derive(Serialize)]
struct ParseResult {
    ast: Option<BoxAstNode>,
    errors: Vec<ParseError>,
}

fn main() {
    let mut src_buf = String::new();

    let args = Args::parse();
    std::fs::File::open(&args.file)
        .expect("failed to open file")
        .read_to_string(&mut src_buf)
        .expect("failed to read file");

    let lexerdef = howlite_syntax::lexerdef();
    let lexer = lexerdef.lexer(&src_buf);
    let tree_builder: TreeBuilder<_> = TreeBuilder::default();
    let (root, errs) = howlite_syntax::parse(&lexer, &tree_builder);
    let tree = tree_builder.finalize();

    let ser_errors: Vec<_> = errs
        .into_iter()
        .map(|e| match e {
            lrpar::LexParseError::LexError(e) => {
                let ((line, column), _) = lexer.line_col(e.span());
                ParseError {
                    token: src_buf[e.span().start()..e.span().end()].to_string(),
                    line,
                    column,
                    repairs: vec![],
                }
            }
            lrpar::LexParseError::ParseError(e) => {
                let span = e.lexeme().span();
                let ((line, column), _) = lexer.line_col(span);
                ParseError {
                    token: src_buf[span.start()..span.end()].to_string(),
                    line,
                    column,
                    repairs: e
                        .repairs()
                        .iter()
                        .map(|seq| {
                            seq.iter()
                                .map(|r| match r {
                                    lrpar::ParseRepair::Insert(tidx) => Repair::Insert(
                                        howlite_syntax::token_epp(*tidx)
                                            .unwrap_or("<UNKNOWN>")
                                            .to_string(),
                                    ),
                                    lrpar::ParseRepair::Delete(l) => Repair::Delete(
                                        src_buf[l.span().start()..l.span().end()].to_string(),
                                    ),
                                    lrpar::ParseRepair::Shift(l) => Repair::Shift(
                                        src_buf[l.span().start()..l.span().end()].to_string(),
                                    ),
                                })
                                .collect()
                        })
                        .collect(),
                }
            }
        })
        .collect();
    let result = ParseResult {
        ast: root.unwrap().ok().map(|root| to_boxed(&tree, root)),
        errors: ser_errors,
    };
    print!(
        "{}",
        serde_json::to_string_pretty(&result).expect("conversion to json failed")
    );
}
