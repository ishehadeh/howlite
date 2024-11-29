use clap::Parser;
use howlite::langctx::LangCtx;
use howlite_syntax::lrpar;
use howlite_syntax::lrpar::LexError;
use howlite_syntax::lrpar::Lexeme;
use howlite_syntax::lrpar::NonStreamingLexer;
use howlite_syntax::{
    ast::{BoxAstNode, HigherOrderNode},
    tree::{DefaultLinearTreeId, Tree, TreeBuilder},
    AstNode,
};
use std::io::Read;
use tracing::error;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::fmt::format::FmtSpan;

#[derive(clap::ValueEnum, Clone, Debug)]
pub enum LogLevel {
    Trace,
    Debug,
    Warn,
    Error,
}

impl LogLevel {
    pub fn to_tracing_filter(&self) -> LevelFilter {
        match self {
            LogLevel::Trace => LevelFilter::TRACE,
            LogLevel::Debug => LevelFilter::DEBUG,
            LogLevel::Warn => LevelFilter::WARN,
            LogLevel::Error => LevelFilter::ERROR,
        }
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(help = "input Howlite source file")]
    file: String,

    #[arg(help = "log level", default_value = "warn")]
    log_level: LogLevel,
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

pub enum Repair {
    Insert(String),
    Delete(String),
    Shift(String),
}
pub struct ParseError {
    token: String,
    line: usize,
    column: usize,
    repairs: Vec<Vec<Repair>>,
}

struct ParseResult {
    ast: Option<BoxAstNode>,
    errors: Vec<ParseError>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]

pub enum Phase {
    Lex,
    Parse,
    TypeCheck,
}

impl std::fmt::Display for Phase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Phase::Lex => f.write_str("lexing"),
            Phase::Parse => f.write_str("parsing"),
            Phase::TypeCheck => f.write_str("type checking"),
        }
    }
}

fn main() {
    let mut src_buf = String::new();

    let args = Args::parse();
    tracing_subscriber::fmt()
        .with_max_level(args.log_level.to_tracing_filter())
        .with_span_events(FmtSpan::FULL)
        .compact()
        .init();

    std::fs::File::open(&args.file)
        .expect("failed to open file")
        .read_to_string(&mut src_buf)
        .expect("failed to read file");

    let lexerdef = howlite_syntax::lexerdef();
    let lexer = lexerdef.lexer(&src_buf);
    let tree_builder: TreeBuilder<_> = TreeBuilder::default();
    let (root, errs) = howlite_syntax::parse(&lexer, &tree_builder);
    let ast = tree_builder.finalize();

    errs.into_iter().for_each(|e| match e {
        lrpar::LexParseError::LexError(e) => {
            let ((line, column), _) = lexer.line_col(e.span());
            let text = &src_buf[e.span().start()..e.span().end()];
            error!(?text, line, column, phase = ?Phase::Lex, "{e}");
        }
        lrpar::LexParseError::ParseError(e) => {
            let span = e.lexeme().span();
            let ((line, column), _) = lexer.line_col(span);

            let text = &src_buf[span.start()..span.end()];
            error!(?text, line, column, phase = ?Phase::Parse, "{e}");
        }
    });

    let root = root.unwrap().unwrap();
    let ctx = LangCtx::new(&ast);
    let root_ctx = ctx.make_lexical_context(ctx.root_scope_id, root);
    let _ = root_ctx.synthesize_ty();

    for err in ctx.errors.iter() {
        error!(error = ?err.error());
    }
}
