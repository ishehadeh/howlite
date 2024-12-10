use std::ops::{Bound, RangeBounds};

use logos::Lexer;
use rowan::{GreenNodeBuilder, TextRange};
use smol_str::SmolStr;
use tracing::{instrument, trace};

use crate::{ast::InfixOp, tokens::SyntaxKind};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("unrecognized character(s) in input: {_0}")]
    LexError(SmolStr),
}

#[derive(Debug)]
pub struct ParseError {
    pub location: rowan::TextRange,
    pub kind: ParseErrorKind,
}

#[derive(Debug)]
pub struct Parser<'source, 'cache> {
    builder: GreenNodeBuilder<'cache>,
    lexer: logos::Lexer<'source, SyntaxKind>,
    pub error: Vec<ParseError>,
}

impl<'source> Parser<'source, 'static> {
    pub fn new(source: &'source str) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            lexer: Lexer::new(source),
            error: Default::default(),
        }
    }
}

impl<'source, 'cache> Parser<'source, 'cache> {
    pub fn lexer_span_as_text_range(&self) -> TextRange {
        let span = self.lexer.span();
        #[cfg(debug_assertions)]
        {
            match span.end_bound() {
                Bound::Excluded(&n) if n > u32::MAX as usize => {
                    panic!("lexer_span_as_text_range: lexer.span() has an upper bound exceeding u32::MAX. lexer.span() = {span:?}")
                }
                Bound::Excluded(_) => (),
                _ => panic!("lexer_span_as_text_range: lexer.span() must have an excluded upper bound. lexer.span() = {span:?}"),
            };
            match span.start_bound() {
                Bound::Included(&n) if n > u32::MAX as usize => {
                    panic!("lexer_span_as_text_range: lexer.span() has a lower bound exceeding u32::MAX. lexer.span() = {span:?}")
                }
                Bound::Included(_) => (),
                _ => panic!("lexer_span_as_text_range: lexer.span() must have an included lower bound. lexer.span() = {span:?}"),
            }
        };

        TextRange::new((span.start as u32).into(), (span.end as u32).into())
    }

    #[instrument(skip(self), ret(Debug), level = "trace")]
    pub fn token(&mut self) -> Option<SyntaxKind> {
        let next_token = self.lexer.next();
        trace!(token=?next_token, slice=?self.lexer.slice());
        match next_token {
            Some(Ok(
                ws @ (SyntaxKind::CrLf | SyntaxKind::Lf | SyntaxKind::Space | SyntaxKind::Tab),
            )) => {
                trace!("skipping whitespace");
                self.builder.token(ws.into(), self.lexer.slice());
                self.token()
            }
            Some(Ok(token)) => Some(token),
            Some(Err(())) => {
                let err = ParseError {
                    location: self.lexer_span_as_text_range(),
                    kind: ParseErrorKind::LexError(self.lexer.slice().into()),
                };
                trace!(?err, "add parse error");
                self.error.push(err);
                Some(SyntaxKind::Error)
            }
            None => {
                trace!("eof");
                None
            }
        }
    }

    fn expect_infix_operator(&mut self) -> Option<(SyntaxKind, InfixOp)> {
        let token = self.token();
        let mapping = token.and_then(|t| t.as_infix_op().map(|op| (t, op)));
        trace!("expect_infix_operator: mapping {token:?} -> {mapping:?}");
        mapping
    }
}
