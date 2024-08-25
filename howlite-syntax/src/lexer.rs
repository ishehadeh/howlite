use super::{ParseError, ParseErrorKind};
use logos::{Logos, SpannedIter};

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Token<'input> {
    #[token("let")]
    KeywordLet,
    #[token("mut")]
    KeywordMut,
    #[token("extern")]
    KeywordExtern,
    #[token("bool")]
    KeywordBool,
    #[token("unit")]
    KeywordUnit,
    #[token("true")]
    KeywordTrue,
    #[token("false")]
    KeywordFalse,

    #[token("struct")]
    KeywordStruct,

    #[token("type")]
    KeywordType,
    #[token("if")]
    KeywordIf,
    #[token("while")]
    KeywordWhile,
    #[token("else")]
    KeywordElse,

    #[token("func")]
    KeywordFunc,

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice())]
    Identifier(&'input str),

    #[regex("\\d+", |lex| lex.slice())]
    LiteralInteger(&'input str),

    #[token("(")]
    ParenLeft,
    #[token(")")]
    ParenRight,
    #[token("[")]
    BraceSquareLeft,
    #[token("]")]
    BraceSquareRight,
    #[token("{")]
    BraceCurlyLeft,
    #[token("}")]
    BraceCurlyRight,
    #[token("<{")]
    TyParamStart,
    #[token("}>")]
    TyParamEnd,

    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    #[token("..")]
    IntegerRange,

    #[token("=")]
    OperatorAssign,

    #[token("==")]
    OperatorCmpEq,
    #[token("!=")]
    OperatorCmpNe,
    #[token("<")]
    OperatorCmpLt,
    #[token(">")]
    OperatorCmpGt,
    #[token("<=")]
    OperatorCmpLe,
    #[token(">=")]
    OperatorCmpGe,

    #[token("+")]
    OperatorAdd,
    #[token("-")]
    OperatorSub,
    #[token("*")]
    OperatorMul,
    #[token("/")]
    OperatorDiv,

    #[regex(r"[ \t\r\n\f]+", |lex| lex.slice())]
    Whitespace(&'input str),
}

// lexer for lalrpop
// source: http://lalrpop.github.io/lalrpop/lexer_tutorial/005_external_lib.html, access 2/13/24

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    // instead of an iterator over characters, we have a token iterator
    token_stream: SpannedIter<'input, Token<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // the Token::lexer() method is provided by the Logos trait
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|(token, span)| {
            match token {
                // an invalid token was met
                Err(()) => Err(ParseError::new(
                    span.start,
                    span.end,
                    ParseErrorKind::InvalidToken,
                )),
                Ok(v) => Ok((span.start, v, span.end)),
            }
        })
    }
}
