use logos::Logos;

use crate::ast::InfixOp;
use memchr::memmem;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // keywords
    #[token("unit")]
    KwUnit,
    #[token("let")]
    KwLet,
    #[token("type")]
    KwType,
    #[token("alias")]
    KwAlias,
    #[token("mut")]
    KwMut,
    #[token("func")]
    KwFunc,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("while")]
    KwWhile,
    #[token("extern")]
    KwExtern,
    #[token("use")]
    KwUse,
    #[token("return")]
    KwReturn,

    #[regex("[_a-zA-Z][_a-zA-Z0-9]* ")]
    Identifier,

    // literals
    #[regex("0x[0-9a-fA-F](_?[0-9a-fA-F])*")]
    LitIntHex,
    #[regex("[0-9](_?[0-9])*")]
    LitIntDec,
    #[regex("0o[0-7](_?[0-7])*")]
    LitIntOct,
    #[regex("0b[01](_?[01])*")]
    LitIntBin,

    // operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("<<")]
    ShiftLeft,
    #[token(">>")]
    ShiftRight,
    #[token("&")]
    Ampersand,
    #[token("|")]
    BitOr,
    #[token("&&")]
    LogicalAnd,
    #[token("||")]
    LogicalOr,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("<=")]
    LessEq,
    #[token(">=")]
    GreaterEq,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("=")]
    Assign,

    #[token("(")]
    ParenL,
    #[token(")")]
    ParenR,
    #[token("[")]
    BracketL,
    #[token("]")]
    BracketR,
    #[token("{")]
    CurlyL,
    #[token("}")]
    CurlyR,

    #[token("&[")]
    SliceBracketL,
    #[token("#[")]
    LiteralArrayBracketL,
    #[token("[:")]
    FuncCallTyParamBracketL,
    #[token("#{")]
    FuncCallTyParamCurlyL,

    #[token(":")]
    Colon,
    #[token("..")]
    DotDot,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("!")]
    Bang,
    #[token("~")]
    Tilde,

    #[regex(r#""((\\["nrt])|(\\x[0-9a-fA-F]{2})|(\\u[0-9a-fA-F]{4})|(\\U[0-9a-fA-F]{6})|[\pL\pM\pN\pS\pP\p{Zs}])*""#)]
    LitString,
    #[regex(r#"'((\\['nrt])|(\\x[0-9a-fA-F]{2})|(\\u[0-9a-fA-F]{4})|(\\U[0-9a-fA-F]{6})|[\pL\pM\pN\pS\pP\p{Zs}])*'"#)]
    LitChar,

    #[token("\r\n")]
    CrLf,
    #[token("\n")]
    Lf,
    #[token(" ")]
    Space,
    #[token("\t")]
    Tab,

    #[regex(r#"//[\pL\pM\pN\pS\pP\p{Zs}]*"#)]
    CommentLine,

    #[token("/*", |lex| {
        let end_idx = memmem::find(lex.remainder().as_bytes(), b"*/")?;
        lex.bump(end_idx + 2); 

        Some(())
    })]
    CommentMultiLine,
    //
    // composite nodes
    ExprPrefix,
    ExprInfix,

    // marks some error in the tree
    Error,

    // top level node
    // IMPORTANT: this node must have the highest value
    Root,
}

impl Into<rowan::SyntaxKind> for SyntaxKind {
    fn into(self) -> rowan::SyntaxKind {
        rowan::SyntaxKind(self as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::Root as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

impl SyntaxKind {
    pub fn as_infix_op(&self) -> Option<InfixOp> {
        Some(match self {
            SyntaxKind::Plus => InfixOp::Div,
            SyntaxKind::Minus => InfixOp::Sub,
            SyntaxKind::Multiply => InfixOp::Mul,
            SyntaxKind::Divide => InfixOp::Div,
            SyntaxKind::ShiftLeft => InfixOp::BitLShift,
            SyntaxKind::ShiftRight => InfixOp::BitRShift,
            SyntaxKind::Ampersand => InfixOp::BitAnd,
            SyntaxKind::BitOr => InfixOp::BitOr,
            SyntaxKind::LogicalAnd => InfixOp::LogicalAnd,
            SyntaxKind::LogicalOr => InfixOp::LogicalOr,
            SyntaxKind::Less => InfixOp::CmpLt,
            SyntaxKind::Greater => InfixOp::CmpGt,
            SyntaxKind::LessEq => InfixOp::CmpLtEq,
            SyntaxKind::GreaterEq => InfixOp::CmpGtEq,
            SyntaxKind::Equal => InfixOp::CmpEq,
            SyntaxKind::NotEqual => InfixOp::CmpNe,
            SyntaxKind::Assign => InfixOp::Assign,
            SyntaxKind::Dot => todo!(),
            _ => return None,
        })
    }
}
