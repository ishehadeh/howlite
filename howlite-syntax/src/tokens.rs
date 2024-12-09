use logos::Logos;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]

pub enum EscapedStrToken {
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

    #[regex(r#"/\*[\pL\pM\pN\pS\pP\p{Zs}\p{Zl}]*\*/"#)]
    CommentMultiLine,
}
