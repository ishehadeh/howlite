use std::fmt::Write;

use logos::Logos;
use lrpar::Span;
use smol_str::{SmolStr, SmolStrBuilder};

#[derive(Logos, Debug, PartialEq)]
#[logos(error = DecodeStringErrorKind)]
pub enum EscapedStrToken {
    #[regex(r#"\\."#, priority = 5)]
    EscChar,

    #[regex(
        r#"\\((x[0-9a-fA-F]{0,2})|(u[0-9a-fA-F]{0,4})|(U[0-9a-fA-F]{0,6}))"#,
        priority = 10
    )]
    EscCodepoint,

    #[regex("[\\pL\\pM\\pN\\pS\\pP\\p{Zs}\\p{Lo}]", priority = 2)]
    Literal,
}

#[derive(Logos, Debug, PartialEq)]
pub enum StrToken {
    #[token("\n", priority = 20)]
    CR,
    #[token("\r", priority = 20)]
    LF,
    #[token("\t", priority = 20)]
    Tab,
    #[token("\"", priority = 20)]
    QuoteDouble,
    #[token("'", priority = 20)]
    QuoteSingle,

    #[regex("[\\pL\\pM\\pN\\pS\\pP\\p{Zs}]+", priority = 10)]
    Literal,

    #[regex(".", priority = 0)]
    NeedsEsc,
}

#[derive(Default, Clone, Copy, Debug, PartialEq)]
pub enum DecodeStringErrorKind {
    InvalidCodepoint,
    UknownEscapeSequence,
    InvalidUnicodeEscape {
        expected_length: usize,
    },

    #[default]
    InvalidLiteralCharacter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecodeStringError {
    span: Span,
    kind: DecodeStringErrorKind,
}

impl std::fmt::Display for DecodeStringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for DecodeStringError {}

pub struct StringEncoder<
    'src,
    const IS_CHAR_LITERAL: bool = false,
    const UPPERCASE_HEX_DIGITS: bool = false,
> {
    src: &'src str,
}

impl<'src> StringEncoder<'src, true, false> {
    pub fn new_for_string(src: &'src str) -> Self {
        Self { src }
    }
}

impl<'src> StringEncoder<'src, false, false> {
    pub fn new_for_char(src: &'src str) -> Self {
        Self { src }
    }
}

impl<'src> StringEncoder<'src, true, true> {
    pub fn new_for_string_upper(src: &'src str) -> Self {
        Self { src }
    }
}

impl<'src> StringEncoder<'src, false, true> {
    pub fn new_for_char_upper(src: &'src str) -> Self {
        Self { src }
    }
}

impl<'src, const IS_CHAR_LITERAL: bool, const UPPERCASE_HEX_DIGITS: bool>
    StringEncoder<'src, IS_CHAR_LITERAL, UPPERCASE_HEX_DIGITS>
{
    pub fn encode(&self) -> SmolStr {
        let mut buf = SmolStrBuilder::new();
        let mut lexer = StrToken::lexer(self.src);
        while let Some(token_res) = lexer.next() {
            let token = token_res.unwrap_or_else(|()| unreachable!("all characters in an arbitrary string should be handled when econding to an escaped string"));
            match token {
                StrToken::CR => buf.write_str("\\r"),
                StrToken::LF => buf.write_str("\\n"),
                StrToken::Tab => buf.write_str("\\t"),
                StrToken::QuoteDouble if !IS_CHAR_LITERAL => buf.write_str("\\\""),
                StrToken::QuoteSingle if IS_CHAR_LITERAL => buf.write_str("\\'"),
                StrToken::NeedsEsc => write_unicode_escape::<_, UPPERCASE_HEX_DIGITS>(
                    &mut buf,
                    lexer.slice().as_bytes(),
                ),
                StrToken::Literal | StrToken::QuoteDouble | StrToken::QuoteSingle => {
                    buf.write_str(lexer.slice())
                }
            }
            .expect("unexpected write error when writing to SmolStrBuilder");
        }

        buf.finish()
    }
}

#[inline(always)]
fn write_unicode_escape<W: Write, const UPPERCASE: bool>(w: &mut W, c: &[u8]) -> std::fmt::Result {
    match c.len() {
        1 => {
            w.write_str("\\x")?;
            let c0 = into_ascii_hex_digits::<UPPERCASE>(c[0]);
            w.write_char(c0[0])?;
            w.write_char(c0[1])?;
            Ok(())
        }
        2 => {
            w.write_str("\\u")?;
            let c1 = into_ascii_hex_digits::<UPPERCASE>(c[1]);
            let c0 = into_ascii_hex_digits::<UPPERCASE>(c[0]);
            w.write_char(c1[0])?;
            w.write_char(c1[1])?;
            w.write_char(c0[0])?;
            w.write_char(c0[1])?;
            Ok(())
        }
        3 => {
            w.write_str("\\U")?;
            let c2 = into_ascii_hex_digits::<UPPERCASE>(c[2]);
            let c1 = into_ascii_hex_digits::<UPPERCASE>(c[1]);
            let c0 = into_ascii_hex_digits::<UPPERCASE>(c[0]);
            w.write_char(c2[0])?;
            w.write_char(c2[1])?;
            w.write_char(c1[0])?;
            w.write_char(c1[1])?;
            w.write_char(c0[0])?;
            w.write_char(c0[1])?;
            Ok(())
        }
        _ => {
            write_unicode_escape::<_, UPPERCASE>(w, &c[0..3])?;
            write_unicode_escape::<_, UPPERCASE>(w, &c[3..])
        }
    }
}

#[inline(always)]
const fn into_ascii_hex_digits<const UPPERCASE: bool>(c: u8) -> [char; 2] {
    let hex_base: u8 = if UPPERCASE { b'A' } else { b'a' };
    let hb_lo = c & 0x0f;
    let hb_hi = (c & 0xf0) >> 4;
    [
        match hb_hi {
            0..10 => hb_hi + b'0',
            10..16 => hb_hi + hex_base - 10,
            _ => unreachable!(),
        } as char,
        match hb_lo {
            0..10 => hb_lo + b'0',
            10..16 => hb_lo + hex_base - 10,
            _ => unreachable!(),
        } as char,
    ]
}

pub struct StringDecoder<'src, const IS_CHAR_LITERAL: bool = false> {
    src: &'src str,
}

const fn from_ascii_hex_digit(digit: u8) -> u8 {
    match digit {
        b'0'..=b'9' => digit - b'0',
        b'a'..=b'f' => digit - b'a' + 10,
        b'A'..=b'F' => digit - b'A' + 10,
        _ => panic!("invalid ascii hex digit"),
    }
}

#[inline(always)]
const fn decode_esc_char_helper<const N: usize>(slice: &[u8], index: usize, exp: u32) -> u32 {
    assert!(N <= 6);
    assert!(slice.len() == N);

    let char_val = from_ascii_hex_digit(slice[index]) as u32 * exp;
    if index == 0 {
        char_val
    } else {
        char_val + decode_esc_char_helper::<N>(slice, index - 1, exp * 16)
    }
}

#[inline(always)]
const fn decode_esc_char<const N: usize>(slice: &[u8]) -> Result<char, DecodeStringErrorKind> {
    assert!(N <= 6);
    if slice.len() != N {
        return Err(DecodeStringErrorKind::InvalidUnicodeEscape { expected_length: N });
    }
    let val = decode_esc_char_helper::<N>(slice, N - 1, 1);
    match char::from_u32(val) {
        Some(v) => Ok(v),
        None => Err(DecodeStringErrorKind::InvalidCodepoint),
    }
}

impl<'src> StringDecoder<'src, false> {
    pub fn new_for_string(src: &'src str) -> Self {
        Self { src }
    }
}

impl<'src> StringDecoder<'src, true> {
    pub fn new_for_char(src: &'src str) -> Self {
        Self { src }
    }
}

impl<'src, const IS_CHAR_LITERAL: bool> StringDecoder<'src, IS_CHAR_LITERAL> {
    pub fn compile(&self) -> Result<SmolStr, DecodeStringError> {
        let mut buffer = SmolStrBuilder::new();
        let mut lexer = EscapedStrToken::lexer(self.src);
        while let Some(tok) = lexer.next() {
            match tok {
                Ok(EscapedStrToken::EscChar) => {
                    // esc char should be in the form \[c]
                    assert!(lexer.slice().len() == 2);

                    // we need to decode the slice as utf-8, since
                    // the second char matches anything (for error-checking purposes)
                    let c = lexer.slice().chars().nth(1).unwrap();
                    match c {
                        '"' if !IS_CHAR_LITERAL => {
                            buffer.push('"');
                            Ok(())
                        }
                        '\'' if IS_CHAR_LITERAL => {
                            buffer.push('\'');
                            Ok(())
                        }
                        'n' => {
                            buffer.push('\n');
                            Ok(())
                        }
                        't' => {
                            buffer.push('\t');
                            Ok(())
                        }
                        'r' => {
                            buffer.push('\r');
                            Ok(())
                        }
                        _ => Err(DecodeStringErrorKind::UknownEscapeSequence),
                    }
                }
                Ok(EscapedStrToken::EscCodepoint) => {
                    assert!(lexer.slice().len() >= 2);
                    // as_bytes here is safe, if there were any unicode characters
                    // they *shouldn't* make it into this branch
                    let bytes = lexer.slice().as_bytes();
                    let c = match bytes[1] {
                        b'x' => decode_esc_char::<2>(&bytes[2..]),
                        b'u' => decode_esc_char::<4>(&bytes[2..]),
                        b'U' => decode_esc_char::<6>(&bytes[2..]),
                        _ => unreachable!("StrToken::EscCodepoint should only ever match x, u, and U escape chars")
                    };
                    match c {
                        Ok(c) => {
                            buffer.push(c);
                            Ok(())
                        }
                        Err(e) => Err(e),
                    }
                }
                Ok(EscapedStrToken::Literal) => {
                    buffer.push_str(lexer.slice());
                    Ok(())
                }
                Err(e) => Err(e),
            }.map_err(|kind| DecodeStringError {
                span: Span::new(lexer.span().start, lexer.span().end + 1),
                kind,
            })?;
        }
        Ok(buffer.finish())
    }
}

#[cfg(test)]
mod test {
    use proptest::proptest;

    use super::StringDecoder;
    use crate::gen::string::{
        any_char_literal_content, any_string_literal_content, any_string_literal_content_char,
    };

    proptest! {
        #[test]
        fn dec_char_single((expected, lit) in any_char_literal_content()) {
            let decoded = StringDecoder::new_for_char(lit.as_str()).compile().expect("failed to decode valid string (char)");
            let got = decoded.chars().next().unwrap();
            assert_eq!(got, expected);
        }

        #[test]
        fn dec_string_single((expected, lit) in any_string_literal_content_char()) {
            let decoded = StringDecoder::new_for_string(lit.as_str()).compile().expect("failed to decode valid string (string)");
            let got = decoded.chars().next().unwrap();
            assert_eq!(got, expected);
        }


        #[test]
        fn dec_string((expected, lit) in any_string_literal_content()) {

            let decoded = StringDecoder::new_for_string(lit.as_str()).compile().expect("failed to decode valid string (string)");
            assert_eq!(decoded.as_str(), expected);
        }
    }
}
