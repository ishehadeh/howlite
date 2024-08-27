%start Program
%parse-param tree: &crate::treeslab::TreeSlab<AstNode> 
%%
Program -> Result<AstRef>:
  ExprInfix { $1 };

TriviaSeries -> Result<Trivia>:
    TriviaSeries TriviaPeice {
      let mut trivia = $1?;
      trivia.peices.push($2?);
      Ok(trivia)
  }
  | TriviaPeice { Ok(Trivia { peices: vec![$1?] }) }
  ;

Trivia -> Result<Option<Trivia>>:
    TriviaSeries { Ok(Some($1?)) }
  | %empty { Ok(None) }
  ;

/// BEGIN: Infix Expressions

ExprInfix -> Result<AstRef>:
  ExprInfixAssign { $1 }
  ;

ExprInfixAssign -> Result<AstRef>:
   ExprInfixLogic '=' Trivia ExprInfixLogic { infix!(tree, $span, $1, $4, InfixOp::Assign) }
  | ExprInfixLogic { $1 }
  ;

ExprInfixLogic -> Result<AstRef>:
    ExprInfixLogic '&&' Trivia ExprInfixCompare {
      infix!(tree, $span, $1, $4, InfixOp::LogicalAnd)
  }
  | ExprInfixLogic '||' Trivia ExprInfixCompare {
      infix!(tree, $span, $1, $4, InfixOp::LogicalAnd)
    }
  | ExprInfixCompare { $1 }
  ;


ExprInfixCompare -> Result<AstRef>:
    ExprInfixCompare '<=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, $4, InfixOp::CmpLtEq)}
  | ExprInfixCompare '>=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, $4, InfixOp::CmpGtEq)}
  | ExprInfixCompare '==' Trivia ExprInfixBitwise { infix!(tree, $span, $1, $4, InfixOp::CmpEq)}
  | ExprInfixCompare '!=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, $4, InfixOp::CmpNe)}
  | ExprInfixCompare '<' Trivia ExprInfixBitwise { infix!(tree, $span, $1, $4, InfixOp::CmpLt)}
  | ExprInfixCompare '>' Trivia ExprInfixBitwise { infix!(tree, $span, $1, $4, InfixOp::CmpGt)}
  | ExprInfixBitwise { $1 }
  ;

ExprInfixBitwise -> Result<AstRef>:
    Term '>>' Trivia Term {
      infix!(tree, $span, $1, $4, InfixOp::BitRShift)
    }
  | Term '<<' Trivia Term {
      infix!(tree, $span, $1, $4, InfixOp::BitLShift)
    }
  | Term '|' Trivia Term {
      infix!(tree, $span, $1, $4, InfixOp::BitOr)
    }
  | Term '&' Trivia Term {
      infix!(tree, $span, $1, $4, InfixOp::BitAnd)
    }
  | Term '^' Trivia Term {
      infix!(tree, $span, $1, $4, InfixOp::BitXor)
    }
  | ExprInfixAdd { $1 }
  ;

ExprInfixAdd -> Result<AstRef>:
    ExprInfixAdd '-' Trivia ExprInfixMul {
      infix!(tree, $span, $1, $4, InfixOp::Sub)
    }
  | ExprInfixAdd '+' Trivia ExprInfixMul {
      infix!(tree, $span, $1, $4, InfixOp::Add)
    }
  | ExprInfixMul { $1 }
  ;

ExprInfixMul -> Result<AstRef>:
    ExprInfixMul '*' Trivia Term {
      infix!(tree, $span, $1, $4, InfixOp::Mul)
    }
  | ExprInfixMul '/' Trivia Term {
      infix!(tree, $span, $1, $4, InfixOp::Div)
    }
  | Term { $1 }
  ;

/// END: Infix Expressions

Term -> Result<AstRef>:
  LiteralInt { $1 }
  | Ident { $1 }
  | '(' Trivia ExprInfix ')' Trivia { $3 }
  ;

Ident -> Result<AstRef>:
    "IDENT" Trivia { node!(tree, $span, Ident { symbol: $1?.span() }) }
  ;

LiteralInt -> Result<AstRef>:
    '-' Trivia LiteralUInt Trivia { node!(tree, $span, LiteralInteger { value: -($3?) }) }
  | '+'  Trivia LiteralUInt  Trivia { node!(tree, $span, LiteralInteger { value: $3? }) }
  | LiteralUInt Trivia { node!(tree, $span, LiteralInteger {value: $1? }) };

LiteralUInt -> Result<BigInt>:
    'UINT2'  { Ok(must_parse_int_radix::<2>($lexer.span_str($1?.span()))) }
  | 'UINT8'  { Ok(must_parse_int_radix::<8>($lexer.span_str($1?.span()))) }
  | 'UINT10' { Ok(must_parse_int_radix::<10>($lexer.span_str($1?.span()))) }
  | 'UINT16' { Ok(must_parse_int_radix::<16>($lexer.span_str($1?.span()))) };
 
TriviaPeice -> Result<TriviaPeice>:
  Newline { $1 }
  | Space { $1 }
  | Tab { $1 }
  | LineComment { $1 }
  | MultiLineComment { $1 }
  ;

Newline -> Result<TriviaPeice>:
   'LF'   { Ok(TriviaPeice::new($1?.span(), TriviaData::Newline(NewlineKind::Lf))) }
  |'CRLF' { Ok(TriviaPeice::new($1?.span(), TriviaData::Newline(NewlineKind::CrLf))) }
  ;

Space -> Result<TriviaPeice>:
  'SPACE' { Ok(TriviaPeice::new($1?.span(), TriviaData::Space)) };

Tab -> Result<TriviaPeice>:
  'TAB' { Ok(TriviaPeice::new($1?.span(), TriviaData::Tab)) };

LineComment -> Result<TriviaPeice>:
  'COMMENT-LINE' {
    Ok(TriviaPeice::new($span , TriviaData::Comment(CommentKind::Line)))
  };

MultiLineComment -> Result<TriviaPeice>:
  'COMMENT-MULTILINE' {
    Ok(TriviaPeice::new($span, TriviaData::Comment(CommentKind::MultiLine)))
  };

%%

use crate::{Trivia, TriviaData, TriviaPeice, NewlineKind, treeslab, treeslab::NodeId, CommentKind, ast::*};
use lrpar::{Span, NonStreamingLexer, LexerTypes};
use num_bigint::{BigInt, Sign};

pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

#[inline(always)]
fn must_parse_int_radix<const RADIX: u32>(s: &str) -> BigInt {
  let radix_prefix_size = if RADIX == 10 { 0 } else { 2 };

  /* TODO(ian, low): potential optimization: we can parse numbers ourselves here to avoid an extra allocation
  let digit_count = s.len() - radix_prefix_size;
  let est_bits_per_digit = RADIX.next_power_of_two().ilog2();
  let est_bytes = ((est_bits_per_digit * digit_count) / 32) + 1;
  let mut digits: Vec<u32> = Vec::with_capacity(est_bytes);
  */

  // lexer should have already verified these are all ascii digits in radix
  let mut digit_bytes = s[radix_prefix_size..]
    .bytes()
    .filter(|&c| c != b'_')
    .collect::<Vec<u8>>();
  
  if RADIX > 10 {
    for i in 0..digit_bytes.len() {
      digit_bytes[i] += ((digit_bytes[i] & 0b0100_000) >> 3) | ((digit_bytes[i] & 0b0100_000) >> 7);
    }
  }

  for i in 0..digit_bytes.len() {
    digit_bytes[i] &= 0b0000_1111;
  }

  BigInt::from_radix_be(Sign::Plus, digit_bytes.as_slice(), RADIX).unwrap()
}

pub type AstRef = NodeId<AstNode>;

macro_rules! node {
  ($t:ident, $span:expr, $node:expr) => {
    Ok($t.push(AstNode::new($span, $node)))
  }
}
macro_rules! infix {
  ($tree:ident, $span:ident, $lhs:expr, $rhs:expr, $op:expr) => {
    node!($tree, $span, ExprInfix { lhs: $lhs?, rhs: $rhs?, op: $op })
  }
}