%start Program
%parse-param tree: &crate::treeslab::TreeSlab<AstNode> 
%%
Program -> Result<AstRef>: Expr { $1 };

/// BEGIN: Full Expressions

Expr -> Result<AstRef>:
    ExprInfix { $1 }
  | ExprLet { $1 }
  | ExprBlock { $1 }
  ;

/// END: Full Expression


/// BEGIN: Block Expression

ExprBlock -> Result<AstRef>:
    '{' Trivia ExprBlockStmtList ';' Trivia '}' Trivia {
      // TODO: which node do we associate the inner trivia with?
      trivia!(left trivia_tree, $7,
        node!(tree, $span, Block {
          returns: false,
          statements: $3?
        }))
    }
  | '{' Trivia ExprBlockStmtList '}' Trivia {
    trivia!(left trivia_tree, $7,
        node!(tree, $span, Block {
          returns: true,
          statements: $3?
        }))
    }
  ;

ExprBlockStmtList -> Result<Vec<AstRef>>:
    ExprBlockStmtList ';' Trivia Expr {
      let mut arr = $1?;
      arr.push(trivia!(left trivia_tree, $3, $4));
      Ok(arr)
    }
  | Expr { Ok(vec![$1?]) }
  ;

/// BEGIN: Let Expression
ExprLet -> Result<AstRef>:
    'let' TriviaRequired Ident ':' Trivia TyExpr '=' Trivia ExprSimple { 
      node!(tree, $span, StmtLet {
        name: trivia!(left trivia_tree, $2, $3),
        ty: trivia!(left trivia_tree, $5, $6),
        value: trivia!(left, trivia_tree, $8, $9),
        mutable: false,
      })
    }
  ;

// Like Expr, but force keyword and assign expressions to be in parens.
ExprSimple -> Result<AstRef>: ExprInfixLogic;

/// END: Let Expression



/// BEGIN: Infix Expressions

ExprInfix -> Result<AstRef>:
  ExprInfixAssign { $1 }
  ;

ExprInfixAssign -> Result<AstRef>:
   ExprInfixLogic '=' Trivia ExprInfixLogic { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Assign) }
  | ExprInfixLogic { $1 }
  ;

ExprInfixLogic -> Result<AstRef>:
    ExprInfixLogic '&&' Trivia ExprInfixCompare {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::LogicalAnd)
  }
  | ExprInfixLogic '||' Trivia ExprInfixCompare {
      infix!(tree,  $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::LogicalAnd)
    }
  | ExprInfixCompare { $1 }
  ;


ExprInfixCompare -> Result<AstRef>:
    ExprInfixCompare '<=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpLtEq)}
  | ExprInfixCompare '>=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpGtEq)}
  | ExprInfixCompare '==' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpEq)}
  | ExprInfixCompare '!=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpNe)}
  | ExprInfixCompare '<' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpLt)}
  | ExprInfixCompare '>' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpGt)}
  | ExprInfixBitwise { $1 }
  ;

ExprInfixBitwise -> Result<AstRef>:
    Term '>>' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, trivia!(right trivia_tree, $5, $4)), InfixOp::BitRShift)
    }
  | Term '<<' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::BitLShift)
    }
  | Term '|' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::BitOr)
    }
  | Term '&' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::BitAnd)
    }
  | Term '^' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::BitXor)
    }
  | ExprInfixAdd { $1 }
  ;

ExprInfixAdd -> Result<AstRef>:
    ExprInfixAdd '-' Trivia ExprInfixMul {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Sub)
    }
  | ExprInfixAdd '+' Trivia ExprInfixMul {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Add)
    }
  | ExprInfixMul { $1 }
  ;

ExprInfixMul -> Result<AstRef>:
    ExprInfixMul '*' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Mul)
    }
  | ExprInfixMul '/' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Div)
    }
  | ExprPrefix { $1 }
  ;

/// END: Infix Expressions


/// BEGIN: Prefix Expressions

ExprPrefixOnly -> Result<AstRef>:
  '-' Trivia Term { 
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::Minus, rhs: $3? }))
    }
  | '+' Trivia Term { 
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::Plus, rhs: $3? }))
    }
  | '~' Trivia Term { 
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::BitNot, rhs: $3? }))
    }
  | '!' Trivia Term { 
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::LogicalNot, rhs: $3? }))
    }
  ;

ExprPrefix -> Result<AstRef>: ExprPrefixOnly { $1 } | Term { $1 };

/// END: Prefix Expressions

Term -> Result<AstRef>:
  LiteralInt { $1 }
  | Ident { $1 }
  | '(' Trivia ExprInfix ')' Trivia { 
    trivia!(
      left trivia_tree, $2, trivia!(right trivia_tree, $5, $3)
    )
  }
  ;

Ident -> Result<AstRef>:
    "IDENT" Trivia { trivia!(right trivia_tree, $2, node!(tree, $span, Ident { symbol: $1?.span() })) }
  ;

LiteralInt -> Result<AstRef>:
    _UInt Trivia { 
      trivia!(right trivia_tree, $4, node!(tree, $span, LiteralInteger {value: $1? }))
    }
  ;


// Helper rule for BigInts, simplifies LiteralInt grammar
_UInt -> Result<BigInt>:
    'UINT2'  { Ok(must_parse_int_radix::<2>($lexer.span_str($1?.span()))) }
  | 'UINT8'  { Ok(must_parse_int_radix::<8>($lexer.span_str($1?.span()))) }
  | 'UINT10' { Ok(must_parse_int_radix::<10>($lexer.span_str($1?.span()))) }
  | 'UINT16' { Ok(must_parse_int_radix::<16>($lexer.span_str($1?.span()))) };
 
/// BEGIN: Types
TyExpr -> Reuslt<AstRef>:
    TyExprUnion { $1 }
  ;

TyExprUnion -> Reuslt<AstRef>:
    TyExprUnion "|" Trivia TyTerm {
      node!(tree, $span, TyExprUnion { 
        lhs: $1?,
        rhs: trivia!(left trivia_tree, $3, $4),
      })
    }
  | TyTerm { $1 }
  ;

TyIntegerRangeTerm -> Result<AstRef>:
    LiteralInt { $1 }
  | ExprPrefixOnly { $1 }
  | ExprBlock { $1 }
  ;

TyIntegerRange -> Reuslt<AstRef>:
    TyIntegerRangeTerm { 
      let number = $1?;
      node!(tree, $span, TyNumberRange { lo: number.clone(), hi: number })
    }
  | TyIntegerRangeTerm '..' Trivia TyIntegerRangeTerm {
      node!(tree, $span, TyNumberRange { lo: $1?, hi: trivia!(left trivia_tree, $4) })
    }
  ;

TyArray -> Result<AstRef>: 
    '[' Trivia TyExpr ';' Trivia LiteralInt ']' Trivia {
      trivia!(right trivia_tree, $8,
        node!(tree, $span, TyArray { 
          element_ty: trivia!(left trivia_tree, $2, $3),
          length: trivia!(left trivia_tree, $5, $6)
        }))
    }
  ;

TySlice -> Result<AstRef>:
    '&[' Trivia TyExpr ';' Trivia TyExpr ']' Trivia {
      trivia!(right trivia_tree, $8,
        node!(tree, $span, TySlice { 
          element_ty: trivia!(left trivia_tree, $2, $3),
          length_ty: trivia!(left trivia_tree, $5, $6)
        }))
    }
  ;

TyRef -> Result<AstRef>:
    '&' Trivia TyTerm {
      node!(tree, $span, TyRef { 
        lhs: $1?,
        rhs: trivia!(left trivia_tree, $3, $4),
      })
    }
  ;

/*
TyParamList -> Result<Vec<NodeId<AstNode>>>:
    TyParam { Ok(vec![$1?]) }
  | TyParam ',' Trivia TyParam {
      let mut arr = $1?;
      arr.push(trivia!(left, trivia_tree, $3, $4));
      Ok(arr)
    }
  ;
*/

TyNamed -> Result<AstRef>:
    Ident {
      trivia!(right trivia_tree, $2,
        node!(tree, $span, TyNamed { 
          name: $1?,
          parameters: vec![],
        })
      )
    }
  ;

TyTerm -> Reuslt<AstRef>:
    TyIntegerRange { $1 }
  | '(' Trivia TyExpr ')' Trivia { 
      // TODO: how to assoc final trivia
      trivia!(left trivia_tree, $2, $3)
    }
  | TyRef { $1 }
  | TySlice { $1 }
  | TyArray { $1 }
  | TyNamed { $1 }
  | 'unit' Trivia { trivia!(right, trivia_tree, $2, node!(tree, $span, TyUnit {})) }
  ;

/// END: Types

/// BEGIN: Trivia

Trivia -> Result<Option<Trivia>>:
    TriviaSeries { Ok(Some($1?)) }
  | %empty { Ok(None) }
  ;

TriviaRequired -> Result<Option<Trivia>>:
    TriviaSeries { Ok(Some($1?)) }
  ;

TriviaSeries -> Result<Trivia>:
    TriviaSeries TriviaPeice {
      let mut trivia = $1?;
      trivia.peices.push($2?);
      Ok(trivia)
  }
  | TriviaPeice { Ok(Trivia { peices: vec![$1?] }) }
  ;


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

/// END: Trivia

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

macro_rules! trivia {
  (left $trivia_map:ident, $trivia:expr, $node:expr) => {
    $node
  };

  (right $trivia_map:ident, $trivia:expr, $node:expr) => {
    $node
  }
}