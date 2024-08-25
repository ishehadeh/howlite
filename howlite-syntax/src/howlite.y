%start Program
%%
Program -> Result<Trivia>:
  Trivia { $1 };

Trivia -> Result<Trivia>:
  Trivia TriviaPeice {
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

%%

use crate::{Trivia, TriviaData, TriviaPeice, NewlineKind, CommentKind};
use lrpar::Span;

pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

