use logos::Logos;
use proptest::{prop_assert_eq, prop_assume, proptest};
use tracing_test::traced_test;

use crate::tokens::SyntaxKind;

#[test]
#[traced_test]

pub fn comment_multi_line() {
    proptest!(|(t in r#"/\*((\*+[^/])|[^\*]){0,21}\*/"#)| {
        let tokens: Vec<_> = SyntaxKind::lexer(&t).collect();
        prop_assert_eq!(tokens, vec![Ok(SyntaxKind::CommentMultiLine)])
    });

    proptest!(|(t in r#"(/\*((\*+[^/])|[^\*]){0,21}\*/){2,2}"#)| {
        let tokens: Vec<_> = SyntaxKind::lexer(&t).collect();
        prop_assert_eq!(tokens, vec![Ok(SyntaxKind::CommentMultiLine), Ok(SyntaxKind::CommentMultiLine)])
    });
}
