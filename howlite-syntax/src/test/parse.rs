use proptest::{prop_assert_ne, proptest};

use crate::parse::Parser;
use tracing_test::traced_test;

#[test]
#[traced_test]
pub fn lexer_pos() {
    // check we get accurate spans back in the presence of odd unicode characters
    proptest!(|(t in r#"([+\-<>]/\*((\*+[^/])|[^\*]){3,10}\*/){4,4}"#)| {
        let mut parser = Parser::new(&t);
        for i in 0..8 {
            let token = parser.token();
            prop_assert_ne!(token, None);
            if i % 2 != 0 {
                let range = parser.lexer_span_as_text_range();
                let slice = &t[range];
                assert_eq!(&slice[0..2], "/*");
                assert_eq!(&slice[slice.len() - 2..], "*/")
            }
        }
    });
}
