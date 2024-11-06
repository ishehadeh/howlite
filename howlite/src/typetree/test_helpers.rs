use howlite_syntax::{
    ast::{BoxAstNode, LiteralInteger, TyNumberRange},
    Span,
};

#[macro_export]
macro_rules! assert_lang_ok {
    ($ctx:expr) => {{
        let _ctx = $ctx;
        if !_ctx.errors.is_empty() {
            let _errs: Vec<_> = _ctx
                .errors
                .iter()
                .map(|entry| entry.error().clone())
                .collect();
            panic!("ERRORS {:?}", _errs);
        }
    }};
}

pub fn make_ty_number_range(a: i128, b: i128) -> BoxAstNode {
    BoxAstNode::new(
        Span::new(0, 0),
        TyNumberRange {
            lo: BoxAstNode::new(Span::new(0, 0), LiteralInteger { value: a.min(b) }),
            hi: BoxAstNode::new(Span::new(0, 0), LiteralInteger { value: a.max(b) }),
        },
    )
}
