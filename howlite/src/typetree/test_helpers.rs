use std::rc::Rc;

use howlite_syntax::{
    ast::{
        BoxAstNode, ExprLet, HigherOrderNode, LiteralArray, LiteralChar, LiteralInteger,
        LiteralString, LiteralStruct, LiteralStructMember, TyNumberRange,
    },
    tree::{DefaultLinearTreeId, Tree, TreeBuilder},
    AstNode, AstNodeData, Span,
};
use howlite_typecheck::Ty;
use proptest::{
    prelude::{any, any_with, Just, Strategy},
    prop_oneof,
    sample::SizeRange,
    string::StringParam,
};
use smol_str::SmolStr;

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol};

use super::{traits::PrepareLexicalCtx, SynthesizeTy};

pub fn synthesize_box_ast(ctx: &LexicalContext<'_, Span>, node: BoxAstNode) -> Rc<Ty<Symbol>> {
    let AstNode { data, span } = node.into_inner();
    let child_ctx = data.prepare_lexical_ctx(ctx.clone());
    let ty_node = data.map(|child| synthesize_box_ast(&child_ctx, child));
    ty_node.synthesize_ty(&ctx.clone().with_location(span))
}

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

#[macro_export]
macro_rules! get_node_type {
    (boxed $node:expr) => {
        $crate::get_node_type!(howlite_syntax::ast::BoxAstNode::new(
            howlite_syntax::Span::new(0, 0),
            $node,
        ))
    };
    ($node:expr) => {{
        let _lang = $crate::langctx::LangCtx::<howlite_syntax::Span>::new();
        let _ty = $crate::typetree::test_helpers::synthesize_box_ast(
            &_lang.make_lexical_context(_lang.root_scope_id, $node.span.clone()),
            $node,
        );
        $crate::assert_lang_ok!(_lang);
        _ty
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

/// Any literal that cannot contain arbirary data
pub fn any_atomic_literal() -> impl Strategy<Value = BoxAstNode> {
    prop_oneof![
        any::<LiteralChar>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)),
        any::<LiteralString>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)),
        any::<LiteralInteger>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v))
    ]
}

pub fn literal_struct_member<K, V>(
    k: K,
    v: V,
) -> impl Strategy<Value = AstNode<LiteralStructMember<BoxAstNode>>>
where
    K: Strategy<Value = String>,
    V: Strategy<Value = BoxAstNode>,
{
    (k, v).prop_map(|(field, value)| AstNode {
        span: Span::new(0, 0),
        data: LiteralStructMember {
            field: field.into(),
            value,
        },
    })
}

pub fn literal_struct<K, V, S>(k: K, v: V, length: S) -> impl Strategy<Value = BoxAstNode>
where
    K: Strategy<Value = String>,
    V: Strategy<Value = BoxAstNode>,
    S: Into<SizeRange>,
{
    proptest::collection::vec(literal_struct_member(k, v), length).prop_map(|members| {
        BoxAstNode::new(
            Span::new(0, 0),
            LiteralStruct {
                members: members.into_iter().collect(),
            },
        )
    })
}

pub fn literal_array<V, S>(v: V, length: S) -> impl Strategy<Value = BoxAstNode>
where
    V: Strategy<Value = BoxAstNode>,
    S: Into<SizeRange>,
{
    proptest::collection::vec(v, length).prop_map(|members| {
        BoxAstNode::new(
            Span::new(0, 0),
            LiteralArray {
                values: members.into_iter().collect(),
            },
        )
    })
}

pub fn any_literal() -> impl Strategy<Value = BoxAstNode> {
    any_atomic_literal().prop_recursive(4, 32, 12, |inner| {
        prop_oneof![
            literal_struct(any_ident(), inner.clone(), 0..12),
            literal_array(inner, 0..12),
        ]
    })
}

pub fn make_expr_let(name: impl Into<SmolStr>, ty: BoxAstNode, value: BoxAstNode) -> BoxAstNode {
    BoxAstNode::new(
        Span::new(0, 0),
        ExprLet {
            name: name.into(),
            ty,
            mutable: true,
            value,
        },
    )
}

pub fn simple_scalar_let() -> impl Strategy<Value = BoxAstNode> {
    (0..u64::MAX as i128, 0..u64::MAX as i128)
        .prop_flat_map(|(a, b)| (Just(make_ty_number_range(a, b)), a.min(b)..b.max(a)))
        .prop_map(|(ty, value)| {
            make_expr_let(
                "_a",
                ty,
                BoxAstNode::new(Span::new(0, 0), LiteralInteger { value }),
            )
        })
}

pub fn must_parse_expr(src: &str) -> (DefaultLinearTreeId, Tree<AstNode>) {
    let full_src = format!("func main(): {{ }} {{ {}; }}", src);
    dbg!(&full_src);
    let lexerdef = howlite_syntax::lexerdef();
    let lexer = lexerdef.lexer(&full_src);
    let tree_builder: TreeBuilder<_> = TreeBuilder::default();
    let (root, errs) = howlite_syntax::parse(&lexer, &tree_builder);
    for e in &errs {
        println!("{}", e.pp(&lexer, &howlite_syntax::token_epp));
    }
    assert!(errs.is_empty());
    _ = root.unwrap().unwrap();

    let tree = tree_builder.finalize();
    let expr_root = match &tree.iter().rev().nth(1).unwrap().data {
        AstNodeData::DefFunc(f) => match &tree.get(f.body).data {
            AstNodeData::Block(b) => *b.statements.first().unwrap(),
            _ => panic!(),
        },
        _ => unreachable!(),
    };
    (expr_root, tree)
}

pub fn any_ident() -> impl Strategy<Value = String> {
    any_with::<String>(StringParam::from("[_a-zA-Z][_a-zA-Z0-9]*"))
}
