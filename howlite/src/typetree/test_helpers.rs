use howlite_syntax::{
    ast::{
        self, BoxAstNode, ExprLet, HigherOrderNode, LiteralArray, LiteralChar, LiteralInteger,
        LiteralString, LiteralStruct, LiteralStructMember, TyNumberRange, TyRef, TyStruct,
        TyStructMember,
    },
    tree::{DefaultLinearTreeId, Tree, TreeBuilder},
    AstNode, AstNodeData, Span,
};
use proptest::{
    prelude::{any, any_with, Just, Strategy},
    prop_oneof,
    sample::SizeRange,
    string::StringParam,
};
use smol_str::SmolStr;

pub fn box_tree_to_linear_helper(
    builder: &TreeBuilder<AstNode>,
    root: BoxAstNode,
) -> DefaultLinearTreeId {
    let node_with_child_ids = root.map(|child| box_tree_to_linear_helper(builder, child));
    builder.push(node_with_child_ids)
}

pub fn box_tree_to_linear(root: BoxAstNode) -> (DefaultLinearTreeId, Tree<AstNode>) {
    let tree_builder = TreeBuilder::new();
    let root_id = box_tree_to_linear_helper(&tree_builder, root);
    (root_id, tree_builder.finalize())
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
    (boxed $node:expr$(, type $tname:ident = $tdef:expr)*) => {
        $crate::get_node_type!(howlite_syntax::ast::BoxAstNode::new(
            howlite_syntax::Span::new(0, 0),
            $node,
        ) $(, type $tname = $tdef)*)
    };
    ($node:expr $(, type $tname:ident = $tdef:expr)*) => {{
        let (_root, _ast) = $crate::typetree::test_helpers::box_tree_to_linear($node);
        let _lang = $crate::langctx::LangCtx::new(&_ast);
        $(
            let _def_ty_name = _lang.symbols.intern(std::stringify!($tname));
            _lang.ty_def(_lang.root_scope_id, $crate::langctx::TyDef {
                name: _def_ty_name,
                params: Default::default(),
                ty: $tdef,
            });
        )*

        let _ty = _lang
        .make_lexical_context(_lang.root_scope_id, _root)
        .synthesize_ty();
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

pub fn ty_struct_member<K, V>(k: K, v: V) -> impl Strategy<Value = BoxAstNode>
where
    K: Strategy<Value = String>,
    V: Strategy<Value = BoxAstNode>,
{
    (k, v).prop_map(|(field, ty)| {
        BoxAstNode::new(
            Span::new(0, 0),
            TyStructMember {
                name: field.into(),
                ty,
                mutable: true,
            },
        )
    })
}

pub fn ty_struct<K, V, S>(k: K, v: V, length: S) -> impl Strategy<Value = BoxAstNode>
where
    K: Strategy<Value = String>,
    V: Strategy<Value = BoxAstNode>,
    S: Into<SizeRange>,
{
    proptest::collection::vec(ty_struct_member(k, v), length).prop_map(|members| {
        BoxAstNode::new(
            Span::new(0, 0),
            TyStruct {
                members: members.into_iter().collect(),
            },
        )
    })
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

pub fn any_ty_struct_with_literal_scalars() -> impl Strategy<Value = BoxAstNode> {
    literal_struct(any_ident(), any_ty_number_range_with_literal(), 0..12)
}

pub fn make_reference_ty(referenced_ty: BoxAstNode) -> BoxAstNode {
    BoxAstNode::new(Span::new(0, 0), TyRef { referenced_ty })
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

pub fn make_ty_slice(element_ty: BoxAstNode, length_ty: BoxAstNode) -> BoxAstNode {
    BoxAstNode::new(
        Span::new(0, 0),
        ast::TySlice {
            element_ty,
            length_ty,
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

pub fn must_parse(src: &str) -> (DefaultLinearTreeId, Tree<AstNode>) {
    let lexerdef = howlite_syntax::lexerdef();
    let lexer = lexerdef.lexer(src);
    let tree_builder: TreeBuilder<_> = TreeBuilder::default();
    let (root, errs) = howlite_syntax::parse(&lexer, &tree_builder);
    for e in &errs {
        println!("{}", e.pp(&lexer, &howlite_syntax::token_epp));
    }
    assert!(errs.is_empty());
    let root_node = root.unwrap().unwrap();

    (root_node, tree_builder.finalize())
}

pub fn any_ty_number_range_with_literal() -> impl Strategy<Value = BoxAstNode> {
    (0..u64::MAX as i128, 0..u64::MAX as i128).prop_map(|(a, b)| make_ty_number_range(a, b))
}

pub fn any_ident() -> impl Strategy<Value = String> {
    any_with::<String>(StringParam::from("[_a-zA-Z][_a-zA-Z0-9]*"))
}
