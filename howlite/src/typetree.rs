use std::rc::Rc;

use bumpalo::Bump;
use hashbrown::HashMap;
use howlite_syntax::{
    ast::{ExprInfix, InfixOp, LiteralInteger, Program},
    tree::{NodeId, Tree, TreeBuilder},
    AstNode, AstNodeData, Span,
};
use howlite_typecheck::{
    errors::{IncompatibleError, OperationError},
    types::TyInt,
    Ty,
};
use slotmap::{new_key_type, Key, SlotMap};

use crate::{
    context::TypeInfo,
    symtab::{Symbol, SymbolTable},
};

new_key_type! { struct ErrorRef; }
new_key_type! { struct ScopeRef; }

pub struct LanguageContext<'src, 'ast> {
    src: &'src str,
    ast: &'ast Tree<AstNode>,
    symbols: SymbolTable<'src>,
    errors: slotmap::SlotMap<ErrorRef, TypeError>,
    scopes: slotmap::SlotMap<ScopeRef, Scope>,
    root_scope: ScopeRef,
    synthesized: HashMap<NodeId<AstNode>, Rc<Ty<Symbol>>>,
}

impl<'src, 'ast> LanguageContext<'src, 'ast> {
    pub fn new(src: &'src str, ast: &'ast Tree<AstNode>) -> Self {
        let mut scopes = SlotMap::with_key();
        let root_scope = scopes.insert(Scope {
            parent: ScopeRef::null(),
            locals: Default::default(),
        });
        Self {
            src,
            ast,
            symbols: SymbolTable::new(),
            errors: SlotMap::with_key(),
            scopes,
            root_scope,
            synthesized: HashMap::with_capacity(ast.node_count()),
        }
    }

    pub fn root_scope(&self) -> ScopeRef {
        self.root_scope
    }

    pub fn synthesize(&mut self, scope: ScopeRef, node: NodeId<AstNode>) -> Rc<Ty<Symbol>> {
        let node_data = self.ast.get(node.clone());
        let mut ctx = SynthesizerContext {
            lang_ctx: self,
            scope,
            node,
        };

        node_data.synthesize_ty(&mut ctx)
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    data: TypeErrorData,
    node: NodeId<AstNode>,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum TypeErrorData {
    #[error("operand(s) are invalid: {}", _0)]
    OpError(#[from] OperationError<Symbol>),

    #[error("invalid assignment: {}", _0)]
    AssignError(#[from] IncompatibleError<Symbol>),

    #[error("cannot assign to a value-type")]
    MissingDeclaredType,
}

#[derive(Debug, Clone)]
pub struct Scope {
    parent: ScopeRef,
    locals: Vec<(Symbol, TypeInfo)>,
}

pub struct SynthesizerContext<'lang, 'src, 'ast> {
    lang_ctx: &'lang mut LanguageContext<'src, 'ast>,
    scope: ScopeRef,
    node: NodeId<AstNode>,
}

impl<'lang, 'src, 'ast> SynthesizerContext<'lang, 'src, 'ast> {
    pub fn error(&mut self, data: TypeErrorData) -> ErrorRef {
        self.lang_ctx.errors.insert(TypeError {
            data,
            node: self.node.clone(),
        })
    }

    pub fn hole(&self) -> Rc<Ty<Symbol>> {
        thread_local! {
            static HOLE: Rc<Ty<Symbol>> = Rc::new(Ty::Hole);
        }

        HOLE.with(|h| h.clone())
    }

    pub fn depend(&mut self, node: NodeId<AstNode>) -> Rc<Ty<Symbol>> {
        if let Some(t) = self.lang_ctx.synthesized.get(&node) {
            t.clone()
        } else {
            let node_data = self.lang_ctx.ast.get(node.clone());
            let mut new_ctx = SynthesizerContext {
                lang_ctx: self.lang_ctx,
                scope: self.scope,
                node: node.clone(),
            };
            let t = node_data.synthesize_ty(&mut new_ctx);
            self.lang_ctx.synthesized.insert(node, t.clone());
            t
        }
    }
}

trait SynthesizeTy {
    fn synthesize_ty(&self, ctx: &mut SynthesizerContext) -> Rc<Ty<Symbol>>;
}

impl SynthesizeTy for AstNode {
    fn synthesize_ty(&self, ctx: &mut SynthesizerContext) -> Rc<Ty<Symbol>> {
        match &self.data {
            AstNodeData::LiteralInteger(n) => n.synthesize_ty(ctx),
            AstNodeData::ExprInfix(n) => n.synthesize_ty(ctx),
            _ => todo!(),
        }
    }
}

impl SynthesizeTy for LiteralInteger {
    fn synthesize_ty(&self, _: &mut SynthesizerContext) -> Rc<Ty<Symbol>> {
        Rc::new(Ty::Int(TyInt::single(self.value)))
    }
}

impl SynthesizeTy for ExprInfix {
    fn synthesize_ty(&self, ctx: &mut SynthesizerContext) -> Rc<Ty<Symbol>> {
        match self.op {
            InfixOp::Add => {
                let lhs = ctx.depend(self.lhs.clone());
                let rhs = ctx.depend(self.rhs.clone());
                match lhs.arith_add(&rhs) {
                    Ok(v) => Rc::new(v),
                    Err(e) => {
                        ctx.error(e.into());
                        ctx.hole()
                    }
                }
            }
            _ => todo!(),
        }
    }
}

fn must_parse_expr<'a>(
    test_alloc: &'a bumpalo::Bump,
    src: &str,
) -> (NodeId<AstNode>, LanguageContext<'a, 'a>) {
    use howlite_syntax::lex_and_parse;
    let full_src = test_alloc.alloc(format!("func main(): 0 {{{};}}", src));
    let (tree, root, errors) = lex_and_parse(full_src);
    if !errors.is_empty() {
        panic!("ERRORS: {errors:#?}")
    }

    let Ok(AstNode {
        data: AstNodeData::Program(program),
        ..
    }) = root.map(|n| tree.get(n))
    else {
        panic!("error in program")
    };

    let expr_root =
        if let AstNodeData::DefFunc(deffunc) = &tree.get(program.definitions[0].clone()).data {
            if let AstNodeData::Block(block) = &tree.get(deffunc.body.clone()).data {
                block.statements[0].clone()
            } else {
                unreachable!("expected block, got: {:#?}", tree.get(deffunc.body.clone()))
            }
        } else {
            unreachable!("expected func, got: {:#?}", program.definitions[0])
        };

    let tree = test_alloc.alloc(tree);

    (expr_root, LanguageContext::new(full_src.as_str(), tree))
}

#[test]
fn test_simple() {
    let alloc = Bump::new();
    let (expr_node, mut lang_ctx) = must_parse_expr(&alloc, "3 + 2");

    let add_ty = lang_ctx.synthesize(lang_ctx.root_scope(), expr_node.clone());
    assert_eq!(add_ty.as_int().unwrap().clone(), TyInt::single(5));
}
