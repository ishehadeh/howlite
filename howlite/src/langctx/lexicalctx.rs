use std::rc::Rc;

use howlite_syntax::{tree::DefaultLinearTreeId, AstNode};
use howlite_typecheck::Ty;

use crate::{symtab::Symbol, typetree::SynthesizeTy, CompilationError, CompilationErrorKind};

use super::{LangCtx, ScopeId, TyDef, VarDef};

#[derive(Debug, Clone)]
pub struct LexicalContext<'a, 'b> {
    pub(super) scope: ScopeId,
    pub(super) parent: &'a LangCtx<'b>,
    pub(super) node: DefaultLinearTreeId,
}

impl<'a, 'b> LexicalContext<'a, 'b> {
    pub fn child(&self, child_node_id: DefaultLinearTreeId) -> Self {
        Self {
            scope: self.scope,
            parent: self.parent,
            node: child_node_id,
        }
    }

    pub fn new_with_scope(&self) -> Self {
        Self {
            scope: self.parent.scope_new(self.scope),
            parent: self.parent,
            node: self.node,
        }
    }

    pub fn node_data(&self) -> &AstNode {
        self.get_node(self.node)
    }

    pub fn get_node(&self, id: DefaultLinearTreeId) -> &AstNode {
        self.parent.ast.get(id)
    }

    pub fn get_scope(&self) -> ScopeId {
        self.scope.clone()
    }

    pub fn synthesize_ty(&self) -> Rc<Ty<Symbol>> {
        self.node_data().data.synthesize_ty(self)
    }

    pub fn error(&self, err: CompilationErrorKind) {
        self.parent.error(CompilationError {
            location: self.node_data().span.clone(),
            kind: err,
        });
    }

    pub fn sym_intern(&self, name: &str) -> Symbol {
        self.parent.symbols.intern(name)
    }

    pub fn var_get(&self, name: Symbol) -> Option<VarDef> {
        self.parent.var_get(self.scope, name)
    }

    pub fn var_get_or_err(&self, name: Symbol) -> VarDef {
        match self.var_get(name) {
            Some(v) => v,
            None => {
                self.error(CompilationErrorKind::UnknownVariable {
                    name: self
                        .parent
                        .symbols
                        .stringify(name)
                        .expect("symbol table lookup failed"),
                });
                let hole = Rc::new(Ty::Hole);
                VarDef {
                    assumed_ty: hole.clone(),
                    last_assignment: hole,
                    is_mutable: true,
                }
            }
        }
    }

    pub fn var_def(&self, name: Symbol, def: VarDef) {
        self.parent.var_def(self.scope, name, def);
    }

    pub fn ty_get(&self, name: Symbol) -> Option<TyDef> {
        self.parent.ty_get(self.scope, name)
    }

    pub fn ty_get_or_err(&self, name: Symbol) -> TyDef {
        match self.ty_get(name) {
            Some(v) => v,
            None => {
                self.error(CompilationErrorKind::UnknownType {
                    name: self
                        .parent
                        .symbols
                        .stringify(name)
                        .expect("symbol table lookup failed"),
                });
                TyDef {
                    name,
                    params: Default::default(),
                    ty: Rc::new(Ty::Hole),
                }
            }
        }
    }

    pub fn ty_def(&self, def: TyDef) {
        self.parent.ty_def(self.scope, def);
    }
}
