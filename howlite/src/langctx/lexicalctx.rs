use std::rc::Rc;

use aries::utils::input::Sym;
use howlite_syntax::{tree::DefaultLinearTreeId, AstNode};
use howlite_typecheck::Ty;
use smol_str::SmolStr;

use crate::{symtab::Symbol, typetree::SynthesizeTy, CompilationError, CompilationErrorKind};

use super::{FuncDef, LangCtx, ScopeId, TyDef, VarDef};

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

    pub fn narrow(&self, name: Symbol, ty: Rc<Ty<Symbol>>) {
        let scope_id = self.get_scope();
        self.parent
            .scopes
            .get_mut(&scope_id)
            .unwrap()
            .narrows
            .insert(name, ty);
    }

    pub fn get_current_var_ty(&self, name: Symbol) -> Option<Rc<Ty<Symbol>>> {
        let mut scope_id = self.get_scope();
        loop {
            if let Some(ty) = self
                .parent
                .scopes
                .get(&scope_id)
                .unwrap()
                .narrows.get(&name)
            {
                return Some(ty.clone());
            }
            if let Some(def) = self.parent.scopes.get(&scope_id).unwrap().get_local(name) {
                return Some(def.last_assignment.clone());
            }
            if let Some(p) = self.parent.scope_parent(scope_id) {
                scope_id = p;
            } else {
                return None;
            }
        }
    }

    pub fn clear_assigned_tys(&self) {
        let mut scope_id = self.get_scope();
        loop {
            let var_assumed_tys: Vec<_> = self
                .parent
                .scopes
                .get(&scope_id)
                .unwrap()
                .locals
                .iter()
                .map(|f| (f.0, f.1.assumed_ty.clone()))
                .collect();
            let mut scope = self.parent
                .scopes
                .get_mut(&self.scope)
                .unwrap();
            for (var, ty) in var_assumed_tys.into_iter() {
                scope.narrows.try_insert(var, ty);
            }
            if let Some(p) = self.parent.scope_parent(scope_id) {
                scope_id = p;
            } else {
                break;
            }
        }
    }

    pub fn get_current_var_ty_or_err(&self, name: Symbol) -> Rc<Ty<Symbol>> {
        match self.get_current_var_ty(name) {
            Some(v) => v,
            None => {
                self.error(CompilationErrorKind::UnknownVariable {
                    name: self
                        .parent
                        .symbols
                        .stringify(name)
                        .expect("symbol table lookup failed"),
                });
                Rc::new(Ty::Hole)
            }
        }
    }

    pub fn node(&self) -> DefaultLinearTreeId {
        self.node
    }

    pub fn new_with_scope(&self) -> Self {
        Self {
            scope: self.parent.scope_new(self.scope),
            parent: self.parent,
            node: self.node,
        }
    }

    pub fn new_with_scope_catch_returns(&self) -> Self {
        Self {
            scope: self.parent.scope_new_catch(self.scope, true),
            parent: self.parent,
            node: self.node,
        }
    }

    pub fn node_data(&self) -> &AstNode {
        self.get_node(self.node)
    }

    pub fn add_return_ty(&self, ty: Rc<Ty<Symbol>>) {
        let mut scope_id = self.scope;
        loop {
            if self.parent.scopes.get(&scope_id).unwrap().catch_returns {
                break;
            }
            if let Some(p) = self.parent.scope_parent(scope_id) {
                scope_id = p;
            } else {
                todo!("gracefully handle out-of-context returns");
            }
        }

        let mut scope = self.parent.scopes.get_mut(&scope_id).unwrap();
        scope.return_tys.push(ty);
    }

    pub fn scope_return_tys(&self) -> Vec<Rc<Ty<Symbol>>> {
        self.parent
            .scopes
            .get(&self.scope)
            .unwrap()
            .return_tys
            .clone()
    }

    pub fn get_node(&self, id: DefaultLinearTreeId) -> &AstNode {
        self.parent.ast.get(id)
    }

    pub fn get_scope(&self) -> ScopeId {
        self.scope
    }

    pub fn synthesize_ty(&self) -> Rc<Ty<Symbol>> {
        self.node_data().data.synthesize_ty(self)
    }

    pub fn error(&self, err: CompilationErrorKind) {
        self.parent.error(CompilationError {
            location: self.node_data().span,
            kind: err,
        });
    }

    pub fn sym_intern(&self, name: &str) -> Symbol {
        self.parent.symbols.intern(name)
    }

    pub fn sym_stringify(&self, sym: Symbol) -> SmolStr {
        self.parent.symbols.stringify(sym).unwrap()
    }

    pub fn var_get(&self, name: Symbol) -> Option<VarDef> {
        self.parent.var_get(self.scope, name)
    }

    pub fn var_update(&self, name: Symbol, f: impl FnOnce(VarDef) -> VarDef) -> bool {
        self.parent.var_update(self.scope, name, f)
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

    pub fn func_get(&self, name: Symbol) -> Option<FuncDef> {
        self.parent.func_get(self.scope, name)
    }

    pub fn func_get_or_err(&self, name: Symbol) -> FuncDef {
        match self.func_get(name) {
            Some(v) => v,
            None => {
                self.error(CompilationErrorKind::UnknownVariable {
                    name: self
                        .parent
                        .symbols
                        .stringify(name)
                        .expect("symbol table lookup failed"),
                });
                FuncDef {
                    name,
                    params: vec![],
                    ty_params: vec![],
                    returns: Ty::Hole.into(),
                }
            }
        }
    }

    pub fn func_def(&self, def: FuncDef) {
        self.parent.func_def(self.scope, def);
    }
}
