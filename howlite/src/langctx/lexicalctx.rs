use std::rc::Rc;

use howlite_typecheck::Ty;

use crate::{symtab::Symbol, CompilationError, CompilationErrorKind};

use super::{LangCtx, ScopeId, TyDef, VarDef};

#[derive(Debug, Clone)]
pub struct LexicalContext<'a, LocT: Clone> {
    pub(super) scope: ScopeId,
    pub(super) location: LocT,
    pub(super) parent: &'a LangCtx<LocT>,
}

impl<'a, LocT: Clone> LexicalContext<'a, LocT> {
    pub fn with_location(mut self, location: LocT) -> Self {
        self.location = location;
        self
    }

    pub fn with_scope(mut self) -> Self {
        self.scope = self.parent.scope_new(self.scope);
        self
    }

    pub fn get_scope(&self) -> ScopeId {
        self.scope.clone()
    }

    pub fn error(&self, err: CompilationErrorKind) {
        self.parent.error(CompilationError {
            location: self.location.clone(),
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
