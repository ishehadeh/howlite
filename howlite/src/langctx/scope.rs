use std::rc::Rc;

use howlite_typecheck::{Ty, TyBinder};
use smallvec::SmallVec;

use crate::{symtab::Symbol, CompilationErrorKind};

use super::lexicalctx::LexicalContext;

#[derive(Debug, Default)]
pub struct Scope {
    /// List of local variable definitions.
    /// There can be duplicate symbols if a symbol is redefined
    /// the list MUST be in the order variables are defined
    pub locals: Vec<(Symbol, VarDef)>,

    /// List of local variable definitions.
    /// There can be duplicate symbols if a symbol is redefined
    /// the list MUST be in the order variables are defined
    pub tys: Vec<TyDef>,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub assumed_ty: Rc<Ty<Symbol>>,
    pub last_assignment: Rc<Ty<Symbol>>,
}

#[derive(Debug, Clone)]
pub struct TyDef {
    pub name: Symbol,
    pub params: Vec<(Symbol, Rc<Ty<Symbol>>)>,
    pub ty: Rc<Ty<Symbol>>,
}

impl TyDef {
    pub fn instantiate(&self, ctx: &LexicalContext, params: &[Rc<Ty<Symbol>>]) -> Rc<Ty<Symbol>> {
        if params.len() != self.params.len() {
            ctx.error(CompilationErrorKind::IncorrectTyParamCount {
                expected: self.params.len(),
                got: params.len(),
                ty: self.name,
            });
        };

        let mut mapped_params: SmallVec<[(Symbol, Rc<Ty<Symbol>>); 4]> =
            SmallVec::with_capacity(self.params.len());
        for (i, given_ty) in params.iter().enumerate() {
            if i >= self.params.len() {
                break;
            }
            if let Err(e) = given_ty.is_assignable_to(&*self.params[i].1) {
                ctx.error(CompilationErrorKind::InvalidTyParam {
                    param: self.params[i].0,
                    source: e,
                    ty: self.name,
                });
                mapped_params.push((self.params[i].0, Rc::new(Ty::Hole)))
            } else {
                mapped_params.push((self.params[i].0, given_ty.clone()))
            }
        }

        let mut bind = TyBinder::new(&mapped_params);
        let bound = bind.bind(self.ty.clone());
        for err in bind.errors() {
            ctx.error(err.clone().into());
        }

        bound
    }
}

impl Scope {
    pub fn get_local(&self, name: Symbol) -> Option<&VarDef> {
        self.locals
            .iter()
            .rev()
            .find(|(l_name, _)| *l_name == name)
            .map(|(_, var)| var)
    }

    pub fn get_local_mut(&mut self, name: Symbol) -> Option<&mut VarDef> {
        self.locals
            .iter_mut()
            .rev()
            .find(|(l_name, _)| *l_name == name)
            .map(|(_, var)| var)
    }

    pub fn get_ty(&self, name: Symbol) -> Option<&TyDef> {
        self.tys.iter().rev().find(|ty| ty.name == name)
    }

    pub fn get_ty_mut(&mut self, name: Symbol) -> Option<&mut TyDef> {
        self.tys.iter_mut().rev().find(|ty| ty.name == name)
    }
}
