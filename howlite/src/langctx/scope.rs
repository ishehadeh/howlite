use std::rc::Rc;

use hashbrown::{HashMap, HashSet};
use howlite_typecheck::{Ty, TyBinder};
use smallvec::SmallVec;
use tracing::debug;

use crate::{symtab::Symbol, CompilationErrorKind};

use super::lexicalctx::LexicalContext;

#[derive(Debug, Default)]
pub struct Scope {
    /// List of local variable definitions.
    /// There can be duplicate symbols if a symbol is redefined
    /// the list MUST be in the order variables are defined
    pub locals: Vec<(Symbol, VarDef)>,
    pub mutates: Vec<Symbol>,
    pub narrows: HashMap<Symbol, Rc<Ty<Symbol>>>,

    /// List of local type definitions.
    /// There can be duplicate symbols if a symbol is redefined
    /// the list MUST be in the order variables are defined
    pub tys: Vec<TyDef>,

    /// List of local function definitions
    pub funcs: Vec<FuncDef>,

    pub catch_returns: bool,
    pub return_tys: Vec<Rc<Ty<Symbol>>>,
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

    pub fn get_func(&self, name: Symbol) -> Option<&FuncDef> {
        self.funcs.iter().rev().find(|f| f.name == name)
    }

    pub fn get_func_mut(&mut self, name: Symbol) -> Option<&mut FuncDef> {
        self.funcs.iter_mut().rev().find(|f| f.name == name)
    }
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: Symbol,
    pub params: Vec<Rc<Ty<Symbol>>>,
    pub ty_params: Vec<(Symbol, Rc<Ty<Symbol>>)>,
    pub returns: Rc<Ty<Symbol>>,
}

impl FuncDef {
    /// check the given params after instaitiating ty_params, then return the instantiated return ty
    pub fn check_params(
        &self,
        ctx: &LexicalContext,
        ty_params: &[Rc<Ty<Symbol>>],
        params: &[Rc<Ty<Symbol>>],
    ) -> Rc<Ty<Symbol>> {
        if ty_params.len() != self.ty_params.len() {
            ctx.error(CompilationErrorKind::IncorrectTyParamCount {
                expected: self.params.len(),
                got: params.len(),
                ty: self.name,
            });
        };

        if params.len() != self.params.len() {
            ctx.error(CompilationErrorKind::IncorrectParamCount {
                expected: self.params.len(),
                got: params.len(),
                func: ctx.sym_stringify(self.name),
            });
        };

        let mut mapped_params: SmallVec<[(Symbol, Rc<Ty<Symbol>>); 4]> =
            SmallVec::with_capacity(self.params.len());
        for (i, given_ty) in ty_params.iter().enumerate() {
            if i >= self.ty_params.len() {
                break;
            }
            if let Err(e) = given_ty.is_assignable_to(&*self.ty_params[i].1) {
                ctx.error(CompilationErrorKind::InvalidTyParam {
                    param: self.ty_params[i].0,
                    source: e,
                    ty: self.name,
                });
                debug!(param_ty = ?self.ty_params[i].1, ?given_ty, "bad type parameter: given_ty not assignable to param_ty");
                mapped_params.push((self.ty_params[i].0, Rc::new(Ty::Hole)))
            } else {
                debug!(name_sym = ?self.ty_params[i].0, ?given_ty, "adding mapped type param");
                mapped_params.push((self.ty_params[i].0, given_ty.clone()))
            }
        }

        let mut bound_params = Vec::with_capacity(self.params.len() + 1);
        for param in self.params.iter().chain([&self.returns]) {
            let mut binder = TyBinder::new(&mapped_params);
            let bound = binder.bind(param.clone());
            for err in binder.errors() {
                ctx.error(err.clone().into());
            }
            bound_params.push(bound)
        }

        let bound_returns = bound_params.pop().unwrap();
        for (bound_param, param) in bound_params.iter().zip(params) {
            if let Err(e) = param.is_assignable_to(bound_param) {
                ctx.error(CompilationErrorKind::InvalidParam {
                    source: e,
                    got: param.clone(),
                    func: ctx.sym_stringify(self.name),
                });
            }
        }

        bound_returns
    }
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub assumed_ty: Rc<Ty<Symbol>>,
    pub last_assignment: Rc<Ty<Symbol>>,
    pub is_mutable: bool,
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
        if params.is_empty() {
            return self.ty.clone();
        }

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
                debug!(param_ty = ?self.params[i].1, ?given_ty, "bad type parameter: given_ty not assignable to param_ty");
                mapped_params.push((self.params[i].0, Rc::new(Ty::Hole)))
            } else {
                debug!(name_sym = ?self.params[i].0, ?given_ty, "adding mapped type param");
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
