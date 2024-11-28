use std::rc::Rc;

use thiserror::Error;
use tracing::debug;

use crate::{types::TyStruct, AccessPath, Symbol, Ty, TyArray, TyReference, TySlice};

/// Recursively binds types to a set of types identified by a symbol
pub struct TyBinder<'params, SymbolT: Symbol> {
    current_path: AccessPath<SymbolT>,
    errors: Vec<BindError<SymbolT>>,
    pub params: &'params [(SymbolT, Rc<Ty<SymbolT>>)],
}

impl<'params, SymbolT: Symbol> TyBinder<'params, SymbolT> {
    pub fn new(params: &'params [(SymbolT, Rc<Ty<SymbolT>>)]) -> Self {
        Self {
            params,
            current_path: AccessPath::default(),
            errors: Vec::new(),
        }
    }

    pub fn errors(&self) -> &[BindError<SymbolT>] {
        &self.errors
    }

    pub fn bind(&mut self, ty: Rc<Ty<SymbolT>>) -> Rc<Ty<SymbolT>> {
        match &*ty {
            Ty::Hole => ty,
            Ty::Int(_) => ty,
            Ty::Struct(s) => {
                let mut newstruc: Option<TyStruct<SymbolT>> = None;
                for (i, field) in s.fields.iter().enumerate() {
                    self.current_path.push_field(field.name.clone());
                    let bound = self.bind(field.ty.clone());
                    self.current_path.pop();
                    if !Rc::ptr_eq(&bound, &field.ty) && newstruc.is_none() {
                        newstruc = Some(s.clone());
                    }

                    if let Some(newstruc) = &mut newstruc {
                        newstruc.fields[i].ty = bound;
                    }
                }
                newstruc
                    .map(|struc| Rc::new(Ty::Struct(struc)))
                    .unwrap_or(ty)
            }
            Ty::Array(a) => {
                let bound = self.bind(a.element_ty.clone());
                if !Rc::ptr_eq(&bound, &a.element_ty) {
                    Rc::new(Ty::Array(TyArray {
                        length: a.length,
                        element_ty: bound,
                    }))
                } else {
                    ty
                }
            }
            Ty::Slice(a) => {
                let bound_element_ty = self.bind(a.element_ty.clone());
                let bound_index_set = self.bind(a.index_set.clone());
                if !Rc::ptr_eq(&bound_element_ty, &a.element_ty)
                    || !Rc::ptr_eq(&bound_index_set, &a.index_set)
                {
                    Rc::new(Ty::Slice(TySlice {
                        index_set: bound_index_set,
                        element_ty: bound_element_ty,
                    }))
                } else {
                    ty
                }
            }
            Ty::Reference(r) => {
                let bound = self.bind(r.referenced_ty.clone());
                if !Rc::ptr_eq(&bound, &r.referenced_ty) {
                    Rc::new(Ty::Reference(TyReference {
                        referenced_ty: bound,
                    }))
                } else {
                    ty
                }
            }
            Ty::Union(u) => {
                let mut newunion = None;
                for (i, sub_ty) in u.tys.iter().enumerate() {
                    let bound = self.bind(sub_ty.clone());
                    if !Rc::ptr_eq(&bound, sub_ty) && newunion.is_none() {
                        newunion = Some(u.clone());
                    }

                    if let Some(newunion) = &mut newunion {
                        newunion.tys[i] = bound;
                    }
                }
                newunion.map(|u| Rc::new(Ty::Union(u))).unwrap_or(ty)
            }
            Ty::LateBound(lb) => {
                if let Some(param) =
                    self.params
                        .iter()
                        .find_map(|(p_sym, ty)| if *p_sym == lb.name { Some(ty) } else { None })
                {
                    debug!(symbol = ?lb.name, bound_ty = ?&param, "binding type parameter");
                    // TODO: should we check the lb.super_ty?
                    param.clone()
                } else {
                    self.errors.push(BindError::UnboundIdentifier {
                        path: self.current_path.clone(),
                        symbol: lb.name.clone(),
                    });
                    debug!(symbol = ?lb.name, known = ?&self.params, "unknown late bound type name");
                    Rc::new(Ty::Hole)
                }
            }
        }
    }
}

#[derive(Error, Debug, Clone)]
pub enum BindError<SymbolT: Symbol> {
    #[error("unbound identifier {:?}", symbol)]
    UnboundIdentifier {
        path: AccessPath<SymbolT>,
        symbol: SymbolT,
    },
}
