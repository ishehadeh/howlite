use std::rc::Rc;

use crate::{Symbol, Ty};

/// Used for type parameters - may be replaced by any subset of 'super_ty'
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TyLateBound<SymbolT: Symbol> {
    pub name: SymbolT,
    pub ty: Rc<Ty<SymbolT>>,
}
