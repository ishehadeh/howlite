use std::rc::Rc;

use smallvec::SmallVec;

use crate::{Symbol, Ty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyUnion<SymbolT: Symbol> {
    pub tys: SmallVec<[Rc<Ty<SymbolT>>; 8]>,
}
