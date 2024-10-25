use std::collections::HashMap;

use preseli::variables::VariableId;

use crate::{types::TyInt, Symbol};

#[derive(Debug)]
pub struct ConstraintContext<SymbolT: Symbol> {
    solver: preseli::environment::Environment,
    back_ref: HashMap<SymbolT, VariableId>,
}

impl<SymbolT: Symbol> Default for ConstraintContext<SymbolT> {
    fn default() -> Self {
        Self {
            solver: Default::default(),
            back_ref: Default::default(),
        }
    }
}

impl<SymbolT: Symbol> ConstraintContext<SymbolT> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new variable to the context. Variables may not be redeclared.
    pub fn introduce(&mut self, name: SymbolT, ty: TyInt) -> Result<(), ConstraintError<SymbolT>> {
        if self.back_ref.contains_key(&name) {
            return Err(ConstraintError::SymbolRedefined { symbol: name });
        }

        let preseli_id = self.solver.create_variable(ty.values);
        self.back_ref.insert(name, preseli_id);
        Ok(())
    }

    fn get_var(&self, symbol: SymbolT) -> Result<VariableId, ConstraintError<SymbolT>> {
        self.back_ref
            .get(&symbol)
            .copied()
            .ok_or(ConstraintError::SymbolUndefined { symbol })
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ConstraintError<SymbolT: Symbol> {
    #[error("symbol was previously defined: {:?}", symbol)]
    SymbolRedefined { symbol: SymbolT },

    #[error("symbol undefined: {:?}", symbol)]
    SymbolUndefined { symbol: SymbolT },
}
