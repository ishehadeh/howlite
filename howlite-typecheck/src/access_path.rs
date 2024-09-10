use preseli::integer::num_bigint::BigInt;
use smallvec::SmallVec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AccessPathElem<SymbolT: Eq> {
    ArrayAccess(BigInt),
    StructAccess(SymbolT),
}

/// A description, by symbol or index (for arrays) of a where a field can be found in a nested struct.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AccessPath<SymbolT: Eq> {
    elements: SmallVec<[AccessPathElem<SymbolT>; 4]>,
}

impl<SymbolT: Eq> Default for AccessPath<SymbolT> {
    fn default() -> Self {
        Self {
            elements: Default::default(),
        }
    }
}

impl<SymbolT: Eq> AccessPath<SymbolT> {
    pub fn field(mut self, name: SymbolT) -> AccessPath<SymbolT> {
        self.elements.push(AccessPathElem::StructAccess(name));
        self
    }

    pub fn index(mut self, index: impl Into<BigInt>) -> AccessPath<SymbolT> {
        self.elements
            .push(AccessPathElem::ArrayAccess(index.into()));
        self
    }
}
