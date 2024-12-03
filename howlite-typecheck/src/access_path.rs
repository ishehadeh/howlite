use crate::Symbol;
use preseli::integer::Scalar;
use smallvec::SmallVec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AccessPathElem<SymbolT: Symbol> {
    ArrayAccess(Scalar),
    Deref,
    StructAccess(SymbolT),
}

/// A description, by symbol or index (for arrays) of a where a field can be found in a nested struct.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AccessPath<SymbolT: Symbol> {
    elements: SmallVec<[AccessPathElem<SymbolT>; 4]>,
}

impl<SymbolT: Symbol> Default for AccessPath<SymbolT> {
    fn default() -> Self {
        Self {
            elements: Default::default(),
        }
    }
}

impl<SymbolT: Symbol> AccessPath<SymbolT> {
    pub fn field(mut self, name: SymbolT) -> Self {
        self.push_field(name);
        self
    }

    pub fn as_slice(&self) -> &[AccessPathElem<SymbolT>] {
        self.elements.as_slice()
    }

    pub fn push_deref(&mut self) {
        self.elements.push(AccessPathElem::Deref);
    }

    pub fn index(mut self, index: impl Into<Scalar>) -> Self {
        self.push_index(index);
        self
    }

    pub fn push_field(&mut self, name: SymbolT) {
        self.elements.push(AccessPathElem::StructAccess(name));
    }

    pub fn push_index(&mut self, index: impl Into<Scalar>) {
        self.elements
            .push(AccessPathElem::ArrayAccess(index.into()));
    }

    pub fn pop(&mut self) -> Option<AccessPathElem<SymbolT>> {
        self.elements.pop()
    }
}
