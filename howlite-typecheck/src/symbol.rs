use std::{hash::Hash, fmt::Debug};


/// Symbols are used as identifiers within types. For example field names, or bindings
pub trait Symbol: Eq + Debug + Clone + Hash {}

impl<T> Symbol for T where T: Eq + Debug + Clone + Hash {}
