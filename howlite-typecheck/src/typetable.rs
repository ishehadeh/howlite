use elsa::FrozenIndexSet;

use crate::types;

pub struct TypeTable {
    scalars: FrozenIndexSet<types::TyInt>,
}
