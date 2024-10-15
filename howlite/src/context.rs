use std::collections::BTreeMap;

use howlite_syntax::{tree::NodeId, AstNode};
use howlite_typecheck::Ty;
use preseli::integer::Scalar;

use crate::{symtab::Symbol, typetree::TypeError};
pub struct TyContext {
    scopes: Vec<Scope>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    variables: BTreeMap<Symbol, TypeInfo>,
}

#[derive(Debug, Clone)]
pub struct Implication {
    value: Scalar,
    generation: usize,
    updates: Vec<(Accessor, Ty<Symbol>)>,
}

/// A path to a particular value within a TyContext.
/// This is a lower-level translation of path expressions: "a.b.c", "a[2].c"
#[derive(Debug, Clone)]
pub struct Accessor {
    pub base: Symbol,
    pub pat: Vec<AccessorElement>,
}

#[derive(Debug, Clone)]
pub enum AccessorElement {
    Member(Symbol),

    /// Assumes element is a reference - dereference it
    Deref,

    Index(Scalar),
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub ty_assumed: Option<Ty<Symbol>>,
    pub ty_synthesized: Ty<Symbol>,
    pub error: Option<TypeError>,

    // incremented if the synthesized type changes.
    pub generation: usize,
    pub inherited_errors: smallvec::SmallVec<[NodeId<AstNode>; 4]>,
    pub implications: Vec<Implication>,
}

impl TypeInfo {
    pub fn new(value_ty: Ty<Symbol>) -> Self {
        Self {
            ty_assumed: None,
            ty_synthesized: value_ty,
            error: None,
            generation: 0,
            inherited_errors: smallvec::SmallVec::new(),
            implications: Vec::new(),
        }
    }

    pub fn synthesize_from<const N: usize, F>(infos: [&TypeInfo; N], f: F) -> TypeInfo
    where
        F: FnOnce([&Ty<Symbol>; N]) -> Result<Ty<Symbol>, (Ty<Symbol>, TypeError)>,
    {
        let mut synth_tys: [&Ty<Symbol>; N] = [&Ty::Hole; N];
        for i in 0..N {
            synth_tys[i] = &infos[i].ty_synthesized
        }

        let ty_result = f(synth_tys);
        match ty_result {
            Ok(synth) => TypeInfo {
                ty_assumed: None,
                ty_synthesized: synth,
                error: None,
                implications: Vec::new(),
                generation: 0,
                inherited_errors: infos
                    .iter()
                    .flat_map(|t| t.inherited_errors.iter().cloned())
                    .collect(),
            },
            Err((synth, error)) => TypeInfo {
                ty_assumed: None,
                ty_synthesized: synth,
                implications: Vec::new(),
                error: Some(error),
                generation: 0,
                inherited_errors: infos
                    .iter()
                    .flat_map(|t| t.inherited_errors.iter().cloned())
                    .collect(),
            },
        }
    }
}
