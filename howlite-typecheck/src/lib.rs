pub use preseli::IntegerSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyId {
    index: usize,
}

#[derive(Debug, Clone)]
pub struct TyInt {
    pub values: IntegerSet,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct TyStruct {
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct TyArray {
    pub length: usize,
    pub element_ty: TyId,
}

#[derive(Debug, Clone)]
pub struct TySlice {
    pub length_ty: TyId,
    pub element_ty: TyId,
}

#[derive(Debug, Clone)]
pub struct TyReference {
    pub referenced_ty: TyId,
}
#[derive(Debug, Clone)]
pub struct TyUnion {
    pub tys: Vec<TyId>,
}

#[derive(Debug, Clone)]
pub enum Ty {
    Int(TyInt),
    Struct(TyStruct),
    Array(TyArray),
    Slice(TySlice),
    Reference(TyReference),
    Union(TyUnion),
}

#[derive(Default)]
pub struct TyStore {
    tys: Vec<Ty>,
}

impl TyStore {
    pub fn add(&mut self, ty: Ty) -> TyId {
        let id = TyId {
            index: self.tys.len(),
        };
        self.tys.push(ty);
        id
    }

    pub fn get(&self, id: TyId) -> &Ty {
        // TODO:(ian, low) it may be worth having a debug only check that this ID does belong to this tree
        &self.tys[id.index]
    }

    pub fn is_subset(&self, superset_id: TyId, subset_id: TyId) -> bool {
        let superset = self.get(superset_id);
        let subset = self.get(subset_id);

        match (superset, subset) {
            (Ty::Int(sup), Ty::Int(sub)) => sub.values.is_subset_of(&sup.values),
            (Ty::Int(_), _) => false,
            (Ty::Struct(_), Ty::Struct(_)) => todo!(),
            (Ty::Struct(_), _) => false,
            (Ty::Array(_), Ty::Array(_)) => todo!(),
            (Ty::Array(_), _) => false,
            (Ty::Slice(_), Ty::Slice(_)) => todo!(),
            (Ty::Slice(_), _) => todo!(),
            (Ty::Reference(_), Ty::Reference(_)) => todo!(),
            (Ty::Reference(_), _) => false,
            (Ty::Union(_), Ty::Int(_)) => todo!(),
            (Ty::Union(_), Ty::Struct(_)) => todo!(),
            (Ty::Union(_), Ty::Array(_)) => todo!(),
            (Ty::Union(_), Ty::Slice(_)) => todo!(),
            (Ty::Union(_), Ty::Reference(_)) => todo!(),
            (Ty::Union(_), Ty::Union(_)) => todo!(),
        }
    }
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use preseli::{iset, IntegerSet};

    use crate::{Ty, TyInt, TyStore};

    #[test]
    fn ty_store_simple() {
        let mut store = TyStore::default();
        let id = store.add(Ty::Int(TyInt {
            values: iset!(0..5, 10),
        }));

        if let Ty::Int(i) = store.get(id) {
            assert_eq!(i.values, iset!(0..5, 10))
        } else {
            panic!("expected integer type, got {:?}", store.get(id))
        }
    }
}
