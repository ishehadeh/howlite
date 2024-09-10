use std::rc::Rc;

use super::AccessPath;
use smallvec::{smallvec, SmallVec};

use crate::{Symbol, Ty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField<SymbolT: Symbol> {
    pub name: SymbolT,
    pub ty: Ty<SymbolT>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyStruct<SymbolT: Symbol> {
    pub fields: SmallVec<[StructField<SymbolT>; 8]>,
}

impl<SymbolT: Symbol> TyStruct<SymbolT> {
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &StructField<SymbolT>> {
        self.fields.iter()
    }
}

impl<SymbolT: Symbol> TyStruct<SymbolT> {
    pub fn cursor(rc: Rc<Self>) -> StructCursor<SymbolT> {
        StructCursor {
            past: smallvec![(0, rc)],
            next_index: 0,
        }
    }

    pub fn accessible_intersect(first: Rc<Self>, second: Rc<Self>) -> Vec<AccessPath<SymbolT>> {
        // TODO: accessible_intersect could return an iterator
        let mut acc = Vec::new();

        let mut cursor_l = TyStruct::cursor(first);
        let mut cursor_r = TyStruct::cursor(second);

        let mut l_offset = 0;
        let mut r_offset = 0;
        while let (Some(path_l), Some(path_r)) = (cursor_l.advance(), cursor_r.advance()) {
            let (l_index, l_struc) = path_l.last().unwrap();
            let (r_index, r_struc) = path_r.last().unwrap();
            let l_field = &l_struc.fields[*l_index];
            let r_field = &r_struc.fields[*r_index];

            let l_field_size = l_field.ty.sizeof();
            let r_field_size = r_field.ty.sizeof();

            if l_field_size == r_field_size && l_offset == r_offset {
                let l_access =
                    path_l
                        .iter()
                        .fold(AccessPath::default(), |path, (field_index, struc)| {
                            path.field(struc.fields[*field_index].name.clone())
                        });

                // TODO: since we only care that the access paths are identical, we can just check r matches l without building both full paths.
                let r_access =
                    path_r
                        .iter()
                        .fold(AccessPath::default(), |path, (field_index, struc)| {
                            path.field(struc.fields[*field_index].name.clone())
                        });
                if l_access == r_access {
                    acc.push(l_access);
                }
            }

            l_offset += l_field_size;
            r_offset += r_field_size;
        }

        acc
    }
}

pub struct StructCursor<SymbolT: Symbol> {
    past: SmallVec<[(usize, Rc<TyStruct<SymbolT>>); 4]>,
    next_index: usize,
}

impl<SymbolT: Symbol> StructCursor<SymbolT> {
    pub fn advance(&mut self) -> Option<&[(usize, Rc<TyStruct<SymbolT>>)]> {
        // update the history with the last returned index

        match self.past.last_mut() {
            None => None,
            Some((_, struc)) if self.next_index >= struc.fields.len() => {
                self.past.pop();

                // restore the old index, and advance to the next field
                if let Some(&(hist_index, _)) = self.past.last() {
                    self.next_index = hist_index + 1
                }
                self.advance()
            }
            Some((n, struc)) => {
                // advance
                *n = self.next_index;

                if let Some(child_struc) = struc.fields[*n].ty.as_struct() {
                    self.next_index = 0;
                    self.past.push((0, child_struc));

                    self.advance()
                } else {
                    *n = self.next_index;
                    self.next_index += 1;
                    Some(&self.past)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use preseli::iset;

    use crate::{t_array, t_int, t_struct, ty_struct::AccessPath, TyStruct};

    #[test]
    pub fn construction() {
        let struc_ty = t_struct! {
            "a" => t_int!(0..10),
            "b" => t_int!(0..100),
        };
        let struc = struc_ty.as_struct().expect("didn't construct a struct?");
        assert_eq!(struc.fields[0].name, "a");
        assert_eq!(struc.fields[1].name, "b");
        assert_eq!(struc.fields[0].ty.as_int().unwrap().values, iset!(0..10));
        assert_eq!(struc.fields[1].ty.as_int().unwrap().values, iset!(0..100));
    }

    #[test]
    pub fn cursor() {
        let b_struc_ty = t_struct! {
            "c" => t_int!(0),
            "d" => t_int!(1),
        };
        let struc_ty = t_struct! {
            "a" => t_int!(0..10),
            "b" => b_struc_ty.clone(),
            "e" => t_int!(2),
        };
        let struc = struc_ty.as_struct().expect("didn't construct a struct?");
        let b_struc = b_struc_ty.as_struct().unwrap();
        let mut cursor = TyStruct::cursor(struc.clone());

        assert_eq!(cursor.advance().unwrap(), [(0, struc.clone())]);
        assert_eq!(
            cursor.advance().unwrap(),
            [(1, struc.clone()), (0, b_struc.clone())]
        );
        assert_eq!(
            cursor.advance().unwrap(),
            [(1, struc.clone()), (1, b_struc)]
        );
        assert_eq!(cursor.advance().unwrap(), [(2, struc)]);
    }

    #[test]
    pub fn access_intersect() {
        let b_struc_ty = t_struct! {
            "a" => t_int!(0),
            "d" => t_int!(1),
        };
        let a_struc_ty = t_struct! {
            "a" => t_int!(0..10),
            "e" => t_int!(2),
        };
        let b_struc: std::rc::Rc<TyStruct<&str>> =
            b_struc_ty.as_struct().expect("didn't construct a struct?");
        let a_struc: std::rc::Rc<TyStruct<&str>> =
            a_struc_ty.as_struct().expect("didn't construct a struct?");
        assert_eq!(
            TyStruct::accessible_intersect(a_struc, b_struc),
            vec![AccessPath::default().field("a")]
        );
    }

    #[test]
    pub fn access_intersect_nested() {
        let b_struc_ty = t_struct! {
            "a" => t_int!(0),
            "b" => t_struct! {
                "e" => t_int!(1),
                "f" => t_int!(2),
            },
        };
        let a_struc_ty = t_struct! {
        "c" => t_array! [ t_int!(0); 1 ],
        "b" => t_struct! {
            "d" => t_int!(1),
            "f" => t_int!(2),
        },        };
        let b_struc: std::rc::Rc<TyStruct<&str>> =
            b_struc_ty.as_struct().expect("didn't construct a struct?");
        let a_struc: std::rc::Rc<TyStruct<&str>> =
            a_struc_ty.as_struct().expect("didn't construct a struct?");
        assert_eq!(
            TyStruct::accessible_intersect(a_struc, b_struc),
            vec![AccessPath::default().field("b").field("f")]
        );
    }
}
