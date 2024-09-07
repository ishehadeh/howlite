use std::rc::Rc;

use smallvec::{smallvec, SmallVec};

use crate::Ty;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField<SymbolT: Eq> {
    pub name: SymbolT,
    pub ty: Ty<SymbolT>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyStruct<SymbolT: Eq> {
    pub fields: SmallVec<[StructField<SymbolT>; 8]>,
}

impl<SymbolT: Eq> TyStruct<SymbolT> {
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &StructField<SymbolT>> {
        self.fields.iter()
    }
}

impl<SymbolT: Eq> TyStruct<SymbolT> {
    pub fn cursor(rc: Rc<Self>) -> StructCursor<SymbolT> {
        StructCursor {
            past: smallvec![(0, rc)],
            next_index: 0,
        }
    }
}

pub struct StructCursor<SymbolT: Eq> {
    past: SmallVec<[(usize, Rc<TyStruct<SymbolT>>); 4]>,
    next_index: usize,
}

impl<SymbolT: Eq> StructCursor<SymbolT> {
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
    use crate::{Ty, TyStruct};
    use preseli::iset;

    macro_rules! t_struct {
        ($($field:expr => $ty:expr),* ,) => {
            t_struct! {
                $($field => $ty),*
            }
        };

        (
            $($field:expr => $ty:expr),*
        ) => {
            Ty::Struct(std::rc::Rc::new($crate::TyStruct {
                fields: smallvec::smallvec![
                    $($crate::ty_struct::StructField {
                        name: $field,
                        ty: $ty
                    }),*
                ]
            }))
        };
    }

    macro_rules! t_int {
        ($($toks:tt)*) => {
            $crate::Ty::Int({
                $crate::TyInt {
                    values: preseli::iset!($($toks)*)
                }
            })
        };
    }

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
}
