use std::marker::PhantomData;

use super::{NodeId, Tree};

#[derive(Debug, Clone)]
pub struct AssociatedTree<T, BaseT> {
    _t: PhantomData<BaseT>,
    tree: Vec<T>,
}

impl<T, BaseT> AssociatedTree<T, BaseT> {
    pub fn get(&self, node: NodeId<BaseT>) -> &T {
        &self.tree[node.index]
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.tree.iter()
    }
}

pub enum PartialBuildResult<'a, T, BaseT, E> {
    Incomplete(PartialAssociatedTree<'a, T, BaseT>),
    Complete(AssociatedTree<T, BaseT>),
    Err(PartialAssociatedTree<'a, T, BaseT>, E),
}

#[derive(Debug, Clone)]
pub struct PartialAssociatedTree<'a, T, BaseT> {
    base: &'a Tree<BaseT>,
    pub(crate) items: Vec<T>,
}

pub struct AssociatedTreeBuildContext<'a, 'b, T, BaseT> {
    tree: &'b PartialAssociatedTree<'a, T, BaseT>,
}

impl<'a, 'b: 'a, T, BaseT> AssociatedTreeBuildContext<'a, 'b, T, BaseT> {
    pub fn get(&self, node: NodeId<BaseT>) -> &'a T {
        self.tree.items.get(node.index)
            .expect("invalid node ID used during associated tree construction, was this an outside reference?")
    }

    pub fn get_base(&self, node: NodeId<BaseT>) -> &'a BaseT {
        self.tree.base.get(node)
    }
}

impl<'a, T, BaseT> PartialAssociatedTree<'a, T, BaseT> {
    pub fn new(base: &'a Tree<BaseT>) -> Self {
        Self {
            base,
            items: Vec::with_capacity(base.tree.len()),
        }
    }

    pub fn create_next<
        E,
        F: FnOnce(&'a BaseT, AssociatedTreeBuildContext<'a, '_, T, BaseT>) -> Result<T, E>,
    >(
        mut self,
        builder: F,
    ) -> PartialBuildResult<'a, T, BaseT, E> {
        if self.items.len() >= self.base.tree.len() {
            PartialBuildResult::Complete(AssociatedTree {
                _t: PhantomData::<BaseT>,
                tree: self.items,
            })
        } else {
            let node_id = NodeId {
                _t: std::marker::PhantomData,
                index: self.items.len(),
            };
            let new_el = match builder(
                self.base.get(node_id),
                AssociatedTreeBuildContext { tree: &self },
            ) {
                Ok(v) => v,
                Err(e) => return PartialBuildResult::Err(self, e),
            };

            self.items.push(new_el);
            if self.base.tree.len() <= self.items.len() {
                PartialBuildResult::Complete(AssociatedTree {
                    _t: PhantomData::<BaseT>,
                    tree: self.items,
                })
            } else {
                PartialBuildResult::Incomplete(self)
            }
        }
    }
}
