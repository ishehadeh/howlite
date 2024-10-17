use std::marker::PhantomData;

use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};

use super::{NodeId, Tree};

#[derive(Debug, Clone)]
pub struct AssociatedTree<T, BaseT, A: Allocator = Global> {
    _t: PhantomData<BaseT>,
    tree: Vec<T, A>,
}

impl<T, BaseT, A: Allocator> AssociatedTree<T, BaseT, A> {
    pub fn get(&self, node: NodeId<BaseT>) -> &T {
        &self.tree[node.index]
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.tree.iter()
    }
}

pub enum PartialBuildResult<
    'a,
    T,
    BaseT,
    E,
    TreeAllocT: Allocator = Global,
    AssocTreeAllocT: Allocator = TreeAllocT,
> {
    Incomplete(PartialAssociatedTree<'a, T, BaseT, TreeAllocT, AssocTreeAllocT>),
    Complete(AssociatedTree<T, BaseT, AssocTreeAllocT>),
    Err(
        PartialAssociatedTree<'a, T, BaseT, TreeAllocT, AssocTreeAllocT>,
        E,
    ),
}

#[derive(Debug, Clone)]
pub struct PartialAssociatedTree<
    'a,
    T,
    BaseT,
    TreeAllocT: Allocator = Global,
    AssocTreeAllocT: Allocator = TreeAllocT,
> {
    base: &'a Tree<BaseT, TreeAllocT>,
    pub(crate) items: Vec<T, AssocTreeAllocT>,
}

pub struct AssociatedTreeBuildContext<
    'a,
    'b,
    T,
    BaseT,
    TreeAllocT: Allocator,
    AssocTreeAllocT: Allocator,
> {
    tree: &'b PartialAssociatedTree<'a, T, BaseT, TreeAllocT, AssocTreeAllocT>,
}

impl<'a, 'b: 'a, T, BaseT, TreeAllocT: Allocator, AssocTreeAllocT: Allocator>
    AssociatedTreeBuildContext<'a, 'b, T, BaseT, TreeAllocT, AssocTreeAllocT>
{
    pub fn get(&self, node: NodeId<BaseT>) -> &'a T {
        self.tree.items.get(node.index)
            .expect("invalid node ID used during associated tree construction, was this an outside reference?")
    }

    pub fn get_base(&self, node: NodeId<BaseT>) -> &'a BaseT {
        self.tree.base.get(node)
    }
}

impl<'a, T, BaseT, TreeAllocT: Allocator, AssocTreeAllocT: Allocator>
    PartialAssociatedTree<'a, T, BaseT, TreeAllocT, AssocTreeAllocT>
{
    pub fn new_in(base: &'a Tree<BaseT, TreeAllocT>, alloc: AssocTreeAllocT) -> Self {
        Self {
            base,
            items: Vec::with_capacity_in(base.tree.len(), alloc),
        }
    }

    pub fn create_next<
        E,
        F: FnOnce(
            &'a BaseT,
            AssociatedTreeBuildContext<'a, '_, T, BaseT, TreeAllocT, AssocTreeAllocT>,
        ) -> Result<T, E>,
    >(
        mut self,
        builder: F,
    ) -> PartialBuildResult<'a, T, BaseT, E, TreeAllocT, AssocTreeAllocT> {
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
