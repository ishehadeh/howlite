use std::{
    cell::UnsafeCell,
    fmt::Debug,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::Deref,
    sync::atomic::{AtomicUsize, Ordering},
};

pub struct TreeSlab<T: Node, const BLOCK_SIZE: usize = 64> {
    block_elem_index: AtomicUsize,
    nodes: UnsafeCell<Vec<Box<[MaybeUninit<T>; BLOCK_SIZE]>>>,
}

impl<T: Node, const BLOCK_SIZE: usize> Default for TreeSlab<T, BLOCK_SIZE> {
    fn default() -> Self {
        Self {
            block_elem_index: AtomicUsize::new(0),
            nodes: Default::default(),
        }
    }
}

impl<T: Node + Debug, const BLOCK_SIZE: usize> std::fmt::Debug for TreeSlab<T, BLOCK_SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();
        for (node_id, node) in self.iter() {
            map.entry(&node_id, node);
        }
        map.finish()
    }
}

#[derive(Clone, Debug)]
pub struct TreeSlabIter<'a, T: Node, const BLOCK_SIZE: usize, const REVERSE: bool> {
    tree: &'a TreeSlab<T, BLOCK_SIZE>,
    block_index: usize,
    block_elem_index: usize,

    end_block_index: usize,
    end_block_elem_index: usize,
}

impl<'a, T: Node + Debug, const BLOCK_SIZE: usize> Iterator
    for TreeSlabIter<'a, T, BLOCK_SIZE, false>
{
    type Item = (NodeId<T>, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        dbg!(
            &self.block_elem_index,
            self.block_index,
            self.end_block_elem_index,
            self.end_block_index
        );
        if self.block_index >= self.end_block_index
            && self.block_elem_index >= self.end_block_elem_index
        {
            None
        } else {
            let id = NodeId {
                block_elem_index: self.block_elem_index,
                block_index: self.block_index,
                _t: Default::default(),
            };
            self.block_elem_index += 1;
            if self.block_elem_index >= BLOCK_SIZE {
                self.block_elem_index = 0;
                self.block_index += 1;
            }
            Some((id.clone(), self.tree.get(id)))
        }
    }
}

pub trait Node {}

#[derive(Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId<T> {
    _t: PhantomData<T>,
    block_index: usize,
    block_elem_index: usize,
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        Self {
            _t: PhantomData,
            block_index: self.block_index,
            block_elem_index: self.block_elem_index,
        }
    }
}

impl<T> std::fmt::Debug for NodeId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[#{}.{}]", self.block_index, self.block_elem_index)
    }
}

impl<T: Node + Debug, const BLOCK_SIZE: usize> TreeSlab<T, BLOCK_SIZE> {
    pub fn iter(&self) -> TreeSlabIter<'_, T, BLOCK_SIZE, false> {
        TreeSlabIter {
            tree: self,
            block_index: 0,
            block_elem_index: 0,
            end_block_index: self.block_count() - 1,
            end_block_elem_index: self.block_elements_used(),
        }
    }

    pub fn get(&self, r: NodeId<T>) -> &T {
        unsafe {
            self.block(r.block_index).expect("Invalid node ID!")[r.block_elem_index]
                .assume_init_ref()
        }
    }

    pub fn push(&self, item: T) -> NodeId<T> {
        dbg!(&item);
        let _ = self.block_elem_index.compare_exchange(
            BLOCK_SIZE,
            0,
            Ordering::SeqCst,
            Ordering::Relaxed,
        );
        let block_elem_index = self.block_elem_index.fetch_add(1, Ordering::SeqCst);
        if block_elem_index == 0 {
            unsafe { self.push_block() }
        }
        let id: NodeId<T> = NodeId {
            _t: PhantomData,
            block_index: self.block_count() - 1,
            block_elem_index,
        };

        unsafe {
            dbg!(&id);
            self.set_cell(id.block_index, id.block_elem_index, item);
        }
        id
    }

    pub fn block_elements_used(&self) -> usize {
        self.block_elem_index.load(Ordering::Acquire)
    }

    // UNSAFE
    fn block(&self, index: usize) -> Option<&[MaybeUninit<T>; BLOCK_SIZE]> {
        unsafe {
            let ptr = self.nodes.get();
            (*ptr).get(index).map(|x| x.deref())
        }
    }

    unsafe fn set_cell(&self, block_index: usize, elem_index: usize, item: T) {
        dbg!(block_index, elem_index);
        unsafe {
            let ptr = self.nodes.get();
            (*ptr).get_mut(block_index).expect("invalid block index")[elem_index] =
                MaybeUninit::new(item);
        };
    }

    pub fn block_count(&self) -> usize {
        unsafe {
            let ptr = self.nodes.get();
            (*ptr).len()
        }
    }

    unsafe fn push_block(&self) {
        unsafe {
            let ptr = self.nodes.get();
            (*ptr).push(Box::new(
                MaybeUninit::<[MaybeUninit<T>; BLOCK_SIZE]>::zeroed().assume_init(),
            ))
        }
    }
}
