use super::NodeId;

#[derive(Debug, Clone)]
pub struct Tree<T> {
    pub(crate) tree: Vec<T>,
}

impl<T> Tree<T> {
    pub fn get(&self, node: NodeId<T>) -> &T {
        &self.tree[node.index]
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.tree.iter()
    }
}
