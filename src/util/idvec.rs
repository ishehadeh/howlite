use std::{hash::Hash, marker::PhantomData, sync::atomic::AtomicUsize};

#[derive(Debug, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Id<MarkerT> {
    element_id: usize,
    token: IdVecToken<MarkerT>,
}

impl<Marker> Clone for Id<Marker> {
    fn clone(&self) -> Self {
        Id {
            element_id: self.inner(),
            token: self.token,
        }
    }
}

impl<MarkerT> Id<MarkerT> {
    pub fn inner(&self) -> usize {
        self.element_id
    }
}

#[derive(Debug)]
pub struct IdVecToken<MarkerT> {
    container_id: usize,
    marker: PhantomData<MarkerT>,
}

impl<MarkerT> IdVecToken<MarkerT> {
    pub fn inner(&self) -> usize {
        self.container_id
    }

    pub fn owns(&self, id: Id<MarkerT>) -> bool {
        *self == id.token
    }

    pub fn new() -> IdVecToken<MarkerT> {
        static VEC_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
        let container_id = VEC_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        IdVecToken {
            container_id,
            marker: PhantomData::default(),
        }
    }
}

impl<Marker> Clone for IdVecToken<Marker> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Marker> Hash for IdVecToken<Marker> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.container_id.hash(state);
    }
}

impl<Marker> Copy for IdVecToken<Marker> {}

impl<Marker> PartialEq for IdVecToken<Marker> {
    fn eq(&self, other: &Self) -> bool {
        self.container_id == other.container_id
    }
}

impl<Marker> Eq for IdVecToken<Marker> {}

impl<Marker> PartialOrd for IdVecToken<Marker> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<Marker> Ord for IdVecToken<Marker> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.container_id.cmp(&other.container_id)
    }
}

#[derive(Clone, Debug)]
pub struct IdVec<T, Marker = T> {
    elements: Vec<T>,
    token: IdVecToken<Marker>,
}

impl<T, Marker> IdVec<T, Marker> {
    pub fn new() -> IdVec<T, Marker> {
        IdVec {
            elements: Vec::new(),
            token: IdVecToken::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn token(&self) -> IdVecToken<Marker> {
        self.token
    }

    fn next_id(&mut self) -> Id<Marker> {
        let id = Id {
            element_id: self.elements.len(),
            token: self.token(),
        };
        id
    }

    pub fn push(&mut self, val: T) -> Id<Marker> {
        let id = self.next_id();
        self.elements.push(val);
        id
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<Marker>, &T)> {
        self.elements.iter().map(|x| {
            (
                Id {
                    element_id: self.elements.len(),
                    token: self.token(),
                },
                x,
            )
        })
    }

    pub fn get(&self, id: Id<Marker>) -> &T {
        assert!(self.token().owns(id.clone()));
        self.elements.get(id.inner()).unwrap()
    }

    pub fn get_mut(&mut self, id: Id<Marker>) -> &mut T {
        assert!(self.token().owns(id.clone()));
        self.elements.get_mut(id.inner()).unwrap()
    }
}

impl<T, Marker> Default for IdVec<T, Marker> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::IdVec;

    #[test]
    fn idvec_push_get() {
        let mut vec: IdVec<usize> = IdVec::new();
        let a = vec.push(1);
        let b = vec.push(2);
        let c = vec.push(3);
        let d = vec.push(4);

        assert_eq!(*vec.get(a), 1);
        assert_eq!(*vec.get(b), 2);
        assert_eq!(*vec.get(c), 3);
        assert_eq!(*vec.get(d), 4);
    }
}
