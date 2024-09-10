//! Polyfill for currently unstable try_collect iterator (as of 2024-09-10)
struct TryIter<'a, T, ErrT, InnerIterT>
where
    InnerIterT: Iterator<Item = Result<T, ErrT>>,
{
    err: &'a mut Option<ErrT>,
    inner: InnerIterT,
}

impl<'a, T, ErrT, InnerIterT> TryIter<'a, T, ErrT, InnerIterT>
where
    InnerIterT: Iterator<Item = Result<T, ErrT>>,
{
    pub fn new(iter: InnerIterT, err: &'a mut Option<ErrT>) -> TryIter<'a, T, ErrT, InnerIterT> {
        TryIter { err, inner: iter }
    }
}

impl<'a, T, ErrT, InnerIterT> Iterator for TryIter<'a, T, ErrT, InnerIterT>
where
    InnerIterT: Iterator<Item = Result<T, ErrT>>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(_) = self.err {
            None
        } else {
            match self.inner.next() {
                Some(Ok(val)) => Some(val),
                Some(Err(e)) => {
                    *self.err = Some(e);
                    None
                }
                None => None,
            }
        }
    }
}

pub trait TryCollect<T, ErrT>: Iterator<Item = Result<T, ErrT>> {
    fn try_collect_poly<C: FromIterator<T>>(&mut self) -> Result<C, ErrT>;
}

impl<T, ErrT, IterT> TryCollect<T, ErrT> for IterT
where
    IterT: Iterator<Item = Result<T, ErrT>>,
{
    fn try_collect_poly<C: FromIterator<T>>(&mut self) -> Result<C, ErrT> {
        let mut err: Option<ErrT> = None;
        let try_iter = TryIter::new(self, &mut err);
        let collection = C::from_iter(try_iter);
        err.map_or(Ok(collection), |err| Err(err))
    }
}


