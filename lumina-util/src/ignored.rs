// A simple wrapper type that causes the interior value to be ignored by `PartialEq` and `Hash`
// derives. Useful for keeping metadata in hashmap keys.
#[derive(Debug, Clone, Default, Copy)]
pub struct Ignored<T> {
    pub inner: T,
}

impl<T> Ignored<T> {
    pub const fn new(inner: T) -> Self {
        Self { inner }
    }
}

impl<T> PartialEq for Ignored<T> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
impl<T> Eq for Ignored<T> {}
impl<T> std::hash::Hash for Ignored<T> {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
}

impl<T> std::ops::Deref for Ignored<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner
    }
}
