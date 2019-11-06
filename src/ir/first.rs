#[derive(Debug, Clone)]
pub struct First<T> {
    inner: Vec<T>,
}

impl<T> First<T> {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Vec::with_capacity(cap),
        }
    }

    pub fn to_void(&self) -> std::slice::Iter<T> {
        self.inner[0..self.inner.len() - 1].iter()
    }
    pub fn to_eval(&self) -> &T {
        &self.inner[self.inner.len() - 1]
    }
}

impl<T> From<Vec<T>> for First<T> {
    fn from(v: Vec<T>) -> First<T> {
        First { inner: v }
    }
}
