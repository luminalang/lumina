use super::*;

/// Helper trait for `O(N)` lookups on small collections
pub trait LinearFind<K, V> {
    fn find(&self, extract: impl FnMut(&V) -> bool) -> Option<K>;
}

impl<K: EntityRef, V> LinearFind<K, V> for Map<K, V> {
    fn find(&self, cond: impl FnMut(&V) -> bool) -> Option<K> {
        self.values().position(cond).map(K::new)
    }
}

impl<K: EntityRef, V> LinearFind<K, V> for &[V] {
    fn find(&self, cond: impl FnMut(&V) -> bool) -> Option<K> {
        self.iter().position(cond).map(K::new)
    }
}

impl<K: EntityRef, V> LinearFind<K, V> for Vec<V> {
    fn find(&self, cond: impl FnMut(&V) -> bool) -> Option<K> {
        self.iter().position(cond).map(K::new)
    }
}

pub trait MapExt<K: EntityRef, V> {
    fn push_as(&mut self, key: K, value: V);
    fn as_capacity<NK: EntityRef, NV>(&self) -> Map<NK, NV>;
}

impl<K: EntityRef + std::fmt::Debug, V> MapExt<K, V> for Map<K, V> {
    #[track_caller]
    fn push_as(&mut self, key: K, value: V) {
        assert_eq!(key, self.push(value), "corrupt key ordering");
    }

    fn as_capacity<NK: EntityRef, NV>(&self) -> Map<NK, NV> {
        Map::with_capacity(self.len())
    }
}

pub trait IterMapCollect<T, O, C: FromIterator<O>>: IntoIterator<Item = T> + Sized {
    fn map_collect<F>(self, f: F) -> C
    where
        F: FnMut(T) -> O,
    {
        self.into_iter().map(f).collect()
    }
}

impl<T, O> IterMapCollect<T, O, Vec<O>> for Vec<T> {}
impl<'a, T, O> IterMapCollect<&'a T, O, Vec<O>> for &'a [T] {}
impl<K: EntityRef, T, O> IterMapCollect<(K, T), O, Map<K, O>> for Map<K, T> {}
