use std::fmt;
use std::marker::PhantomData;
use std::mem;
use std::ops::{Index, IndexMut};

pub trait MapKey:
    From<usize> + Into<usize> + Sized + fmt::Display + fmt::Debug + Copy + PartialEq + Eq
{
    /// Cast this key to another key with the same underlying index
    fn cast<K: MapKey>(self) -> K {
        K::from(self.into())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Map<K, V> {
    inner: Vec<V>,
    _key: PhantomData<K>,
}

impl<K: MapKey, V> Default for Map<K, V> {
    fn default() -> Self {
        Map::new()
    }
}

impl<K: MapKey, V> Map<K, V> {
    pub fn new() -> Self {
        Self { inner: Vec::new(), _key: PhantomData }
    }

    pub fn as_mut_vec(&mut self) -> &mut Vec<V> {
        &mut self.inner
    }
    pub fn as_slice(&self) -> &[V] {
        self.inner.as_slice()
    }
    pub fn as_mut_slice(&mut self) -> &mut [V] {
        self.inner.as_mut_slice()
    }

    /// Truncate by key, invalidating any keys greater than `at`
    pub fn truncate(&mut self, at: K) {
        self.inner.truncate(at.into())
    }

    /// Remove all elements while keeping the capacity the same
    pub fn clear(&mut self) {
        self.inner.clear()
    }

    pub fn with_capacity(cap: usize) -> Self {
        Map { inner: Vec::with_capacity(cap), _key: PhantomData }
    }

    pub fn push(&mut self, value: V) -> K {
        let key = self.next_key();
        self.inner.push(value);
        key
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn next_key(&self) -> K {
        K::from(self.inner.len())
    }

    pub fn has(&self, key: K) -> bool {
        key.into() < self.next_key().into()
    }

    /// Construct a new Map with the same capacity of an existing Map
    pub fn secondary<U, T>(&self) -> Map<U, T> {
        Map { inner: Vec::with_capacity(self.len()), _key: PhantomData }
    }

    /// Construct a new Map with a default value in each space of an existing Map
    pub fn secondary_with<U: MapKey, T>(&self, mut new: impl FnMut(K, &V) -> T) -> Map<U, T> {
        self.iter().map(|(k, v)| new(k, v)).collect()
    }

    pub fn map<T, F>(&self, mut f: F) -> Map<K, T>
    where
        F: FnMut(&V) -> T,
    {
        self.inner.iter().map(|v| f(v)).collect()
    }

    pub fn find(&self, mut f: impl FnMut(&V) -> bool) -> Option<K> {
        self.iter().find_map(|(k, v)| f(v).then_some(k))
    }
    pub fn findk(&self, mut f: impl FnMut(K, &V) -> bool) -> Option<K> {
        self.iter().find_map(|(k, v)| f(k, v).then_some(k))
    }

    pub fn keys(&self) -> KeysIter<K> {
        KeysIter { range: 0..self.next_key().into(), _key: PhantomData }
    }
    pub fn values(&self) -> std::slice::Iter<'_, V> {
        self.inner.iter()
    }
    pub fn values_mut(&mut self) -> std::slice::IterMut<'_, V> {
        self.inner.iter_mut()
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        let keys = self.keys();
        keys.zip(self.inner.iter())
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)> {
        let keys = self.keys();
        keys.zip(self.inner.iter_mut())
    }
}

impl<K: MapKey + PartialEq + Eq, V> Map<K, V> {
    #[track_caller]
    pub fn push_as(&mut self, key: K, value: V) {
        assert_eq!(key, self.push(value), "key ordering corruption");
    }

    pub fn get_many_mut<const N: usize>(&mut self, indices: [K; N]) -> [&mut V; N] {
        for (i, &idx) in indices.iter().enumerate() {
            if idx.into() >= self.len() {
                panic!("{idx} does not exist in map");
            }
            for &idx2 in &indices[..i] {
                if idx == idx2 {
                    panic!("key references twice in get_many_mut");
                }
            }
        }

        let slice: *mut V = self.inner.as_mut_ptr();
        let mut arr: mem::MaybeUninit<[&mut V; N]> = mem::MaybeUninit::uninit();
        let arr_ptr = arr.as_mut_ptr();

        unsafe {
            for i in 0..N {
                let idx = *indices.get_unchecked(i);
                *(*arr_ptr).get_unchecked_mut(i) = &mut *slice.add(idx.into());
            }
            arr.assume_init()
        }
    }
}

impl<K: Into<usize>, V> Index<K> for Map<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.inner[index.into()]
    }
}

impl<K: Into<usize>, V> IndexMut<K> for Map<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.inner[index.into()]
    }
}

#[derive(Clone)]
pub struct KeysIter<K> {
    range: std::ops::Range<usize>,
    _key: PhantomData<K>,
}

impl<K: MapKey> KeysIter<K> {
    pub fn range(key: K, len: usize) -> Self {
        let start = key.into();
        let end = start.checked_add(len).unwrap_or(usize::MAX);
        KeysIter { range: start..end, _key: PhantomData }
    }

    pub fn up_to(key: K) -> Self {
        KeysIter { range: 0..key.into(), _key: PhantomData }
    }
}

impl<K: From<usize>> Iterator for KeysIter<K> {
    type Item = K;

    fn next(&mut self) -> Option<K> {
        self.range.next().map(K::from)
    }
}

impl<K: MapKey, V> FromIterator<V> for Map<K, V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Map { inner: iter.into_iter().collect(), _key: PhantomData }
    }
}

impl<'a, K: MapKey, V> IntoIterator for &'a Map<K, V> {
    type Item = (K, &'a V);

    type IntoIter = std::iter::Zip<KeysIter<K>, std::slice::Iter<'a, V>>;

    fn into_iter(self) -> Self::IntoIter {
        let keys = self.keys();
        keys.zip(self.inner.iter())
    }
}

impl<K: From<usize>, V> IntoIterator for Map<K, V> {
    type Item = (K, V);

    type IntoIter = MapIntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        MapIntoIter { map: self.inner.into_iter(), _key: PhantomData, i: 0 }
    }
}

pub struct MapIntoIter<K, V> {
    i: usize,
    map: std::vec::IntoIter<V>,
    _key: PhantomData<K>,
}

impl<K: From<usize>, V> Iterator for MapIntoIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.map.next().map(|elem| {
            let key = K::from(self.i);
            self.i += 1;
            (key, elem)
        })
    }
}

impl<K, V, const N: usize> From<[V; N]> for Map<K, V> {
    fn from(values: [V; N]) -> Self {
        Map { inner: Vec::from(values), _key: PhantomData }
    }
}

impl<K, V> From<Vec<V>> for Map<K, V> {
    fn from(inner: Vec<V>) -> Self {
        Map { inner, _key: PhantomData }
    }
}

impl<K: MapKey, V> Map<K, V> {
    fn format_with<F>(&self, f: &mut fmt::Formatter, mut fmt: F) -> fmt::Result
    where
        F: FnMut(&V) -> String,
    {
        write!(f, "Map ")?;

        if self.inner.is_empty() {
            write!(f, "{{}}")
        } else {
            write!(f, "{{")?;
            for (key, elem) in self.iter() {
                let v = fmt(elem);
                let multiline = v.contains('\n') || v.len() > 40;

                if multiline {
                    writeln!(f, "\n  {key} ->\n    {}", v.replace('\n', "\n    "))?;
                } else {
                    writeln!(f, "\n  {key} -> {v}")?;
                }
            }
            write!(f, "}}")
        }
    }
}

impl<K: MapKey, V: fmt::Display> fmt::Display for Map<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_with(f, V::to_string)
    }
}

impl<K: MapKey, V: fmt::Debug> fmt::Debug for Map<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_with(f, |elem| format!("{:#?}", elem))
    }
}
