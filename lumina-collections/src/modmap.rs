use crate::{map_key_impl, Map, MapKey};
use std::fmt;
use std::ops::{Deref, DerefMut, Index, IndexMut};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module(pub u32);

map_key_impl!(Module(u32), "module");

/// A set of modules each containing a set of K -> V pairs
///
/// Access with M<K>
#[derive(Clone, PartialEq, Eq)]
pub struct MMap<K, V> {
    modules: Map<Module, Map<K, V>>,
}

impl<K, V> Default for MMap<K, V> {
    fn default() -> Self {
        MMap::new()
    }
}

impl<K, V> MMap<K, V> {
    pub fn new() -> Self {
        MMap::with_capacity(0)
    }

    pub fn with_capacity(cap: usize) -> Self {
        MMap { modules: Map::with_capacity(cap) }
    }
}

impl<K: MapKey, V> MMap<K, V> {
    pub fn add_module(&mut self, cap: usize) -> Module {
        self.modules.push(Map::with_capacity(cap))
    }

    pub fn push(&mut self, module: Module, value: V) -> M<K> {
        let key = self.modules[module].push(value);
        M(module, key)
    }

    pub fn push_as(&mut self, M(module, key): M<K>, value: V) {
        self.modules[module].push_as(key, value);
    }

    // TODO: If we make a custom struct we can return 'static instead
    pub fn iter(&self) -> impl Iterator<Item = M<K>> + '_ {
        self.modules
            .iter()
            .flat_map(move |(module, map)| map.keys().map(move |k| M(module, k)))
    }

    pub fn map<T>(&self, mut f: impl FnMut(M<K>, &V) -> T) -> MMap<K, T> {
        MMap {
            modules: self
                .modules
                .iter()
                .map(|(module, map)| map.iter().map(|(k, v)| f(M(module, k), v)).collect())
                .collect(),
        }
    }

    pub fn secondary<U, T>(&self) -> MMap<U, T> {
        MMap { modules: self.modules.values().map(Map::secondary).collect() }
    }

    /// Construct a new Map with a default value in each space of an existing Map
    pub fn secondary_with<U: MapKey, T>(&self, mut new: impl FnMut(M<K>, &V) -> T) -> MMap<U, T> {
        MMap {
            modules: self
                .modules
                .iter()
                .map(|(module, map)| map.iter().map(|(k, v)| new(M(module, k), v)).collect())
                .collect(),
        }
    }

    pub fn modules(&self) -> impl Iterator<Item = Module> + 'static {
        self.modules.keys()
    }

    pub fn iter_module(&self, module: Module) -> impl Iterator<Item = M<K>> {
        self.modules[module].keys().map(move |k| M(module, k))
    }

    pub fn get_both(&mut self, keys: [M<K>; 2]) -> [&mut V; 2] {
        if keys[0].0 == keys[1].0 {
            self.modules[keys[0].0].get_many_mut(keys.map(|k| k.1))
        } else {
            let modules = keys.map(|k| k.0);
            let [fst, snd] = self.modules.get_many_mut(modules);

            [&mut fst[keys[0].1], &mut snd[keys[1].1]]
        }
    }
}

impl<K: MapKey, V> FromIterator<Module> for MMap<K, V> {
    fn from_iter<T: IntoIterator<Item = Module>>(iter: T) -> Self {
        MMap { modules: iter.into_iter().map(|_| Map::new()).collect() }
    }
}

impl<K: MapKey, V> Index<Module> for MMap<K, V> {
    type Output = Map<K, V>;

    fn index(&self, module: Module) -> &Map<K, V> {
        &self.modules[module]
    }
}

impl<K: MapKey, V> IndexMut<Module> for MMap<K, V> {
    fn index_mut(&mut self, module: Module) -> &mut Map<K, V> {
        &mut self.modules[module]
    }
}

impl<K: MapKey, V> Index<M<K>> for MMap<K, V> {
    type Output = V;

    fn index(&self, index: M<K>) -> &V {
        &self.modules[index.0][index.1]
    }
}

impl<K: MapKey, V> IndexMut<M<K>> for MMap<K, V> {
    fn index_mut(&mut self, index: M<K>) -> &mut V {
        &mut self.modules[index.0][index.1]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct M<K>(pub Module, pub K);

impl<K> M<K> {
    pub fn parent_of<T>(self, key: T) -> M<T> {
        M(self.0, key)
    }

    pub fn map<T>(self, f: impl FnOnce(K) -> T) -> M<T> {
        M(self.0, f(self.1))
    }
}

impl<T> Deref for M<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<T> DerefMut for M<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl<T: fmt::Display> fmt::Display for M<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl<T: fmt::Debug> fmt::Debug for M<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{:#?}", self.0, self.1)
    }
}

impl<K: MapKey, V: fmt::Display> fmt::Display for MMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.modules.fmt(f)
    }
}

impl<K: MapKey, V: fmt::Debug> fmt::Debug for MMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.modules.fmt(f)
    }
}
