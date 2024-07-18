use super::{EntityRef, Map, Module, M};
use itertools::Itertools;
use std::fmt;
use std::ops::{Index, IndexMut};

pub struct ModMap<K: EntityRef, V>(Map<Module, Map<K, V>>);

impl<K: EntityRef, V> Default for ModMap<K, V> {
    fn default() -> Self {
        Self(Map::new())
    }
}

impl<K: EntityRef, V> FromIterator<Module> for ModMap<K, V> {
    fn from_iter<T: IntoIterator<Item = Module>>(iter: T) -> Self {
        ModMap(iter.into_iter().map(|_| Map::new()).collect())
    }
}

impl<K: EntityRef, V> ModMap<K, V> {
    pub fn with_capacity(iter: impl IntoIterator<Item = usize>) -> Self {
        Self(iter.into_iter().map(Map::with_capacity).collect())
    }

    pub fn new() -> Self {
        Self::with_capacity([])
    }

    pub fn len(&self) -> usize {
        self.0.values().map(|v| v.len()).sum()
    }

    pub fn add_module(&mut self, cap: usize) -> Module {
        self.0.push(Map::with_capacity(cap))
    }

    pub fn push(&mut self, module: Module, value: V) -> M<K> {
        module.m(self.0[module].push(value))
    }

    pub fn iter(&self) -> impl Iterator<Item = M<K>> + '_ {
        self.0
            .iter()
            .map(move |(m, map)| map.keys().map(move |k| m.m(k)))
            .flatten()
    }

    pub fn map<T>(&self, mut f: impl FnMut((M<K>, &V)) -> T) -> ModMap<K, T> {
        ModMap(
            self.0
                .iter()
                .map(move |(m, v)| v.iter().map(|(k, v)| f((m.m(k), v))).collect())
                .collect(),
        )
    }

    pub fn iterv(&self) -> impl Iterator<Item = (M<K>, &'_ V)> {
        self.iter().map(|k| (k, &self[k]))
    }

    pub fn modules(&self) -> impl Iterator<Item = Module> + 'static {
        self.0.keys()
    }

    pub fn secondary<A: EntityRef, B>(&self) -> ModMap<A, B> {
        self.0.keys().collect()
    }

    pub fn secondary_inner_capacity<A: EntityRef, B>(&self) -> ModMap<A, B> {
        self.0
            .values()
            .map(|entries| Map::with_capacity(entries.len()))
            .collect()
    }

    pub fn secondary_with<A: EntityRef, B>(&self, mut f: impl FnMut((K, &V)) -> B) -> ModMap<A, B> {
        ModMap(
            self.0
                .values()
                .map(|entries| entries.iter().map(&mut f).collect())
                .collect(),
        )
    }
}

impl<K: EntityRef + 'static, V> ModMap<K, V> {
    pub fn iter_module(&self, module: Module) -> impl Iterator<Item = M<K>> + 'static {
        self.0[module].keys().map(move |k| module.m(k))
    }
}

impl<K: EntityRef + std::fmt::Debug, V> ModMap<K, V> {
    pub fn push_as(&mut self, key: M<K>, value: V) {
        assert_eq!(self.0[key.module].push(value), key.value);
    }
}

impl<K: EntityRef, V> Index<M<K>> for ModMap<K, V> {
    type Output = V;

    fn index(&self, M { module, value }: M<K>) -> &V {
        &self.0[module][value]
    }
}

impl<K: EntityRef, V> IndexMut<M<K>> for ModMap<K, V> {
    fn index_mut(&mut self, M { module, value }: M<K>) -> &mut V {
        &mut self.0[module][value]
    }
}

impl<K: EntityRef, V> Index<Module> for ModMap<K, V> {
    type Output = Map<K, V>;

    fn index(&self, module: Module) -> &Map<K, V> {
        &self.0[module]
    }
}

impl<K: EntityRef, V> IndexMut<Module> for ModMap<K, V> {
    fn index_mut(&mut self, module: Module) -> &mut Map<K, V> {
        &mut self.0[module]
    }
}

impl<K: EntityRef, V> FromIterator<Map<K, V>> for ModMap<K, V> {
    fn from_iter<T: IntoIterator<Item = Map<K, V>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<K: EntityRef + fmt::Display, V: fmt::Display> fmt::Display for ModMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        modmap_fmt(self, f, |v| format!("{}", v))
    }
}

impl<K: EntityRef + fmt::Display, V: fmt::Debug> fmt::Debug for ModMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        modmap_fmt(self, f, |v| format!("{:#?}", v))
    }
}

fn modmap_fmt<K: EntityRef + fmt::Display, V>(
    map: &ModMap<K, V>,
    f: &mut fmt::Formatter<'_>,
    fmt: impl Fn(&V) -> String,
) -> fmt::Result {
    if map.0.is_empty() {
        write!(f, "{{}}")
    } else {
        write!(f, "{{\n")?;

        for (module, entities) in map.0.iter() {
            write!(f, "  {module} ")?;
            if entities.is_empty() {
                write!(f, "{{}}")
            } else {
                write!(f, "{{\n")?;
                for (key, entity) in entities.iter() {
                    write!(f, "    {key} ")?;
                    let entity = fmt(entity);
                    if entity.contains("\n") {
                        write!(f, "{{\n")?;
                        write!(f, "      {}", entity.lines().format("\n      "))?;
                        write!(f, "}}\n")
                    } else {
                        write!(f, ": {entity}")
                    }?;
                }
                write!(f, "}}\n")
            }?;
        }

        write!(f, "}}")
    }
}
