use super::{Map, MapKey};
use std::collections::HashMap;
use std::rc::Rc;

/// An de-duplicating read-only value table
pub struct ReadOnlyTable<K> {
    bytes: Map<K, Rc<Vec<u8>>>,
    query: HashMap<Rc<Vec<u8>>, K>,
}

impl<K: MapKey> ReadOnlyTable<K> {
    pub fn new() -> Self {
        Self { bytes: Map::new(), query: HashMap::new() }
    }

    pub fn push_bytes(&mut self, bytes: Vec<u8>) -> K {
        if let Some(key) = self.query.get(&bytes) {
            return *key;
        }

        let str = Rc::new(bytes);

        let key = self.bytes.push(str.clone());
        self.query.insert(str, key);

        key
    }

    pub fn get(&self, key: K) -> &[u8] {
        self.bytes[key].as_slice()
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &[u8])> {
        self.bytes.iter().map(|(k, v)| (k, v.as_slice()))
    }
}
