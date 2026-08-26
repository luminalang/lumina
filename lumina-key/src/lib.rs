//! Key types that are used by in multiple lumina crates
//!
//! Some helper methods for cranelift-entity

pub use cranelift_entity::PrimaryMap as Map;
pub use cranelift_entity::{
    entity_impl, packed_option::PackedOption, packed_option::ReservedValue, EntityList, EntityRef,
    EntitySet, ListPool, PrimaryMap, SecondaryMap,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Field(pub u32);
entity_impl!(Field, "field");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Lambda(pub u32);
entity_impl!(Lambda, "λ");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Generic(pub u32);
entity_impl!(Generic, "·");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Variant(pub u32);
entity_impl!(Variant, "variant");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AssociatedType(pub u32);
entity_impl!(AssociatedType, "assoc");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct Method(pub u32);
entity_impl!(Method, "method");

pub fn findv<K: EntityRef, V, F>(map: &PrimaryMap<K, V>, mut f: F) -> Option<(K, &V)>
where
    F: FnMut(&V) -> bool,
{
    map.iter().find_map(|(k, v)| f(v).then_some((k, v)))
}

/// Iterate over an `EntityList` while re-borrowing the `ListPool` on `next`
pub struct EntityIter<T: EntityRef + ReservedValue> {
    i: usize,
    entities: EntityList<T>,
}

impl<T: EntityRef + ReservedValue> EntityIter<T> {
    pub fn from(entities: EntityList<T>) -> Self {
        Self { i: 0, entities }
    }

    pub fn next(&mut self, pool: &ListPool<T>) -> Option<T> {
        let v = self.entities.get(self.i, pool);
        self.i += 1;
        v
    }
}
