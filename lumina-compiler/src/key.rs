use derive_new::new;
pub use lumina_key::*;
use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Project(pub u32);
entity_impl!(Project, "project");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct File(pub u32);
entity_impl!(File, "file");

impl File {
    pub const ROOT: Self = File(0);
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Item(pub u32);
entity_impl!(Item, "item");

#[derive(Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Func(pub u32);
entity_impl!(Func, "func");

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Type(pub u32);
entity_impl!(Type, "type");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Impl(u32);
entity_impl!(Impl, "impl");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Val(u32);
entity_impl!(Val, "val");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Use(u32);
entity_impl!(Use, "use");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Alias(u32);
entity_impl!(Alias, "alias");

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct External(pub u32);
entity_impl!(External, "ext");

#[derive(Clone, Copy, Debug, PartialEq, Eq, new)]
pub struct InProject<T> {
    pub project: Project,
    pub item: T,
}

impl<T> InProject<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> InProject<U> {
        InProject { project: self.project, item: f(self.item) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, new, Debug)]
pub struct Ext<T> {
    pub ext: External,
    pub item: T,
}
