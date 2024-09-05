use lumina_collections::{kind_key, map_key_impl};
pub use lumina_collections::{MMap, Map};
pub use lumina_collections::{Module, M};

kind_key! {
    pub enum TypeKind {
        Record(Record),
        Sum(Sum),
        Trait(Trait),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Record(pub u32);
map_key_impl!(Record(u32), "record");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Sum(pub u32);
map_key_impl!(Sum(u32), "sum");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AssociatedType(pub u32);
map_key_impl!(AssociatedType(u32), "assoc");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Func(u32);
map_key_impl!(Func(u32), "func");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Impl(u32);
map_key_impl!(Impl(u32), "impl");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Param(pub u32);
map_key_impl!(Param(u32), "param");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Val(u32);
map_key_impl!(Val(u32), "val");

/// A Sum variant
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Variant(pub u32);
map_key_impl!(Variant(u32), "variant");

/// A Record field
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Field(pub u32);
map_key_impl!(Field(u32), "field");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ReadOnly(u32);
map_key_impl!(ReadOnly(u32), "ro");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Bind(pub u32);
map_key_impl!(Bind(u32), "b");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Capture(u32);
map_key_impl!(Capture(u32), "c");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DecisionTreeTail(pub u32);
map_key_impl!(DecisionTreeTail(u32), "tail");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Method(pub u32);
map_key_impl!(Method(u32), "method");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Trait(u32);
map_key_impl!(Trait(u32), "trait");

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Generic(pub u32);
map_key_impl!(Generic(u32), |this, f| ((this.0 as u8 + b'a') as char)
    .fmt(f));

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lambda(pub u32);
map_key_impl!(Lambda(u32), |this, f| write!(f, "λ·{}", this.0));

pub const PRELUDE: Module = Module(0);
