pub use cranelift_entity::{entity_impl, EntityRef, PrimaryMap as Map};
use std::fmt;
use std::ops::{Deref, DerefMut};

mod extra;
mod modmap;
pub use extra::{IterMapCollect, LinearFind, MapExt};
pub use modmap::ModMap;

pub const VTABLE_DATA: u32 = 0;
pub const VTABLE_TABLE: u32 = 1;

#[macro_export]
macro_rules! keys {
    ($($name:ident . $fmt:literal),*) => {
        $(#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub u32);
        entity_impl!($name, $fmt);
        )*
    };
    ($($name:ident |$this:ident, $f:ident| $do:expr),*) => {
        $(#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub u32);
        entity_impl!($name);

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                (|$this: &$name, $f: &mut fmt::Formatter| $do)(self, f)
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{self}")
            }
        }
        )*
    };
}

#[macro_export]
macro_rules! key_wrapper {
    ($name:ident, [$($child:ident),*]) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
        pub enum $name {
            $($child(crate::$child)),*
        }

        $(
            impl From<crate::$child> for $name {
                fn from(key: crate::$child) -> $name {
                    Self::$child(key)
                }
            }
        )*

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(
                    Self::$child(a) => a.fmt(f)
                     ),*
                }
            }
        }
    };
}

key_wrapper!(TypeKind, [Record, Sum, Trait]);

keys! {
    Record . "record",
    Sum . "sum",
    AssociatedType . "assoc",
    Func . "func",
    Impl . "impl",
    Param . "param",
    SumVariant . "variant",
    Static . "val",
    RecordField . "field",
    ReadOnly . "ro",
    Val . "val",
    Bind . "v",
    Capture . "c",
    DecisionTreeTail . "tail",
    Method . "method"
}

pub const PRELUDE: Module = Module(0);

pub const CLOSURE: Trait = Trait(0);
// pub const SIZED: Trait = Trait(1);
// pub const ALLOCATOR: Trait = Trait(2);

pub const ALLOC: Method = Method(0);
pub const DEALLOC: Method = Method(1);

pub const LAYOUT: Record = Record(0);
pub const STRING: Record = Record(1);

// const NUM: Trait = Trait(2);

keys! {
    Generic |this, f| ((this.0 as u8 + b'a') as char).fmt(f),
    Lambda |this, f| write!(f, "λ·{}", this.0),
    Module |this, f| write!(f, "▵·{}", this.0),
    Project |this, f| write!(f, "▵·{}", this.0),
    Trait |this, f| match *this {
        _ => write!(f, "trait{}", this.0),
    }
}

/// A module key attached to a value
#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub struct M<T> {
    pub value: T,
    pub module: Module,
}

impl Module {
    pub fn m<T>(self, value: T) -> M<T> {
        M { value, module: self }
    }
}

impl<T> M<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> M<U> {
        M { value: f(self.value), module: self.module }
    }
}

impl<T> Deref for M<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for M<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T: fmt::Display> fmt::Display for M<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.module, self.value)
    }
}
