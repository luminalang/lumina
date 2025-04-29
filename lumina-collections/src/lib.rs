#[cfg(test)]
mod tests;

mod map;
pub use map::{KeysIter, Map, MapKey};

mod ro;
pub use ro::ReadOnlyTable;

mod modmap;
pub use modmap::{MMap, Module, M};

mod kindmap;

#[macro_export]
macro_rules! map_key_impl {
    ($ty:ident($int:ty), $name:literal) => {
        map_key_impl!($ty($int), |this, f| write!(f, "{}{}", $name, this.0));
    };
    ($ty:ident($int:ty), |$this:ident, $f:ident| $do:expr) => {
        impl From<usize> for $ty {
            fn from(n: usize) -> $ty {
                $ty(n as $int)
            }
        }

        impl Into<usize> for $ty {
            fn into(self) -> usize {
                self.0 as usize
            }
        }

        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                let $this = self;
                let $f = f;
                $do
            }
        }

        impl std::fmt::Debug for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                let $this = self;
                let $f = f;
                $do
            }
        }

        impl $crate::MapKey for $ty {}

        impl $ty {
            #[inline(always)]
            #[allow(dead_code)]
            pub fn inside(self, module: $crate::Module) -> M<$ty> {
                $crate::M(module, self)
            }
        }
    };
}
