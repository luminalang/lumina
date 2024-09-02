#[macro_export]
macro_rules! kind_key {
    ($vs:vis enum $name:ident {
        $($vname:ident($vty:ty)),*,
    }) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        $vs enum $name {
            $($vname($vty)),*,
        }

        $(
            impl From<$vty> for $name {
                fn from(key: $vty) -> Self {
                    Self::$vname(key)
                }
            }
        )*


        $(
            impl std::convert::TryFrom<$name> for $vname {
                type Error = ();

                fn try_from(kinds: $name) -> Result<$vname, Self::Error> {
                    match kinds {
                         $name::$vname(key) => Ok(key),
                         _ => Err(()),
                    }
                }
            }
        )*

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $($name::$vname(key) => key.fmt(f)),*,
                }
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $($name::$vname(key) => key.fmt(f)),*,
                }
            }
        }
    }
}

// TODO: we didn't end up actually needing this
/// Declares numerous maps each associated to a variant of a Kind enum containing a key for each
#[macro_export]
macro_rules! kind_map {
    ($vs:vis struct $name:ident $(<$($generics:tt),*>)? {
        $($field:ident: Map<$varty:ty, $v:ty>),*,
    } enum $tykind:ident {$($variant:ident($vty:ty)),*,}) => {
        $vs struct $name $(<$($generics),*>)? {
            pub $($field: Map<$varty, $v>),*
        }

        #[doc="Enum over the keys "]
        $(#[doc=stringify!($varty)])*
        #[doc=" "]
        $vs enum $tykind {
            $($variant($varty)),*,
        }

        impl $(<$($generics),*>)? $name $(<$($generics),*>)? {
            pub fn new() -> Self {
                Self { $($field: Map::with_capacity(10)),* }
            }

            /// Retrieve data without knowing the exact key by mapping any value into T
            pub fn get_by_fold<T>(&self, key: $tykind, $($field: impl FnOnce(&$v) -> T),*) -> T {
                match key {
                    $($variant(key) => $field(key)),*,
                }
            }
        }

        define_index!($name $(<$($generics),*>)? $($field: $variant($vty) => $v),*);
    };
}

#[macro_export]
macro_rules! define_index {
    ($name:ident $($field:ident: $variant:ident($varty:ty) => $v:ty),*) => {
        $(
        impl std::ops::Index<$varty> for $name {
            type Output = $v;

            fn index(&self, key: $varty) -> &Self::Output {
                &self.$field[key]
            }
        }

        impl std::ops::IndexMut<$varty> for $name {
            fn index_mut(&mut self, key: $varty) -> &mut Self::Output {
                &mut self.$field[key]
            }
        }
        )*
    };
    ($name:ident<$($generics:tt),*> $($field:ident: $variant:ident($varty:ty) => $v:ty),*) => {
        $(
        impl<$($generics),*> std::ops::Index<$varty> for $name<$($generics),*> {
            type Output = $v;

            fn index(&self, key: $varty) -> &Self::Output {
                &self.$field[key]
            }
        }

        impl<$($generics),*> std::ops::IndexMut<$varty> for $name<$($generics),*> {
            fn index_mut(&mut self, key: $varty) -> &mut Self::Output {
                &mut self.$field[key]
            }
        }
        )*
    };
}

// kind_map! {
//     struct Types {
//         records: Map<(), Map<(), ()>>,
//         sums: Map<usize, Map<(), ()>>,
//     }
//
//     enum TypeKind {
//         Record(()),
//         Sum(usize),
//     }
// }
