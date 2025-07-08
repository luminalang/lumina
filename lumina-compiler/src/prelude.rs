#![allow(unused)]

pub use cranelift::codegen::entity::{PrimaryMap, SecondaryMap};
pub use derive_new::new;
pub use itertools::Itertools;
// pub use lumina_key as key;
pub use super::key;
pub use lumina_key::{MMap, Map, M};
pub use lumina_util::{Span, Spanned, Tr};
pub use std::collections::HashMap;
pub use tracing::{error, info, trace, warn};

pub type Poison<T> = Option<T>;
