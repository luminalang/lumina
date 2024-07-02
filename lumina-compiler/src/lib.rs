#![feature(type_changing_struct_update)]

pub mod ast;
pub mod backend;
pub mod hir;
pub mod lir;
pub mod mir;
mod prelude;
pub mod target;
pub use target::Target;

mod fmt;

use derive_new::new;
use key::M;
use lumina_key as key;

pub const LISTABLE_CONS: key::Method = key::Method(0);
pub const LISTABLE_NEW: key::Method = key::Method(1);
pub const LISTABLE_WITH_CAPACITY: key::Method = key::Method(2);
pub const LISTABLE_SPLIT: key::Method = key::Method(3);

pub const MAYBE_JUST: key::SumVariant = key::SumVariant(0);
pub const MAYBE_NONE: key::SumVariant = key::SumVariant(1);

pub const CLOSURE_CALL: key::Method = key::Method(0);
pub const CLOSURE_CAPTURES: key::Param = key::Param(0);

pub const SIZE_OF: key::Method = key::Method(0);

pub const TRAIT_OBJECT_DATA_FIELD: key::RecordField = key::RecordField(0);
pub const VTABLE_FIELD: key::RecordField = key::RecordField(1);

#[derive(new, Clone, Copy)]
pub struct ProjectInfo {
    main: M<key::Func>,
    closure: M<key::Trait>,
    size: M<key::Trait>,
    allocator: (M<key::Func>, M<key::Func>),
    reflect_type: M<key::Sum>,
    listable: M<key::Trait>,
    string: M<key::TypeKind>,
}
