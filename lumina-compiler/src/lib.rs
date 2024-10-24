#![feature(type_changing_struct_update)]
#![feature(get_many_mut)]

pub mod ast;
pub mod backend;
mod debuginfo;
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

pub const STRINGABLE_FROM_RAW_PARTS: key::Method = key::Method(4);

pub const LIST_CONCAT: key::Variant = key::Variant(1);
pub const LIST_SINGLETON: key::Variant = key::Variant(2);
pub const LIST_NIL: key::Variant = key::Variant(3);

pub const MAYBE_JUST: key::Variant = key::Variant(0);
pub const MAYBE_NONE: key::Variant = key::Variant(1);

pub const CLOSURE_CALL: key::Method = key::Method(0);
pub const CLOSURE_CAPTURES: key::Param = key::Param(0);

pub const SIZE_OF: key::Method = key::Method(0);

pub const TRAIT_OBJECT_DATA_FIELD: key::Field = key::Field(0);
pub const VTABLE_FIELD: key::Field = key::Field(1);

#[derive(new, Clone, Copy)]
pub struct ProjectInfo {
    main: M<key::Func>,
    sys_init: M<key::Func>,
    closure: M<key::Trait>,
    allocator: (M<key::Func>, M<key::Func>),
    reflect_type: M<key::Trait>,
    listable: M<key::Trait>,
    global_list_default: M<key::TypeKind>,
    stringable: M<key::Trait>,
    string: M<key::Record>,
    maybe: M<key::Sum>,
}
