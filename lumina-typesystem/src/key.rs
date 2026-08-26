pub use lumina_key::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Var(pub(crate) u32);

#[derive(Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Application(u32);

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SameasUnification(u32);

// Instead of type aliasing the indices to various Vec's we use statically typed keys
entity_impl!(Var, "var");
entity_impl!(Application, "application");
entity_impl!(SameasUnification, "same-as");
