use cranelift::codegen::entity::{entity_impl, PrimaryMap};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Project(pub u32);
entity_impl!(Project, "project");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct File(pub u32);
entity_impl!(File, "file");
