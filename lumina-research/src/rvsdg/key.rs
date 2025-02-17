use lumina_collections::{map_key_impl, M};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Argument(pub u32);
map_key_impl!(Argument(u32), "a");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Node(pub u32);
map_key_impl!(Node(u32), "node");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Input(pub u32);
map_key_impl!(Input(u32), "node");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Output(pub u32);
map_key_impl!(Output(u32), "node");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Result(pub u32);
map_key_impl!(Result(u32), "node");
