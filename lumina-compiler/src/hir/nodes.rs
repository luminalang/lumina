use super::lit::Literal;
use lumina_util::Tr;
use rvsdg::node_kind_impl;

node_kind_impl!(Literal, "literal");

#[derive(Debug, Clone)]
pub struct Eq;
node_kind_impl!(Eq, "eq");

#[derive(Debug, Clone)]
pub struct Poison {}
node_kind_impl!(Poison, "poison");

#[derive(Debug, Clone)]
pub struct UnresolvedFieldAccessor(pub String);
node_kind_impl!(UnresolvedFieldAccessor, "unres_field_accessor");

#[derive(Debug, Clone)]
pub struct FieldAccessor(pub usize);
node_kind_impl!(FieldAccessor, "field");

/// The identity node takes a single input and has a single output.
///
/// It directly gives its input as its output.
///
/// The main purpose of this is if you need to associate additional information with a node that
/// does not have a known node origin.
#[derive(Debug, Clone)]
pub struct Identity;
node_kind_impl!(Identity, "identity");

#[derive(Debug, Clone)]
pub struct UnresolvedRecord {
    field_names: Vec<Tr<String>>,
}
node_kind_impl!(UnresolvedRecord, "unres_record");

#[derive(Debug, Clone)]
pub struct Match();
node_kind_impl!(Match, "match");
