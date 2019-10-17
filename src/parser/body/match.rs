use super::{BodySource, Token};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct MatchExpr {}

pub fn build(_source: impl BodySource) -> Result<MatchExpr, ()> {
    unimplemented!();
}
