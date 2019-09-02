pub mod function;
pub mod r#type;

use crate::parser::index::Index;
use std::path::Path;

// NOTES
// Since nothing can re-import main I can safely use `0` as the SELF id

pub trait Identifier {
    fn from(w: &str, call_fid: &Path, index: &mut Index) -> Self;
    fn as_str(&self) -> Option<&str>;
    fn get_fid_id(&self) -> (usize, usize);
}
