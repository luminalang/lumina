use std::convert::TryFrom;
use std::fmt;

#[derive(PartialEq, Clone, Debug, Default, Hash, Eq)]
pub struct Operator {
    pub identifier: String,
}

impl From<String> for Operator {
    fn from(s: String) -> Self {
        Self { identifier: s }
    }
}
impl TryFrom<&[u8]> for Operator {
    type Error = ();

    fn try_from(s: &[u8]) -> Result<Self, Self::Error> {
        const ALLOWED_CHARACTERS: &[u8] = b"!$%&/=?^~@+-*/;<>";

        for c in s {
            if !ALLOWED_CHARACTERS.contains(&c) {
                return Err(());
            }
        }

        let valid = Self {
            identifier: String::from_utf8(s.to_vec()).unwrap(),
        };
        Ok(valid)
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.identifier)
    }
}
