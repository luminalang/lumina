use std::convert::TryFrom;

#[derive(PartialEq, Clone, Debug)]
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
        const allowed_characters: &[u8] = b"!$%&/=?^~@+-*/;";

        for c in s {
            if !allowed_characters.contains(&c) {
                return Err(());
            }
        }

        let valid = Self {
            identifier: String::from_utf8(s.to_vec()).unwrap(),
        };
        Ok(valid)
    }
}
