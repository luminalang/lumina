use std::convert::TryFrom;

#[derive(PartialEq, Clone, Debug, Default, Hash)]
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
        const ALLOWED_CHARACTERS: &[u8] = b"!$%&/=?^~@+-*/;:";
        // TODO: `:` should be allowed if the character before / after it is also a operator
        // allowed character. I need to implement this into the tokenizer!

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
