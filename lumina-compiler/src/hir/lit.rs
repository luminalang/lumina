use lumina_parser as parser;

#[derive(Debug, Clone)]
pub enum Literal {
    Int(bool, u128),
    Float(f64),
    String(Vec<u8>),
    Char(char),
    Bool(bool),
}

impl<'s> From<&'s str> for Literal {
    fn from(raw: &'s str) -> Self {
        Literal::String(escape(raw))
    }
}

impl<'s> From<&parser::Literal<'s>> for Literal {
    fn from(lit: &parser::Literal<'s>) -> Self {
        match lit {
            parser::Literal::Int(b, n) => Literal::Int(*b, *n),
            parser::Literal::Float(f) => Literal::Float(*f),
            parser::Literal::String(raw) => Literal::String(escape(raw)),
            parser::Literal::Char(raw) => {
                let bytes = escape(raw);
                let mut chars = std::str::from_utf8(&bytes).unwrap().chars();
                match (chars.next(), chars.next()) {
                    (Some(char), None) => Literal::Char(char),
                    _ => panic!("ET: invalid char literal size"),
                }
            }
        }
    }
}

fn escape(str: &str) -> Vec<u8> {
    let mut buffer = Vec::with_capacity(str.len());
    let mut bytes = str.bytes();

    loop {
        let Some(mut b) = bytes.next() else {
            break buffer;
        };

        match b {
            b'\\' => match bytes.next() {
                Some(b'n') => b = b'\n',
                Some(b'r') => b = b'\r',
                Some(b't') => b = b'\t',
                Some(b'\\') => b = b'\\',
                Some(b'"') => b = b'"',
                Some(b'\'') => b = b'\'',
                Some(b'0') => b = b'\0',
                b => {
                    panic!("ET: invalid escape sequence: {b:?}");
                }
            },
            _ => {}
        }

        buffer.push(b);
    }
}
