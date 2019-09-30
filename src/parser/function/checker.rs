use super::FunctionBuilder;
use crate::parser::{Key, RawToken, Token, Type};

struct Grouper {
    func: FunctionBuilder,
    index: usize,
    infered_types: Vec<Type>,
    held_buffer: Vec<Token>,
}

impl Grouper {
    /*
    fn walk(&mut self, to: &RawToken) {
        let mut target = match self.func.body.get_mut(self.index) {
            None => return,
            Some(t) => {
                if t.inner == *to {
                    return;
                } else {
                    t
                }
            }
        };
        match &target.inner {
            RawToken::Key(k) => match k {
                Key::Pipe => {
                    self.walk(&RawToken::Key(Key::Pipe));
                    let mut vec = Vec::new();
                    std::mem::swap(&mut vec, &mut self.held_buffer);
                    *target = Token::group(vec, target.source_index)
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    */
}
