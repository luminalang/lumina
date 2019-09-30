mod token;
use std::convert::TryFrom;
pub use token::{is_valid_identifier, Header, Inlined, Key, RawToken, Token};

pub struct Tokenizer<'s> {
    source_code: &'s [u8],
    index: usize,
}

const BREAK_AT: &[u8] = b" ,()[]+*/\n#{}|=:";
const IGNORES_SPACE: &[u8] = b",([+*/\n#{}|:";
// &[(x, [y])] -> Only early-breaks on x if the next byte isn't any of y
const MAYBE_IGNORES_SPACE: &[(u8, &[u8])] = &[
    (b'-', &[b'>']),
    (b'<', &[b'=', b'<']),
    (b'=', &[b'>']),
    (b'!', &[b'=']),
];

impl<'s> From<&'s [u8]> for Tokenizer<'s> {
    fn from(source_code: &'s [u8]) -> Self {
        Self {
            source_code,
            index: 0,
        }
    }
}

impl<'s> Tokenizer<'s> {
    pub fn regress(&mut self, n: usize) {
        self.index -= n;
    }
    fn progress(&mut self, n: usize) {
        self.index += n;
    }
    fn progress_until<F>(&mut self, mut predicate: F) -> bool
    where
        F: FnMut(&mut Self) -> bool,
    {
        loop {
            if self.source_code.len() <= self.index {
                break false;
            }
            if predicate(self) {
                break true;
            }
            self.progress(1);
        }
    }
    fn next_char(&mut self) -> u8 {
        let mut i = 1;
        loop {
            let c = self.source_code[self.index + i];
            if c != b' ' {
                return c;
            }
            i += 1;
        }
    }
    fn _last_char(&mut self) -> u8 {
        let mut i = 1;
        loop {
            let c = self.source_code[self.index - i];
            if c != b' ' {
                return c;
            }
            i += 1;
        }
    }
    pub fn skip_spaces_and_newlines(&mut self) {
        let c = self.source_code.get(self.index);
        match c {
            Some(b' ') | Some(b'\n') => {
                self.progress(1);
                self.skip_spaces_and_newlines();
            }
            _ => {}
        }
    }

    fn gather_to(&mut self, stoppers: &'s [u8]) -> &'s [u8] {
        let origin = self.index;
        let c = match self.source_code.get(self.index) {
            Some(c) => *c,
            None => return &[],
        };
        if c == b' ' {
            self.progress(1);
            return self.gather_to(stoppers);
        }
        if c == b'|' {
            let next = self.source_code.get(self.index + 1).copied();
            // Single line comment
            if next == Some(b'|') {
                self.progress_until(|s| s.source_code[s.index] == b'\n');
                self.progress(1);
                return self.gather_to(stoppers);
            }
            if next == Some(b'>') {
                let found_comment_exit = self.progress_until(|s| {
                    (s.source_code[s.index] == b'|' && s.next_char() == b'<')
                        || (s.source_code[s.index] == b'<' && s.next_char() == b'|')
                });
                self.progress(1);
                if !found_comment_exit {
                    return &[];
                }
                self.progress(1);
                return self.gather_to(stoppers);
            }
        }
        for cmp_against in IGNORES_SPACE.iter().cloned() {
            if c == cmp_against {
                self.progress(1);
                return &self.source_code[origin..self.index];
            }
        }
        for (cmp_against, unless_buf) in MAYBE_IGNORES_SPACE.iter().cloned() {
            if c == cmp_against && !unless_buf.contains(&self.next_char()) {
                self.progress(1);
                return &self.source_code[origin..self.index];
            }
        }
        loop {
            self.progress(1);
            let c = match self.source_code.get(self.index) {
                Some(c) => *c,
                None => return &self.source_code[origin..self.index],
            };
            if stoppers.contains(&c) {
                return &self.source_code[origin..self.index];
            }
        }
    }
}

impl<'s> Iterator for Tokenizer<'s> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let raw = self.gather_to(BREAK_AT);
        Token::try_from(raw)
            .ok()
            .map(|t| t.with_source_index(self.index))
    }
}
