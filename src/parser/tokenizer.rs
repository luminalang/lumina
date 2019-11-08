mod token;
use std::convert::TryFrom;
pub use token::{
    is_valid_identifier, Header, Inlined, Key, Operator, RawToken, Token,
    ALLOWED_IDENTIFIER_CHARACTERS,
};

pub struct Tokenizer<'s> {
    source_code: &'s [u8],
    history: Vec<usize>,
    index: usize,
}

const BREAK_AT: &[u8] = b" ,()[]+*/\n#{}#";
const IGNORES_SPACE: &[u8] = b",([+*/\n#{}#";
// &[(x, [y])] -> Only early-breaks on x if the next byte isn't any of y
const MAYBE_IGNORES_SPACE: &[(u8, &[u8])] = &[
    (b'-', &[b'>']),
    (b'!', &[b'=']),
    (b':', ALLOWED_IDENTIFIER_CHARACTERS),
];

impl<'s> From<&'s [u8]> for Tokenizer<'s> {
    fn from(source_code: &'s [u8]) -> Self {
        Self {
            source_code,
            history: vec![0],
            index: 0,
        }
    }
}

impl<'s> Tokenizer<'s> {
    pub fn push_history(&mut self, n: usize) {
        // We don't need more than 3 in history, so lets reuse some allocations
        if self.history.len() == 3 {
            self.history[0] = self.history[1];
            self.history[1] = self.history[2];
            self.history[2] = n;
        } else {
            self.history.push(n);
        }
    }
    pub fn pop_history(&mut self) -> usize {
        self.history.pop().unwrap_or(0)
    }
    pub fn index(&self) -> usize {
        self.index
    }
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
    pub fn next_char(&mut self) -> u8 {
        let mut i = 1;
        loop {
            let c = self.source_code[self.index + i];
            if c != b' ' && c != b'\n' {
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
            Some(b'/') => {
                let next = self.source_code.get(self.index + 1).copied();
                if next == Some(b'/') {
                    self.progress_until(|s| s.source_code[s.index] == b'\n');
                    self.skip_spaces_and_newlines();
                } else if next == Some(b'*') {
                    self.progress_until(|s| {
                        (s.source_code[s.index] == b'*' && s.next_char() == b'/')
                    });
                }
            }
            _ => {}
        }
    }

    fn prettified_index(&self) -> usize {
        let mut offset = 0;
        loop {
            let c = match self.source_code.get(self.index + offset) {
                None => return self.index + offset - 1,
                Some(c) => *c,
            };
            if c == b' ' || c == b'\n' {
                offset += 1;
            } else {
                return self.index + offset;
            }
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
        if c == b'/' {
            let next = self.source_code.get(self.index + 1).copied();
            // Single line comment
            if next == Some(b'/') {
                self.progress_until(|s| s.source_code[s.index] == b'\n');
                self.progress(1);
                return self.gather_to(stoppers);
            }
            if next == Some(b'*') {
                let found_comment_exit = self
                    .progress_until(|s| (s.source_code[s.index] == b'*' && s.next_char() == b'/'));
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

impl<'s> super::body::BodySource for Tokenizer<'s> {
    fn next(&mut self) -> Option<Token> {
        // self.last_index = self.index;
        self.push_history(self.index);
        let source_index = self.prettified_index();

        let raw = self.gather_to(BREAK_AT);
        let mut t = Token::try_from(raw)
            .ok()
            .map(|t| t.with_source_index(source_index))?;

        // This is a little hacked together but the edge case is that `a:b` is an external call
        // while `a: b` is a lambda/matchbranch/whatever.
        if let RawToken::Identifier(ident) = &t.inner {
            let mut spl = ident.split(':');
            let first = spl.next().unwrap();
            if let Some(second) = spl.next() {
                if second.is_empty() {
                    // edge-case for `valid_ident: unrelated_code`
                    self.regress(1);
                    return Some(Token::new(
                        RawToken::Identifier(ident[0..ident.len() - 1].to_owned()),
                        source_index,
                    ));
                }
                let mut identbuf = vec![first.to_owned(), second.to_owned()];
                for additional in spl {
                    identbuf.push(additional.to_owned());
                }

                t.inner = RawToken::ExternalIdentifier(identbuf);
                return Some(t);
            } else {
                return Some(t);
            }
        }
        Some(t)
    }
    fn undo(&mut self) {
        self.index = self.pop_history();
    }
}
