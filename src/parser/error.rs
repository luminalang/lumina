use super::tokenizer::linecount;
use crate::error::{Error, Leaf};
use std::path::PathBuf;

pub struct ParseError {
    kind: Leaf,
    pub file: PathBuf,
    line_n: linecount::Point,
    line: Option<Vec<u8>>,
}

impl Error for ParseError {
    fn leaf(&self) -> &Leaf {
        &self.kind
    }
    fn source(&self) -> Option<String> {
        let line = self
            .line
            .as_ref()
            .map(|a| String::from_utf8(a.to_vec()).unwrap())?;
        let spacing = std::iter::repeat(b' ')
            .take(
                self.file.file_name().unwrap().len()
                    - std::cmp::max(0, self.line_n.line.to_string().len()),
            )
            .collect::<Vec<u8>>();
        Some(format!(
            "{}{}:   {}",
            String::from_utf8(spacing).unwrap(),
            self.line_n.line + 1, // +1 because indexes start at 0 but humans prefer to start at 1
            line.trim(),
        ))
    }

    fn header(&self) -> String {
        let mut s = String::with_capacity(16);
        s.push_str(self.file.file_name().map(|a| a.to_str().unwrap()).unwrap());
        s.push_str(":");
        s
    }
}

impl ParseError {
    pub fn new(file: PathBuf, kind: Leaf) -> Self {
        Self {
            kind,
            file,
            line_n: linecount::Point::new(),
            line: None,
        }
    }

    pub fn with_linen(mut self, n: linecount::Point) -> Self {
        self.line_n = n;
        self
    }
    pub fn with_line(mut self, line: &[u8]) -> Self {
        if self.line.is_some() {
            self
        } else {
            self.line = Some(line.to_vec());
            self
        }
    }
    pub fn find_line(self, source: &[u8]) -> Self {
        let line = source
            .split(|c| *c == b'\n')
            .enumerate()
            .find_map(|(i, line)| {
                if i == self.line_n.line as usize {
                    Some(line)
                } else {
                    None
                }
            })
            .unwrap();
        self.with_line(line)
    }
}
