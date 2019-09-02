use std::fmt;

#[derive(Debug, Clone, Copy, Default)]
pub struct Point {
    pub line: u32,
}

// TODO: At some point I want a character pointer and not only a line pointer
// for improved errors

impl Into<u32> for Point {
    fn into(self) -> u32 {
        self.line
    }
}

impl Point {
    pub fn new() -> Point {
        Point { line: 0 }
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.line)
    }
}
