use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut, Range};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub indice: u32,
    pub length: u16,
}

#[derive(Clone, Copy)]
pub struct Tr<T> {
    pub span: Span,
    pub value: T,
}

pub trait Spanned<T> {
    fn tr(self, span: Span) -> Tr<T>;
    fn tr_null(self) -> Tr<T>
    where
        Self: Sized,
    {
        self.tr(Span::null())
    }
}

impl<T> Spanned<T> for T {
    fn tr(self, span: Span) -> Tr<T> {
        Tr { span, value: self }
    }
}

impl<T: PartialEq> PartialEq for Tr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq> Eq for Tr<T> {}

impl<T> Tr<Box<T>> {
    pub fn as_deref(&self) -> Tr<&T> {
        (&*self.value).tr(self.span)
    }
}

impl<T: Hash> Hash for Tr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

impl<T> Tr<T> {
    pub const fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }

    pub fn null(value: T) -> Self {
        value.tr(Span::null())
    }

    pub fn indice(&self) -> u32 {
        self.span.indice
    }
    pub fn length(&self) -> u16 {
        self.span.length
    }

    // pub fn some(self) -> Option<Self> {
    //     Some(self)
    // }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Tr<U> {
        f(self.value).tr(self.span)
    }

    pub fn as_ref(&self) -> Tr<&T> {
        (&self.value).tr(self.span)
    }
    pub fn as_mut(&mut self) -> Tr<&mut T> {
        (&mut self.value).tr(self.span)
    }
}

impl<T: Clone> Tr<&T> {
    pub fn cloned(self) -> Tr<T> {
        Tr { span: self.span, value: self.value.clone() }
    }
}

impl<T> Deref for Tr<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Tr<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Span {
        Span {
            indice: range.start as u32,
            length: (range.end - range.start) as u16,
        }
    }
}

// We should just use `Tr` or `Ign` instead
//
// impl std::hash::Hash for Span {
//     fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
// }
//
// impl Eq for Span {}
// impl PartialEq for Span {
//     fn eq(&self, _: &Self) -> bool {
//         true
//     }
// }

impl Span {
    pub fn new(indice: u32, length: u16) -> Self {
        Self { indice, length }
    }

    pub fn extend(self, other: Self) -> Self {
        Self {
            indice: self.indice,
            length: (other.indice - self.indice) as u16 + other.length,
        }
    }

    pub fn from_elems<'a, T>(elems: impl IntoIterator<Item = T>, get: impl Fn(T) -> Span) -> Self {
        let mut params = elems.into_iter();
        let x = params.next().unwrap();
        match params.last() {
            Some(last) => get(x).extend(get(last)),
            None => get(x),
        }
    }

    pub fn extend_by_params<T>(self, params: &[Tr<T>]) -> Self {
        params.last().map(|p| self.extend(p.span)).unwrap_or(self)
    }

    pub fn extend_by_fields<K, V>(self, params: &[(K, Tr<V>)]) -> Self {
        params.last().map(|p| self.extend(p.1.span)).unwrap_or(self)
    }

    pub fn move_indice(mut self, by: i32) -> Self {
        if by.is_positive() {
            self.indice += by as u32;
            self.length -= by as u16;
        } else {
            self.indice -= by.abs() as u32;
            self.length += by.abs() as u16;
        }
        self
    }

    pub fn shift_left(self, by: i16) -> Self {
        self.move_indice(by as i32).extend_length(by)
    }

    pub fn extend_length(mut self, by: i16) -> Self {
        if by.is_positive() {
            self.length += by as u16;
        } else {
            self.length -= by.abs() as u16;
        }
        self
    }

    pub fn shortened(mut self, n: u16) -> Self {
        self.length -= n;
        self
    }

    pub fn end(self) -> Span {
        Span { indice: self.indice + self.length as u32, length: 1 }
    }

    /// Get a span covering the next `n` bytes after this one
    pub fn following(self, n: u16) -> Self {
        Span { indice: self.indice + self.length as u32, length: n }
    }

    pub const fn null() -> Span {
        Span { indice: 0, length: 0 }
    }

    pub fn is_null(self) -> bool {
        self.indice == 0 && self.length == 0
    }

    pub fn get_line(self, source: &str) -> (&str, u32, u32) {
        let start_of_line = seek_newline(source, self.indice as usize, false);
        let end_of_line = seek_newline(source, self.indice as usize, true);

        let offset_from_start = self.indice - start_of_line as u32;
        let offset_from_end = end_of_line as u32 - self.indice;

        (
            &source[start_of_line..end_of_line],
            offset_from_start,
            offset_from_end,
        )
    }

    pub fn get_str(self, source: &str) -> &str {
        &source[self.indice as usize..(self.indice as usize + self.length as usize)]
    }

    pub fn try_get_str(self, source: &str) -> Option<&str> {
        source.get(self.indice as usize..(self.indice as usize + self.length as usize))
    }

    pub fn get_line_number(self, source: &str) -> usize {
        source
            .bytes()
            .enumerate()
            .take_while(|(i, _)| *i != self.indice as usize)
            .fold(1, |n, (_, b)| if b == b'\n' { n + 1 } else { n })
    }
}

// Returns the indice of the closest newline character, left or right depending on `each`
fn seek_newline(source: &str, mut i: usize, forward: bool) -> usize {
    loop {
        match source.as_bytes().get(i) {
            None | Some(b'\n') => break i + if forward { 0 } else { 1 },
            _ if forward => i += 1,
            _ => match i.checked_sub(1) {
                Some(new) => i = new,
                None => return 0,
            },
        }
    }
}

impl<T: fmt::Display> fmt::Display for Tr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

pub const IGNORE_SPAN_IN_DEBUG: bool = true;

impl<T: fmt::Debug> fmt::Debug for Tr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if IGNORE_SPAN_IN_DEBUG {
            self.value.fmt(f)
        } else {
            let v = if f.alternate() {
                format!("{:#?}", self.value)
            } else {
                format!("{:?}", self.value)
            };
            write!(f, "{}.+{}.{v}", self.indice(), self.length())
        }
    }
}
