use lumina_util::{Span, Tr};
use std::fmt;
use std::ops::Deref;

// TODO: use smarter data compression for this kind of thing
#[derive(Clone, Copy)]
pub struct Meta<T> {
    pub kind: T,
    pub span: Span,
    pub comment: Span,
}

impl<T> Meta<T> {
    pub fn new(data: T, span: Span, comment: Span) -> Self {
        Self { kind: data, span, comment }
    }

    pub fn n(data: T, span: Span) -> Self {
        Self { kind: data, span, comment: Span::null() }
    }

    pub fn get_comment<'s>(&self, src: &'s str) -> Option<&'s str> {
        (self.comment != Span::null()).then(|| self.comment.get_str(src))
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Meta<U> {
        Meta { kind: f(self.kind), span: self.span, comment: self.comment }
    }

    pub fn union<U, O>(self, other: Meta<U>, f: impl FnOnce(Meta<T>, Meta<U>) -> O) -> Meta<O> {
        let span = self.span.extend(other.span);
        let comment = if self.comment == Span::null() {
            other.comment
        } else {
            self.comment.extend(other.comment)
        };
        let kind = f(self, other);
        Meta { kind, span, comment }
    }

    pub fn without_comment(&self) -> Meta<&T> {
        let mut v = self.as_ref();
        v.comment = Span::null();
        v
    }

    pub fn as_ref(&self) -> Meta<&T> {
        Meta { kind: &self.kind, span: self.span, comment: self.comment }
    }
}

impl<T> Deref for Meta<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<T: PartialEq> PartialEq for Meta<T> {
    fn eq(&self, other: &Self) -> bool {
        self.kind.eq(&other.kind)
    }
}

impl<T: Eq> Eq for Meta<T> {}

impl<T: fmt::Display> fmt::Display for Meta<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.comment != Span::null() {
            write!(f, "{}({:?})", self.kind, self.comment)
        } else {
            Tr::new(self.span, &self.kind).fmt(f)
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Meta<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.comment != Span::null() {
            write!(f, "{:?}({:?})", self.kind, self.comment)
        } else {
            Tr::new(self.span, &self.kind).fmt(f)
        }
    }
}
