use crate::{expr::DiffConflict, expr::IndentConflict, Parser, Token};
use lumina_util::Span;

#[derive(Clone, Debug)]
pub enum Error {
    ExpectedButGot(Span, String, String),
    ExpectedTokenButGot(Span, String, Token),
    // Unexpected(Span, String),
    InvalidAttributes(Span, Span),
    BadIndentation(Span),
    BadDefault(Span, Token),
    BadIndentForMatch(Span, DiffConflict),
    BadHeaderForWhere(Span, Token),
    Unmatched(Span, String),
    InvalidTraitMember(Span),
    InvalidNestedMatch { previous: Span, new: Span },
    ConflictingBars(IndentConflict),
    MissingReturnType(Span),
    NestedWhere { previous: Span, kw: Span },
}

impl<'a> Parser<'a> {
    pub(crate) fn err_expected_but_got(
        &mut self,
        span: Span,
        exp: impl Into<String>,
        got: impl Into<String>,
    ) {
        self.errors
            .push(Error::ExpectedButGot(span, exp.into(), got.into()));
    }

    pub(crate) fn err_unexpected_token(
        &mut self,
        (token, span): (Token, Span),
        exp: impl Into<String>,
    ) {
        self.errors
            .push(Error::ExpectedTokenButGot(span, exp.into(), token));
    }

    pub(crate) fn err_bad_header_for_where(&mut self, span: Span, header: Token) {
        self.errors.push(Error::BadHeaderForWhere(span, header));
    }

    pub(crate) fn err_bad_indentation(&mut self, span: Span) {
        self.errors.push(Error::BadIndentation(span));
    }

    pub(crate) fn err_bad_default(&mut self, span: Span, other: Token) {
        self.errors.push(Error::BadDefault(span, other));
    }

    pub(crate) fn err_bad_bar_indent(&mut self, span: Span, conflict: DiffConflict) {
        self.errors.push(Error::BadIndentForMatch(span, conflict));
    }

    pub(crate) fn err_unmatched(&mut self, span: Span, for_: &str) {
        self.errors.push(Error::Unmatched(span, for_.to_string()));
    }

    pub(crate) fn err_invalid_trait_member(&mut self, span: Span) {
        self.errors.push(Error::InvalidTraitMember(span));
    }

    pub(crate) fn err_invalid_nested_match(&mut self, previous: Span, new: Span) {
        self.errors
            .push(Error::InvalidNestedMatch { previous, new });
    }

    pub(crate) fn err_conflicting_bars(&mut self, err: IndentConflict) {
        self.errors.push(Error::ConflictingBars(err));
    }

    pub(crate) fn err_missing_return_type(&mut self, span: Span) {
        self.errors.push(Error::MissingReturnType(span));
    }

    pub(crate) fn err_nested_where(&mut self, previous: Span, kw: Span) {
        self.errors.push(Error::NestedWhere { previous, kw });
    }
}
