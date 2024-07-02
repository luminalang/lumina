use super::{ty::ForallAnnotation, Parser, T};
use itertools::Itertools;
use lumina_util::{Highlighting, Identifier, Span, Spanned, Tr};
use std::fmt;

#[derive(Clone, Debug)]
pub struct Declaration<'a> {
    pub path: Tr<Identifier<'a>>,
    pub module_forall_annotation: ForallAnnotation<'a>,
    pub assign_to: Option<Tr<&'a str>>,
    pub exposing: Vec<Tr<Exposed<'a>>>,
}

#[derive(Clone, Debug)]
pub struct Exposed<'a> {
    pub name: &'a str,
    pub members: Members<'a>,
}

#[derive(Clone, Debug)]
pub enum Members<'a> {
    All(Span),
    None,
    Members(Vec<Tr<&'a str>>),
}

impl<'a> Parser<'a> {
    pub fn r#use(&mut self) -> Option<Declaration<'a>> {
        let (t, span) = self.lexer.peek();
        let (span, module_forall_annotation) = match t {
            T::Path => (self.next_then(|_| span), ForallAnnotation::none()),
            T::AnnotatedPath => {
                self.progress();
                let annotation = self.forall_annotation()?;
                (span.extend_length(-1), annotation)
            }
            other => {
                self.err_unexpected_token((other, span), "a path");
                self.recover_next_toplevel();
                return None;
            }
        };

        let path = Identifier::parse(self.take(span)).unwrap().tr(span);

        let (t, span) = self.lexer.peek();
        let (exposing, assign_to_path) = match t {
            T::OpenList => {
                self.progress();
                (self.exposing(span)?, None)
            }
            T::Path => {
                self.progress();
                let path = Identifier::parse(self.take(span)).unwrap();
                let assign_to_path = Some(path.tr(span));

                let exposing = match self.lexer.peek() {
                    (T::OpenList, span) => {
                        self.progress();
                        self.exposing(span)?
                    }
                    _ => vec![],
                };

                (exposing, assign_to_path)
            }

            // TODO: we should be new-line sensitive so we can provide better errors here.
            //
            // Right now we're just handing the responsibility to the top-level parser
            _ => (vec![], None),
        };

        let assign_to = assign_to_path.and_then(|path| match path.as_name() {
            Some(name) => Some(name.tr(path.span)),
            None => {
                self.err_expected_but_got(path.span, "a name", "a path");
                None
            }
        });

        Some(Declaration {
            path,
            module_forall_annotation,
            assign_to,
            exposing,
        })
    }

    fn exposing(&mut self, start: Span) -> Option<Vec<Tr<Exposed<'a>>>> {
        self.shared_list(start, Parser::exposed, None)
            .map(|(elems, _)| elems)
    }

    fn exposed(&mut self) -> Option<Tr<Exposed<'a>>> {
        let Tr { value: name, span } = self.expect_name("member")?;

        let members = match self.lexer.peek() {
            (T::OpenList, _) => self.next_then(Parser::members),
            _ => Members::None,
        };

        Some(Exposed { name, members }.tr(span))
    }

    fn members(&mut self) -> Members<'a> {
        let mut members = vec![];

        let recovery = [T::DotDot, T::CloseList, T::Path, T::Comma];

        loop {
            match self.lexer.peek() {
                (T::Comma, _) => {
                    self.progress();
                    continue;
                }
                (T::DotDot, span) => {
                    self.progress();
                    let _ = self.expect(T::CloseList);
                    break Members::All(span);
                }
                (T::CloseList, _) => {
                    self.progress();
                    break Members::Members(members);
                }
                (T::Operator, span) | (T::Path, span) => {
                    self.progress();
                    let path = Identifier::parse(self.take(span)).unwrap();
                    if let Some(name) = path.as_name() {
                        members.push(name.tr(span));
                    } else {
                        self.err_expected_but_got(span, "a name", "a path");
                    }

                    if [T::Comma, T::CloseList].contains(&self.lexer.peek().0) {
                        continue;
                    }
                }
                other => {
                    self.err_unexpected_token(other, "member");
                }
            }

            if !recovery.contains(&self.recover_for(recovery, false)) {
                break Members::Members(members);
            }
        }
    }
}

impl<'a> fmt::Display for Declaration<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let use_ = "use".keyword();
        let ol = '['.symbol();
        let cl = ']'.symbol();
        write!(
            f,
            "{use_} {}{} {}{ol}{}{cl}",
            self.path.path(),
            self.module_forall_annotation,
            if let Some(name) = self.assign_to {
                format!("{name} ")
            } else {
                "".into()
            },
            self.exposing.iter().format(", ")
        )
    }
}

impl<'a> fmt::Display for Exposed<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ol = '['.symbol();
        let cl = ']'.symbol();
        write!(
            f,
            "{} {ol}{}{cl}",
            self.name,
            match &self.members {
                Members::None => "".to_string(),
                Members::All(..) => "..".to_string(),
                Members::Members(set) => set.iter().format(", ").to_string(),
            }
        )
    }
}
