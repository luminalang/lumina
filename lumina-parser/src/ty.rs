use super::{func, r#impl, select, AnnotatedPath, Expr, ListLength, Parser, T};
use itertools::Itertools;
use lumina_key as key;
use lumina_key::Map;
use lumina_util::{Highlighting, Identifier, Span, Spanned, Tr};
use std::fmt;

#[derive(Debug)]
pub struct Declaration<'a> {
    pub header: Header<'a>,
    pub body: DeclarationBody<'a>,
    pub attributes: Vec<Tr<Expr<'a>>>,
}

#[derive(Debug)]
pub struct Header<'a> {
    pub span: Span,
    pub name: &'a str,
    pub type_params: Map<key::Generic, &'a str>,
}

#[derive(Debug)]
pub enum DeclarationBody<'a> {
    Record(RecordBody<'a>),
    Sum(SumBody<'a>),
    Trait(TraitBody<'a>),
    None,
}

impl<'a> DeclarationBody<'a> {
    pub fn empty_record() -> Self {
        DeclarationBody::Record(RecordBody { fields: Map::new() })
    }
}

#[derive(Debug)]
pub struct SumBody<'a> {
    pub variants: Map<key::Variant, (Span, &'a str, Vec<Tr<Type<'a>>>)>,
}

#[derive(Debug)]
pub struct TraitBody<'a> {
    pub associations: Map<key::AssociatedType, r#impl::Association<'a>>,
    pub methods: Map<key::Method, func::Declaration<'a>>,
}

#[derive(Debug)]
pub struct RecordBody<'a> {
    pub fields: Map<key::Field, (Span, &'a str, Type<'a>)>,
}

#[derive(Debug, Clone)]
pub enum Type<'a> {
    Closure(Vec<Tr<Self>>, Box<Tr<Self>>),
    FnPointer(Vec<Tr<Self>>, Box<Tr<Self>>),
    Pointer(Box<Tr<Self>>),
    Defined(AnnotatedPath<'a>, Vec<Tr<Self>>),
    Tuple(Vec<Tr<Self>>),
    List(Vec<Tr<Self>>, ListLength<'a>),
    Poison,
}

impl<'a> Type<'a> {
    pub fn name(name: &'a str, params: Vec<Tr<Type<'a>>>) -> Self {
        Type::Defined(
            AnnotatedPath::without(Identifier::parse(name).unwrap()),
            params,
        )
    }
}

impl<'a> Parser<'a> {
    pub fn type_decleration(&mut self, attributes: Vec<Tr<Expr<'a>>>) -> Option<Declaration<'a>> {
        let Some(name) = self.expect_name("type declaration") else {
            self.recover_next_toplevel();
            return None;
        };

        let type_params = self.type_declaration_parameters();

        select! { self, "record or sum type body", _span peeked: true;
            T::OpenCurly => self.next_then(|this| this.record_declaration()),
            T::Equal => {
                self.lexer.next();
                if self.next_is(|t| t == T::OpenCurly).is_some() {
                    self.record_declaration()
                } else {
                    self.sum_declaration()
                }
            },
            t if t.is_header() => Some(DeclarationBody::None),
        }
        .map(|body| Declaration {
            header: Header {
                name: *name,
                type_params,
                span: name.span.extend(self.lexer.peek_line_sensitive().1),
            },
            body,
            attributes,
        })
    }

    fn type_declaration_parameters(&mut self) -> Map<key::Generic, &'a str> {
        let mut params = Map::new();

        loop {
            match self.lexer.peek_line_sensitive() {
                (T::Path, span) => {
                    let raw = self.take(span);
                    if Identifier::parse(raw).unwrap().is_name() {
                        params.push(raw);
                    } else {
                        self.err_expected_but_got(span, "a name", "a path");
                    }
                    self.progress();
                }
                _ => break params,
            }
        }
    }

    pub fn record_declaration(&mut self) -> Option<DeclarationBody<'a>> {
        let mut fields = Map::new();

        if self.next_is(|t| t == T::CloseCurly).is_some() {
            return Some(DeclarationBody::Record(RecordBody { fields }));
        }

        loop {
            let Some(name) = self.expect_name("record field") else {
                self.recover_next_toplevel();
                return None;
            };

            let t = self.type_newline_sensitive();

            if let Some(t) = t {
                fields.push((name.span.extend(t.span), *name, t.value));
            }

            select! { self, "the next record field" sensitive: true;
                T::NewLines  => {},
                T::Comma  => {},
                T::CloseCurly => return Some(DeclarationBody::Record(RecordBody  { fields })),
            }

            // skip optional trailing newlines and commas
            loop {
                match self.lexer.peek_line_sensitive().0 {
                    T::NewLines | T::Comma => self.progress(),
                    T::CloseCurly => {
                        self.progress();
                        return Some(DeclarationBody::Record(RecordBody { fields }));
                    }
                    _ => break,
                }
            }
        }
    }

    pub fn sum_declaration(&mut self) -> Option<DeclarationBody<'a>> {
        let mut variants = Map::new();

        loop {
            let Some(name) = self.expect_name("sum type variant") else {
                self.recover_next_toplevel();
                return None;
            };

            if let Some((ts, span)) = self.types(name.span, false) {
                variants.push((span, *name, ts));

                if self.next_is(|t| t == T::Bar).is_some() {
                    continue;
                }

                return Some(DeclarationBody::Sum(SumBody { variants }));
            } else {
                match self.recover_for([T::Bar], false) {
                    T::Bar => self.progress(),
                    _ => return None,
                }
            }
        }
    }

    pub fn r#trait(&mut self, attributes: Vec<Tr<Expr<'a>>>) -> Option<Declaration<'a>> {
        let Some(name) = self.expect_name("trait declaration") else {
            self.recover_next_toplevel();
            return None;
        };

        let type_params = self.type_declaration_parameters();
        let span = name.span.extend(self.lexer.peek_line_sensitive().1);

        select! { self, "end of line", _span sensitive: true;
            T::NewLines => {},
            T::Equal => {}
        }

        let (methods, associations) = self.methods_and_associations()?;

        Some(Declaration {
            header: Header { span, name: *name, type_params },
            body: DeclarationBody::Trait(TraitBody { associations, methods }),
            attributes,
        })
    }

    pub fn type_with_params(&mut self) -> Option<Tr<Type<'a>>> {
        self.r#type(true, true)
    }

    pub fn type_newline_sensitive(&mut self) -> Option<Tr<Type<'a>>> {
        self.r#type(true, true)
    }

    fn r#type(&mut self, with_params: bool, newline_sensitive: bool) -> Option<Tr<Type<'a>>> {
        select! { self, "a type", span sensitive: newline_sensitive;
            T::Path => self.ty_identifier(with_params, newline_sensitive, false, span),
            T::AnnotatedPath => self.ty_identifier(with_params, newline_sensitive, true, span.extend_length(-1)),
            T::OpenParen => self.ty_parenthesis(span),
            T::Fn | T::FnPtr if !with_params => {
                self.err_fn_needs_parenthesis(span);
                self.recover_next_toplevel();
                None
            },
            T::Fn => self.function(span, Type::Closure),
            T::FnPtr => self.function(span, Type::FnPointer),
            T::OpenList => self.ty_list(span),
            T::Operator => {
                if self.take(span).bytes().all(|b| b == b'*') {
                    self.pointer(span, with_params, newline_sensitive)
                } else {
                    None
                }
            }
        }
    }

    fn ty_identifier(
        &mut self,
        with_params: bool,
        newline_sensitive: bool,
        annotated: bool,
        span: Span,
    ) -> Option<Tr<Type<'a>>> {
        let str = self.take(span);
        let path = Identifier::parse(str).unwrap();

        let path = if annotated {
            self.shared_anot_path(path)?
        } else {
            AnnotatedPath::without(path)
        };

        let (params, span) = if with_params {
            let (params, s) = self.types(span, newline_sensitive)?;
            (params, span.extend(s))
        } else {
            (vec![], span)
        };

        Some(Type::Defined(path, params).tr(span))
    }

    fn ty_list(&mut self, start: Span) -> Option<Tr<Type<'a>>> {
        self.shared_list(
            start,
            |parser| parser.type_with_params().map(|a| a),
            Some(Type::Poison.tr(start)),
        )
        .map(|(elems, ender, end)| Type::List(elems, ender).tr(start.extend(end)))
    }

    fn pointer(
        &mut self,
        span: Span,
        with_params: bool,
        newline_sensitive: bool,
    ) -> Option<Tr<Type<'a>>> {
        if !with_params && self.try_take(span.following(1)) == Some(" ") {
            self.err_unexpected_token((T::Operator, span), "a type");
            return None;
        }

        let inner = self.r#type(with_params, newline_sensitive);
        let mut end = span;

        inner
            .map(|t| {
                end = t.span;
                (0..span.length as i32).rev().fold(t, |prev, length| {
                    let span = prev.span.move_indice(-length);
                    Type::Pointer(Box::new(prev)).tr(span)
                })
            })
            .map(|t| t.value.tr(span.extend(end)))
    }

    fn function<OUT, CON>(&mut self, start: Span, constr: CON) -> Option<Tr<OUT>>
    where
        CON: FnOnce(Vec<Tr<Type<'a>>>, Box<Tr<Type<'a>>>) -> OUT,
    {
        let mut ptypes = vec![];

        loop {
            select! { self, "`->`, `)` or `,` for this function type", span peeked: true;
                T::Arrow => {
                  self.progress();
                  break self.r#type(true, false)
                      .and_then(|ret| {
                          let span = span.extend(ret.span);
                          Some(constr(ptypes, Box::new(ret)).tr(span))
                      });
                },
                t if t.is_valid_start_of_type() => {
                    match self.r#type(true, false) {
                        Some(t) => ptypes.push(t),
                        None => {
                            ptypes.push(Type::Poison.tr(start));
                            self.recover_until(|t| t.is_valid_start_of_type() || t == T::Arrow, false);
                        }
                    }
                },
                _ => return None,
            }
        }
    }

    fn ty_parenthesis(&mut self, start: Span) -> Option<Tr<Type<'a>>> {
        let mut elements = vec![];

        if let Some(end) = self.next_is(|t| t == T::CloseParen) {
            return Some(Type::Tuple(elements).tr(start.extend(end)));
        }

        loop {
            match self.r#type(true, false) {
                None => {
                    elements.push(Type::Poison.tr(start));
                    // TODO: do we want to use `TYPE_RECOVERY` instead?
                    self.recover_for([T::Comma, T::CloseParen], false);
                }
                Some(t) => elements.push(t),
            }

            select! { self, "a type or end to the type", span;
                T::Comma => {},
                T::CloseParen => break self.finalize_tuple(elements).map(|t| t.tr(start.extend(span)))
            }
        }
    }

    fn finalize_tuple(&mut self, mut elements: Vec<Tr<Type<'a>>>) -> Option<Type<'a>> {
        if elements.len() == 1 {
            Some(elements.remove(0).value)
        } else {
            Some(Type::Tuple(elements))
        }
    }

    pub fn types(
        &mut self,
        mut end: Span,
        newline_sensitive: bool,
    ) -> Option<(Vec<Tr<Type<'a>>>, Span)> {
        let mut params = vec![];

        loop {
            let (t, _) = self.lexer.peek_line_sensitive();

            if t == T::NewLines {
                if newline_sensitive {
                    break Some((params, end));
                } else {
                    self.lexer.next();
                    continue;
                }
            }

            if t.is_valid_start_of_pattern_param() {
                let type_ = self.r#type(false, newline_sensitive)?;
                end = type_.span;
                params.push(type_);
            } else {
                break Some((params, end));
            }
        }
    }

    pub fn forall_annotation(&mut self) -> Option<ForallAnnotation<'a>> {
        let mut assignments = vec![];

        loop {
            // We parse a type since `Trait(option int)` sugar exists for annotating `self`
            //
            // It's easier to then un-type it if we encounter `as`
            let first = self.lexer.peek();
            match self.r#type(true, false) {
                Some(ty) => {
                    let t = self.lexer.peek();
                    match t.0 {
                        T::Comma | T::CloseParen => {
                            assignments.push((ty.span, "self", ty.value));
                        }
                        T::As => {
                            self.progress();
                            match ty.value {
                                Type::Defined(path, params)
                                    if params.is_empty() && path.path.is_name() =>
                                {
                                    let generic = path.path.as_name().unwrap();
                                    if let Some(ty) = self.r#type(true, false) {
                                        assignments.push((ty.span, generic, ty.value));
                                    } else {
                                        self.recover_for([T::Comma, T::CloseParen], false);
                                    }
                                }
                                _ => {
                                    self.err_unexpected_token(first, "generic name");
                                    self.recover_for([T::Comma, T::CloseParen], false);
                                }
                            }
                        }
                        _ => {
                            self.err_unexpected_token(t, "`as`");
                            self.recover_for([T::Comma, T::CloseParen], false);
                        }
                    }
                }
                None => {
                    self.err_unexpected_token(first, "type or generic name");
                    self.recover_for([T::Comma, T::CloseParen], false);
                }
            }

            select! { self, "`,` or `)`";
                T::CloseParen => break,
                T::Comma => continue
            };
        }

        Some(ForallAnnotation { assignments })
    }
}

#[derive(Clone, Debug)]
pub struct ForallAnnotation<'a> {
    pub assignments: Vec<(Span, &'a str, Type<'a>)>,
}

impl<'a> ForallAnnotation<'a> {
    pub fn none() -> Self {
        Self { assignments: vec![] }
    }
}

impl T {
    pub fn is_valid_start_of_type(self) -> bool {
        match self {
            T::Dot | T::Arrow | T::Comma | T::OpenParen | T::OpenList | T::CloseCurly => true,
            _ => false,
        }
    }
}

impl<'a> fmt::Display for Declaration<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let type_ = "type".keyword();
        let eq = '='.symbol();

        write!(f, "{type_} {}", self.header)?;

        match &self.body {
            DeclarationBody::Record(RecordBody { fields }) => {
                write!(
                    f,
                    " {{\n  {}\n}}",
                    fields
                        .values()
                        .format_with("\n  ", |(_, name, type_), f| f(&format_args!(
                            "{name} {type_}",
                        )))
                )
            }
            DeclarationBody::Sum(SumBody { variants }) => {
                let small = variants.len() < 3;
                let sep = if small { " | " } else { "\n  | " };

                let var_fmt = variants.values().format_with(sep, |(_, name, types), f| {
                    f(&format_args!("{} {}", name, types.iter().format(" ")))
                });

                if small {
                    write!(f, " {eq} {}", var_fmt,)
                } else {
                    write!(f, "\n  {eq} {}", var_fmt)
                }
            }
            DeclarationBody::Trait(TraitBody { associations, methods }) => {
                writeln!(f)?;

                for assoc in associations.values() {
                    writeln!(f, "  {assoc}")?;
                }
                if !associations.is_empty() {
                    writeln!(f)?;
                }

                write!(f, "  {}", methods.values().format("\n  "))
            }
            DeclarationBody::None => "".fmt(f),
        }
    }
}

impl<'a> fmt::Display for Header<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.name.path().fmt(f)?;
        if self.type_params.is_empty() {
            Ok(())
        } else {
            write!(f, " {}", self.type_params.values().format(" "))
        }
    }
}

impl<'a> fmt::Display for ForallAnnotation<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = '('.symbol();
        let cp = ')'.symbol();
        let as_ = "as".keyword();

        write!(
            f,
            "{op}{}{cp}",
            self.assignments
                .iter()
                .format_with(", ", |(_, name, type_), f| f(&format_args!(
                    "{name} {as_} {}",
                    type_.type_()
                )))
        )
    }
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Poison => "poison".fmt(f),
            Type::Closure(ptypes, returns) if ptypes.is_empty() => write!(f, "fn({})", returns),
            Type::Closure(ptypes, returns) => {
                write!(f, "fn({} -> {})", ptypes.iter().format(", "), returns)
            }
            Type::FnPointer(ptypes, returns) if ptypes.is_empty() => {
                write!(f, "fnptr({})", returns)
            }
            Type::FnPointer(ptypes, returns) => {
                write!(f, "fnptr({} -> {})", ptypes.iter().format(", "), returns)
            }
            Type::Pointer(inner) => write!(f, "*{inner}"),
            Type::Defined(path, params) if params.is_empty() => path.fmt(f),
            Type::Defined(path, params) => write!(f, "({} {})", path, params.iter().format(" ")),
            Type::Tuple(elems) => write!(f, "({})", elems.iter().format(", ")),
            Type::List(elems, length) => write!(f, "[{}{length}]", elems.iter().format(", ")),
        }
    }
}
