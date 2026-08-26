use crate::errors;
use crate::prelude::*;
use lumina_parser as parser;
use lumina_typesystem::IntSize;

#[derive(Debug, new)]
pub struct ProjectAttr<'s> {
    #[new(default)]
    pub lang_items: Vec<(Tr<&'s str>, parser::Type<'s>)>,
}

impl<'s> ProjectAttr<'s> {
    pub fn parse(&mut self, exprs: Vec<Tr<parser::Expr<'s>>>) {
        for expr in exprs {
            if let Err(err) = self.parse_attr(expr.as_ref()) {
                emit_attr_error(err);
            }
        }
    }

    pub fn parse_attr(&mut self, expr: Tr<&parser::Expr<'s>>) -> Result<(), AttrError> {
        let (entry, _params) = path(expr, "attribute name")?;
        match entry.path.as_slice() {
            ["langItem"] => lang_item(entry, &mut self.lang_items),
            _ => Err(AttrError::Unknown(expr.span)),
        }
    }
}

#[derive(Debug, Default, new, Clone)]
pub struct SharedAttr<'s> {
    #[new(default)]
    pub platforms: Vec<&'s str>,
    // #[new(default)]
    // pub lang_items: Vec<(Tr<&'s str>, parser::Type<'s>)>,
    #[new(default)]
    pub deprecated: Option<&'s str>,
    #[new(default)]
    pub public: bool,
}

#[derive(Debug, Clone, Default)]
pub struct FuncAttr {
    pub no_mangle: bool,
    pub precedence: Option<u32>,
    pub extern_: Option<String>,
}

#[derive(Debug, Default, Clone)]
pub struct TypeAttr {
    pub repr: Repr,
}

#[derive(Debug, Default, Clone)]
pub struct ValAttr {
    pub no_mangle: bool,
    pub extern_: Option<String>,
}

#[derive(Debug, Default, Clone)]
pub struct UseAttr {}

#[derive(Debug, Default, Clone)]
pub struct AliasAttr {}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Repr {
    #[default]
    Lumina,
    C,
    Packed,
    Align(u8),
    Enum(IntSize),
}

impl TypeAttr {
    pub fn parse<'s>(exprs: &[Tr<parser::Expr<'s>>]) -> (SharedAttr<'s>, TypeAttr) {
        let mut shared = SharedAttr::new();
        let mut this = TypeAttr { repr: Repr::default() };

        for expr in exprs {
            if let Err(err) = this.parse_attr(&mut shared, expr.as_ref()) {
                emit_attr_error(err);
            }
        }

        (shared, this)
    }

    fn parse_attr<'s>(
        &mut self,
        shared: &mut SharedAttr<'s>,
        expr: Tr<&parser::Expr<'s>>,
    ) -> Result<(), AttrError> {
        let (entry, params) = path(expr, "attribute name")?;
        match entry.path.as_slice() {
            ["repr"] => self.parse_repr(expr.span, params),
            _ => shared.parse_attr(expr.span, entry, params),
        }
    }

    fn parse_repr<'s>(
        &mut self,
        span: Span,
        params: &[Tr<parser::Expr<'s>>],
    ) -> Result<(), AttrError> {
        if params.is_empty() {
            return Err(AttrError::Expected(span, "argument for repr"));
        }

        if let Ok(name) = name(params[0].as_ref()) {
            match name {
                "align" if params.len() != 2 => Err(AttrError::Expected(
                    params[0].span.move_indice(5),
                    "integer argument for `repr align`",
                )),
                "align" => num(params[1].as_ref()).map(|n| self.repr = Repr::Align(n as u8)),
                "packed" => {
                    self.repr = Repr::Packed;
                    Ok(())
                }
                _ => {
                    match name.as_bytes() {
                        [s @ (b'u' | b'i'), _, ..] => {
                            if let Ok(size) = name[1..].parse::<u8>() {
                                self.repr = Repr::Enum(IntSize::new(*s == b'i', size));
                                return Ok(());
                            }
                        }
                        _ => {}
                    }

                    Err(AttrError::Expected(
                        params[0].span,
                        "valid repr such as `align` or `packed`",
                    ))
                }
            }
        } else {
            if let Ok(str) = string(params[0].as_ref(), "") {
                match str {
                    "C" => self.repr = Repr::C,
                    "lumina" => self.repr = Repr::Lumina,
                    _ => return Err(AttrError::UnknownRepr(params[0].span, str.to_string())),
                }

                Ok(())
            } else {
                Err(AttrError::Expected(span, "string or identifier"))
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ImplAttr {}

impl ImplAttr {
    pub fn parse<'s>(exprs: &[Tr<parser::Expr<'s>>]) -> (SharedAttr<'s>, ImplAttr) {
        let mut shared = SharedAttr::default();
        let mut this = ImplAttr::default();

        for expr in exprs {
            if let Err(err) = this.parse_attr(&mut shared, expr.as_ref()) {
                emit_attr_error(err);
            }
        }

        (shared, this)
    }

    fn parse_attr<'s>(
        &mut self,
        shared: &mut SharedAttr<'s>,
        expr: Tr<&parser::Expr<'s>>,
    ) -> Result<(), AttrError> {
        let (entry, params) = path(expr, "attribute name")?;
        shared.parse_attr(expr.span, entry, params)
    }
}

impl FuncAttr {
    pub fn parse<'s>(exprs: &[Tr<parser::Expr<'s>>]) -> (SharedAttr<'s>, FuncAttr) {
        let mut shared = SharedAttr::new();
        let mut this = FuncAttr { no_mangle: false, precedence: None, extern_: None };

        for expr in exprs {
            if let Err(err) = this.parse_attr(&mut shared, expr.as_ref()) {
                emit_attr_error(err);
            }
        }

        (shared, this)
    }

    fn parse_attr<'s>(
        &mut self,
        shared: &mut SharedAttr<'s>,
        expr: Tr<&parser::Expr<'s>>,
    ) -> Result<(), AttrError> {
        let (entry, params) = path(expr, "attribute name")?;
        match entry.path.as_slice() {
            ["no_mangle"] => {
                self.no_mangle = true;
                Ok(())
            }
            ["precedence"] => {
                self.precedence = Some(num(params[0].as_ref())?);
                Ok(())
            }
            ["extern"] => {
                let name = string(params[0].as_ref(), "function symbol to link to")?;
                self.extern_ = Some(name.to_string());
                Ok(())
            }
            _ => shared.parse_attr(expr.span, entry, params),
        }
    }
}

impl ValAttr {
    pub fn parse<'s>(exprs: &[Tr<parser::Expr<'s>>]) -> (SharedAttr<'s>, ValAttr) {
        let mut shared = SharedAttr::default();
        let mut this = ValAttr { no_mangle: false, extern_: None };

        for expr in exprs {
            if let Err(err) = this.parse_attr(&mut shared, expr.as_ref()) {
                emit_attr_error(err);
            }
        }

        (shared, this)
    }

    fn parse_attr<'s>(
        &mut self,
        shared: &mut SharedAttr<'s>,
        expr: Tr<&parser::Expr<'s>>,
    ) -> Result<(), AttrError> {
        let (entry, params) = path(expr, "attribute name")?;
        match entry.path.as_slice() {
            ["no_mangle"] => {
                self.no_mangle = true;
                Ok(())
            }
            ["extern"] => {
                let name = string(params[0].as_ref(), "value symbol to link to")?;
                self.extern_ = Some(name.to_string());
                Ok(())
            }
            _ => shared.parse_attr(expr.span, entry, params),
        }
    }
}

impl<'s> SharedAttr<'s> {
    fn parse_attr(
        &mut self,
        span: Span,
        entry: &parser::AnnotatedPath<'s>,
        params: &[Tr<parser::Expr<'s>>],
    ) -> Result<(), AttrError> {
        match entry.path.as_slice() {
            // ["langItem"] => lang_item(entry, &mut self.lang_items),
            ["platform"] => {
                self.platforms
                    .extend(strings(params, "one or more platform names")?);
                Ok(())
            }
            ["pub"] => {
                self.public = true;
                Ok(())
            }
            _ => Err(AttrError::Unknown(span)),
        }
    }
}

fn lang_item<'s>(
    entry: &parser::AnnotatedPath<'s>,
    buf: &mut Vec<(Tr<&'s str>, parser::Type<'s>)>,
) -> Result<(), AttrError> {
    let annotation = &entry
        .for_segments
        .last()
        .expect("lang item without type annotation")
        .1;

    for (name, ty) in &annotation.assignments {
        buf.push((*name, (**ty).clone()));
    }

    return Ok(());
}

fn path<'a, 's>(
    expr: Tr<&'a parser::Expr<'s>>,
    exp: &'static str,
) -> Result<(&'a parser::AnnotatedPath<'s>, &'a [Tr<parser::Expr<'s>>]), AttrError> {
    match expr.value {
        parser::Expr::Call(apath, params) => Ok((apath, params)),
        _ => Err(AttrError::Expected(expr.span, exp)),
    }
}

fn name<'a, 's>(expr: Tr<&'a parser::Expr<'s>>) -> Result<&'s str, AttrError> {
    match expr.value {
        parser::Expr::Call(apath, params) if apath.path.is_name() && params.is_empty() => {
            Ok(apath.path.as_name().unwrap())
        }
        _ => Err(AttrError::Expected(
            expr.span,
            "identifier without parameters",
        )),
    }
}

fn string<'a, 's>(expr: Tr<&'a parser::Expr<'s>>, exp: &'static str) -> Result<&'s str, AttrError> {
    match expr.value {
        parser::Expr::Lit(parser::Literal::String(name)) => Ok(name),
        _ => Err(AttrError::Expected(expr.span, exp)),
    }
}

fn num<'a, 's>(expr: Tr<&'a parser::Expr<'s>>) -> Result<u32, AttrError> {
    match expr.value {
        parser::Expr::Lit(parser::Literal::Int(false, n)) => Ok(*n as u32),
        _ => Err(AttrError::Expected(expr.span, "integer")),
    }
}

fn strings<'s>(
    exprs: &[Tr<parser::Expr<'s>>],
    exp: &'static str,
) -> Result<Vec<&'s str>, AttrError> {
    match exprs {
        [Tr { value: parser::Expr::List(elems, _), .. }] => strings(elems, exp),
        _ => Ok(exprs
            .iter()
            .map(|expr| string(expr.as_ref(), exp))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .collect()),
    }
}

#[derive(Debug, Clone)]
pub enum AttrError {
    Expected(Span, &'static str),
    UnknownRepr(Span, String),
    Unknown(Span),
}

fn emit_attr_error(err: AttrError) {
    match err {
        AttrError::Expected(span, expected) => {
            errors::err("invalid attribute")
                .line(span, format!("expected {expected}"))
                .emit();
        }
        AttrError::UnknownRepr(span, repr) => {
            errors::err("invalid attribute")
                .line(span, format!("unknown repr `{repr}`"))
                .emit();
        }
        AttrError::Unknown(span) => {
            errors::err("invalid attribute")
                .line(span, "unknown attribute")
                .emit();
        }
    }
}
