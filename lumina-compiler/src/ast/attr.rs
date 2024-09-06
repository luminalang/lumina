use crate::ast::Sources;
use derive_new::new;
use lumina_key as key;
use lumina_parser as parser;
use lumina_util::{Span, Spanned, Tr};

#[derive(new)]
pub struct ModuleAttr<'s> {
    #[new(default)]
    pub lang_items: Vec<(Tr<&'s str>, parser::Type<'s>)>,
}

impl<'s> ModuleAttr<'s> {
    pub fn parse(
        &mut self,
        module: key::Module,
        sources: &Sources,
        exprs: Vec<Tr<parser::Expr<'s>>>,
    ) {
        for expr in exprs {
            if let Err(err) = self.parse_attr(expr.as_ref()) {
                emit_err(module, sources, err);
            }
        }
    }

    pub fn parse_attr(&mut self, expr: Tr<&parser::Expr<'s>>) -> Result<(), Error> {
        let (entry, _params) = path(expr, "attribute name")?;
        match entry.path.as_slice() {
            ["langItem"] => lang_item(entry, &mut self.lang_items),
            _ => Err(Error::Unknown(expr.span)),
        }
    }
}

#[derive(Debug, Default, new, Clone)]
pub struct SharedAttr<'s> {
    #[new(default)]
    pub platforms: Vec<&'s str>,
    #[new(default)]
    pub lang_items: Vec<(Tr<&'s str>, parser::Type<'s>)>,
    #[new(default)]
    pub deprecated: Option<&'s str>,
    #[new(default)]
    pub public: bool,
}

#[derive(Debug, Default, Clone)]
pub struct FuncAttr<'s> {
    pub no_mangle: bool,
    pub precedence: Option<u32>,
    pub extern_: Option<String>,
    pub shared: SharedAttr<'s>,
}

#[derive(Debug, Default)]
pub struct TypeAttr<'s> {
    pub shared: SharedAttr<'s>,
}

impl<'s> TypeAttr<'s> {
    pub fn parse(
        module: key::Module,
        sources: &Sources,
        exprs: &[Tr<parser::Expr<'s>>],
    ) -> TypeAttr<'s> {
        let mut this = TypeAttr { shared: SharedAttr::new() };

        for expr in exprs {
            if let Err(err) = this.parse_attr(expr.as_ref()) {
                emit_err(module, sources, err);
            }
        }

        this
    }

    fn parse_attr(&mut self, expr: Tr<&parser::Expr<'s>>) -> Result<(), Error> {
        let (entry, params) = path(expr, "attribute name")?;
        match entry.path.as_slice() {
            _ => self.shared.parse_attr(expr.span, entry, params),
        }
    }
}

#[derive(Debug)]
pub struct ImplAttr<'s> {
    pub shared: SharedAttr<'s>,
}

fn emit_err(module: key::Module, sources: &Sources, err: Error) {
    let base = sources.error("invalid attribute").m(module);
    match err {
        Error::Expected(span, exp) => base.eline(span, format!("expected {exp}")),
        Error::Unknown(span) => base.eline(span, format!("unknown attribute")),
    }
    .emit()
}

impl<'s> FuncAttr<'s> {
    pub fn parse(
        module: key::Module,
        sources: &Sources,
        exprs: &[Tr<parser::Expr<'s>>],
    ) -> FuncAttr<'s> {
        let mut this = FuncAttr {
            no_mangle: false,
            precedence: None,
            shared: SharedAttr::new(),
            extern_: None,
        };

        for expr in exprs {
            if let Err(err) = this.parse_attr(expr.as_ref()) {
                emit_err(module, sources, err);
            }
        }

        this
    }

    fn parse_attr(&mut self, expr: Tr<&parser::Expr<'s>>) -> Result<(), Error> {
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
            _ => self.shared.parse_attr(expr.span, entry, params),
        }
    }
}

impl<'s> SharedAttr<'s> {
    fn parse_attr(
        &mut self,
        span: Span,
        entry: &parser::AnnotatedPath<'s>,
        params: &[Tr<parser::Expr<'s>>],
    ) -> Result<(), Error> {
        match entry.path.as_slice() {
            ["langItem"] => lang_item(entry, &mut self.lang_items),
            ["platform"] => {
                self.platforms
                    .extend(strings(params, "one or more platform names")?);
                Ok(())
            }
            ["pub"] => {
                self.public = true;
                Ok(())
            }
            _ => Err(Error::Unknown(span)),
        }
    }
}

fn lang_item<'s>(
    entry: &parser::AnnotatedPath<'s>,
    buf: &mut Vec<(Tr<&'s str>, parser::Type<'s>)>,
) -> Result<(), Error> {
    let annotation = &entry
        .for_segments
        .last()
        .expect("lang item without type annotation")
        .1;

    for (span, name, ty) in &annotation.assignments {
        buf.push(((*name).tr(*span), ty.clone()));
    }

    return Ok(());
}

fn path<'a, 's>(
    expr: Tr<&'a parser::Expr<'s>>,
    exp: &'static str,
) -> Result<(&'a parser::AnnotatedPath<'s>, &'a [Tr<parser::Expr<'s>>]), Error> {
    match expr.value {
        parser::Expr::Call(apath, params) => Ok((apath, params)),
        _ => Err(Error::Expected(expr.span, exp)),
    }
}

fn string<'a, 's>(expr: Tr<&'a parser::Expr<'s>>, exp: &'static str) -> Result<&'s str, Error> {
    match expr.value {
        parser::Expr::Lit(parser::Literal::String(name)) => Ok(name),
        _ => Err(Error::Expected(expr.span, exp)),
    }
}

fn num<'a, 's>(expr: Tr<&'a parser::Expr<'s>>) -> Result<u32, Error> {
    match expr.value {
        parser::Expr::Lit(parser::Literal::Int(false, n)) => Ok(*n as u32),
        _ => Err(Error::Expected(expr.span, "integer")),
    }
}

fn strings<'s>(exprs: &[Tr<parser::Expr<'s>>], exp: &'static str) -> Result<Vec<&'s str>, Error> {
    match exprs {
        [Tr { value: parser::Expr::List(elems), .. }] => strings(elems, exp),
        _ => Ok(exprs
            .iter()
            .map(|expr| string(expr.as_ref(), exp))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .collect()),
    }
}

pub enum Error {
    Expected(Span, &'static str),
    Unknown(Span),
}
