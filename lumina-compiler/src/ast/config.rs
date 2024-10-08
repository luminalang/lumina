use lumina_parser as parser;
use lumina_parser::{Declaration, Parser, Type};
use lumina_util::{Span, Tr};
use std::collections::HashMap;

#[derive(Default, Debug)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub authors: Vec<String>,
    pub parameters: Vec<String>,
    pub epanic: bool,
    pub prelude: String,
    pub dependencies: Vec<Dependency>,
    pub linker_args: Vec<String>,
    pub linker_libs: Vec<String>,
}

#[derive(Debug)]
pub struct Dependency {
    pub name: String,
    pub version: String,
    pub parameters: HashMap<String, Type<'static>>,
}

impl ProjectConfig {
    pub fn parse(src: &str) -> Result<Self, Error> {
        let mut parser = Parser::new(src);

        let mut project = ProjectConfig::default();
        project.prelude = String::from("std:prelude");

        while let Some((span, decl)) = parser.declaration() {
            match decl {
                Declaration::Type(mparam) => {
                    project.parameters.push(mparam.header.name.to_string());
                    match mparam.body {
                        parser::ty::DeclarationBody::None => {}
                        _ => return Err(Error::InvalidTy(span)),
                    }
                }
                Declaration::Val(decl) => project.parse_val(decl)?,

                _ => return Err(Error::InvalidDeclaration(span)),
            }
        }

        Ok(project)
    }

    fn parse_val(&mut self, val: parser::val::Declaration<'_>) -> Result<(), Error> {
        match val.name {
            "dependencies" => self.parse_deps(val.value),
            "name" => {
                self.name = name(val.value)?;
                Ok(())
            }
            "version" => {
                self.version = name(val.value)?;
                Ok(())
            }
            "epanic" => {
                self.epanic = bool(val.value)?;
                Ok(())
            }
            "authors" => self
                .parse_str_list(val.value)
                .map(|authors| self.authors.extend(authors)),
            "prelude" => {
                self.prelude = name(val.value)?;
                Ok(())
            }
            "linker_args" => self
                .parse_str_list(val.value)
                .map(|args| self.linker_args.extend(args)),
            "linker_libs" => self
                .parse_str_list(val.value)
                .map(|args| self.linker_libs.extend(args)),
            _ => Err(Error::InvalidVal(val.span)),
        }
    }

    fn parse_str_list(&mut self, expr: Tr<parser::Expr>) -> Result<Vec<String>, Error> {
        match expr.value {
            parser::Expr::List(elems, _) => elems.into_iter().map(|elem| name(elem)).collect(),
            _ => Err(Error::Expected(expr.span, "list")),
        }
    }

    fn parse_deps(&mut self, expr: Tr<parser::Expr>) -> Result<(), Error> {
        match expr.value {
            parser::Expr::List(deps, _) => deps.into_iter().try_for_each(|expr| match expr.value {
                parser::Expr::Record { fields, .. } => {
                    let mut dep = Dependency {
                        name: String::new(),
                        version: String::new(),
                        parameters: HashMap::new(),
                    };

                    fields.into_iter().try_for_each(|field| match field {
                        parser::Field::Assigned { field_path, bind: None, value: v } => {
                            match field_path.as_slice() {
                                &[value] => match *value {
                                    "name" => {
                                        dep.name = name(v)?;
                                        Ok(())
                                    }
                                    "version" => {
                                        dep.version = name(v)?;
                                        Ok(())
                                    }
                                    other => {
                                        let ty = ty_in_str_literal(v)?;
                                        dep.parameters.insert(other.to_string(), ty);
                                        Ok(())
                                    }
                                },
                                &[_, Tr { span, .. }, ..] => Err(Error::InvalidDep(span)),
                                &[] => Err(Error::InvalidDep(expr.span)),
                            }
                        }
                        _ => Err(Error::InvalidDep(expr.span)),
                    })?;

                    self.dependencies.push(dep);

                    Ok(())
                }
                _ => Err(Error::InvalidDep(expr.span)),
            }),
            _ => todo!(),
        }
    }
}

fn ty_in_str_literal(expr: Tr<parser::Expr>) -> Result<parser::Type<'static>, Error> {
    match expr.value {
        parser::Expr::Lit(parser::Literal::String(str)) => {
            let str = Box::leak(String::from(str).into_boxed_str());
            match parser::Parser::new(str).type_with_params() {
                None => Err(Error::InvalidTypeInStr(expr.span)),
                Some(ty) => Ok(ty.value),
            }
        }
        _ => Err(Error::InvalidTypeInStr(expr.span)),
    }
}

fn name(expr: Tr<parser::Expr>) -> Result<String, Error> {
    match expr.value {
        parser::Expr::Lit(parser::Literal::String(str)) => Ok(str.to_string()),
        _ => Err(Error::Expected(expr.span, "string")),
    }
}

fn bool(expr: Tr<parser::Expr>) -> Result<bool, Error> {
    match expr.value {
        parser::Expr::Call(path, ..) => match path.path.as_slice()[0] {
            "true" => return Ok(true),
            "false" => return Ok(false),
            _ => {}
        },
        _ => {}
    }

    Err(Error::InvalidVal(expr.span))
}

pub enum Error {
    InvalidDeclaration(Span),
    InvalidDep(Span),
    InvalidVal(Span),
    InvalidTy(Span),
    Expected(Span, &'static str),
    InvalidTypeInStr(Span),
}
