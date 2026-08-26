use crate::project::ProjectPath;
use lumina_parser as parser;
use lumina_parser::{Declaration, Parser};
use lumina_util::{Span, Tr};
use std::fmt;
use std::path::{Path, PathBuf};

#[derive(Default, Debug)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub authors: Vec<String>,
    pub parameters: Vec<String>,
    pub epanic: bool,
    pub super_debug: bool,
    pub dependencies: Vec<Dependency>,
    pub linker_args: Vec<String>,
    pub linker_libs: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Dependency {
    pub name: String,
    pub path: ProjectPath,
    pub version: String,
    pub indirect: bool,
}

impl ProjectConfig {
    pub fn parse(src: &str, lumina_path: &Path, project_path: &Path) -> Result<Self, Error> {
        let mut parser = Parser::new(src);

        let mut project = ProjectConfig::default();

        while let Some((span, decl)) = parser.item() {
            let parsed = match decl {
                Declaration::Val(decl) => project.parse_val(decl, lumina_path, project_path),
                _ => Err(Error::Invalid(span)),
            };

            if let Some(error) = parser.take_errors().pop() {
                return Err(Error::ParseError(error));
            } else if let Err(error) = parsed {
                return Err(error);
            }
        }

        Ok(project)
    }

    fn parse_val(
        &mut self,
        val: parser::val::Declaration<'_>,
        lumina_path: &Path,
        project_path: &Path,
    ) -> Result<(), Error> {
        match val.name {
            "dependencies" => self.parse_deps(val.value, lumina_path, project_path),
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
            "linker_args" => self
                .parse_str_list(val.value)
                .map(|args| self.linker_args.extend(args)),
            "linker_libs" => self
                .parse_str_list(val.value)
                .map(|args| self.linker_libs.extend(args)),
            _ => Err(Error::Invalid(val.span)),
        }
    }

    fn parse_str_list(&mut self, expr: Tr<parser::Expr>) -> Result<Vec<String>, Error> {
        match expr.value {
            parser::Expr::List(elems, _) => elems.into_iter().map(|elem| name(elem)).collect(),
            _ => Err(Error::ExpectedButGot(expr.span, "list")),
        }
    }

    fn parse_deps(
        &mut self,
        expr: Tr<parser::Expr>,
        lumina_path: &Path,
        project_path: &Path,
    ) -> Result<(), Error> {
        match expr.value {
            parser::Expr::List(deps, _) => deps.into_iter().try_for_each(|expr| match expr.value {
                parser::Expr::Record { fields, .. } => {
                    let mut dep = Dependency {
                        name: String::new(),
                        version: String::new(),
                        path: ProjectPath::empty(),
                        indirect: false,
                    };

                    let weird_dep_error = |span| {
                        Err(Error::ExpectedButGot(
                            span,
                            "a valid dependency field such as `version = \"1.0\"`",
                        ))
                    };

                    fields.into_iter().try_for_each(|field| {
                        match field.field_names.as_slice() {
                            &[value] => match *value {
                                "name" => {
                                    dep.name = just(value.span, field.value, name)?;
                                    Ok(())
                                }
                                "path" => {
                                    dep.path = just(value.span, field.value, |expr| {
                                        path(expr, lumina_path, project_path)
                                    })
                                    .map(ProjectPath)?;
                                    Ok(())
                                }
                                "version" => {
                                    dep.version = just(value.span, field.value, name)?;
                                    Ok(())
                                }
                                _ => weird_dep_error(value.span),
                            },
                            &[_, Tr { span, .. }, ..] => weird_dep_error(span),
                            &[] => weird_dep_error(expr.span),
                        }
                    })?;

                    self.dependencies.push(dep);

                    Ok(())
                }
                _ => Err(Error::ExpectedButGot(
                    expr.span,
                    "a dependency record such as `{ version = \"1.0\" }`",
                )),
            }),
            _ => todo!(),
        }
    }
}

fn just<'s, T, F>(span: Span, expr: Option<Tr<parser::Expr<'s>>>, f: F) -> Result<T, Error>
where
    F: FnOnce(Tr<parser::Expr<'s>>) -> Result<T, Error>,
{
    match expr {
        Some(expr) => f(expr),
        None => Err(Error::ExpectedButGot(span, "field value")),
    }
}

fn name<'s>(expr: Tr<parser::Expr<'s>>) -> Result<String, Error> {
    match expr.value {
        parser::Expr::Lit(parser::Literal::String(str)) => Ok(str.to_string()),
        _ => Err(Error::ExpectedButGot(expr.span, "string")),
    }
}

fn path<'s>(
    expr: Tr<parser::Expr<'s>>,
    lumina_path: &Path,
    project_path: &Path,
) -> Result<PathBuf, Error> {
    match expr.value {
        parser::Expr::Lit(parser::Literal::String(str)) => Ok(PathBuf::from(str)),
        parser::Expr::Operators { init, ops } => {
            let init = path(*init, lumina_path, project_path)?;

            ops.into_iter()
                .try_fold(init, |mut lhs, (op, rhs)| match *op {
                    "<>" => {
                        let rhs = path(rhs, lumina_path, project_path)?;
                        let rhs = rhs.strip_prefix("/").unwrap_or(&rhs);
                        lhs.push(rhs);
                        Ok(lhs)
                    }
                    _ => Err(Error::UnknownOperator(op.map(str::to_string))),
                })
        }
        parser::Expr::Call(apath, params) if params.is_empty() => match apath.path.as_slice() {
            ["path", "lumina"] => Ok(lumina_path.to_path_buf()),
            ["path", "project"] => Ok(project_path.to_path_buf()),
            _ => Err(Error::ExpectedButGot(expr.span, "path")),
        },
        _ => Err(Error::ExpectedButGot(expr.span, "path")),
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

    Err(Error::ExpectedButGot(expr.span, "`true` or `false`"))
}

#[derive(Debug)]
pub enum Error {
    ParseError(parser::Error),
    UnknownOperator(Tr<String>),
    // Syntacticly valid but not a valid config item
    Invalid(Span),
    // Partially known configuration option with error
    ExpectedButGot(Span, &'static str),
}

impl fmt::Display for Dependency {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{ name = {}, version = {}, path = {} }}",
            self.name,
            self.version,
            self.path.0.display()
        )
    }
}
