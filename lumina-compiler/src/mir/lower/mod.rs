use super::func::{InstInfo, Local};
use super::tcheck::emit_record_error;
use super::tyfmt::TyFmtState;
use super::{Current, LangItems, RSolver, ReadOnlyBytes, Verify};
use crate::prelude::*;

mod expr;
pub use expr::Expr;
pub mod pat;

use derive_new::new;
use lumina_typesystem::{
    implicitly_declare_generic, Bitsize, ConcreteInst, Container, Finalizer, Forall, ForeignInst,
    FuncKind, GenericKind, IType, ImplIndex, RecordVar, TEnv, Type, Var,
};
use lumina_util::Highlighting;
use lumina_util::ParamFmt;
use std::fmt;

#[derive(new)]
pub struct Function {
    pub typing: ConcreteTyping,
    pub lambdas: Map<key::Lambda, Lambda>,
    pub lcaptures: Map<key::Lambda, Vec<key::Bind>>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct ConcreteTyping {
    pub forall: Forall<'static, Type>,
    pub params: Vec<Type>,
    pub returns: Type,
}

#[derive(Debug, Clone, new)]
pub struct CallTypes {
    pub params: Map<key::Param, Type>,
    pub ret: Type,
}

#[derive(new)]
pub struct Lambda {
    pub typing: ConcreteTyping,
    pub expr: Expr,
}

#[derive(new)]
pub struct Lower<'a, 's> {
    iquery: &'a ImplIndex,

    env: &'a mut TEnv<'s>,

    read_only_table: &'a mut ModMap<key::ReadOnly, (ReadOnlyBytes, Type)>,

    forall: &'a mut Forall<'s, IType>,
    lforalls: &'a mut Map<key::Lambda, Forall<'s, IType>>,
    pub current: &'a mut Current,
    pub implicits: bool,

    items: LangItems,
    rsolver: RSolver<'a, 's>,
    vnames: &'a ModMap<key::Sum, Map<key::SumVariant, Tr<&'s str>>>,
    vtypes: &'a ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,

    #[new(default)]
    pub errors: Vec<FinError<'s>>,

    #[new(default)]
    lambdas: Map<key::Lambda, Lambda>,
}

pub enum FinError<'s> {
    TS(lumina_typesystem::FinError<'s>),
    UnreachablePattern(Span),
    MissingPatterns(Span, Vec<pat::MissingPattern>),
    InvalidCast(Tr<Type>, Type),
}

impl<'a, 's> Lower<'a, 's> {
    fn finalizer<F, T>(&mut self, and_then: F) -> T
    where
        F: FnOnce(Finalizer<'_, '_, 's>) -> (T, Vec<lumina_typesystem::FinError<'s>>),
    {
        let module = self.current.fkey.module;
        let system = self.rsolver.as_typesystem(module, self.env);
        let fin = Finalizer::new(system, self.forall, self.lforalls, self.implicits);
        let (value, errors) = and_then(fin);
        self.errors.extend(errors.into_iter().map(FinError::TS));
        value
    }

    fn fin_typing(&mut self, info: &InstInfo) -> (ConcreteInst, CallTypes) {
        let inst = self.fin_inst(&info.inst);
        self.finalizer(|mut fin| {
            let ptypes = info.ptypes.iter().map(|ty| fin.apply(&**ty)).collect();
            let returns = fin.apply(&info.ret);
            let call = CallTypes::new(ptypes, returns);
            ((inst, call), fin.errors)
        })
    }

    fn fin_inst(&mut self, info: &ForeignInst<Var>) -> ConcreteInst {
        self.finalizer(|mut fin| {
            (
                ConcreteInst {
                    generics: info.generics.values().map(|var| fin.var(*var)).collect(),
                    pgenerics: info.pgenerics.values().map(|var| fin.var(*var)).collect(),
                    self_: info.self_.map(|var| fin.var(var)),
                },
                fin.errors,
            )
        })
    }

    pub fn lower_func_typing(&mut self, typing: &hir::Typing<IType>) -> ConcreteTyping {
        self.current.lambda = None;
        self.typing(None, typing)
    }

    pub fn lower_lambda_typings<'t>(
        &mut self,
        lambdas: impl Iterator<Item = (key::Lambda, &'t hir::Typing<IType>)>,
    ) {
        lambdas.for_each(|(lkey, lambda)| {
            self.current.lambda = Some(lkey);
            let typing = self.typing(Some(lkey), lambda);
            info!("lambda {lkey} typing finalised to: {typing}");
            self.lambdas
                .push_as(lkey, Lambda::new(typing, Expr::Poison))
        });
    }

    pub fn lower_lambda_expressions(
        mut self,
        exprs: &Map<key::Lambda, Tr<hir::Expr<'s>>>,
        params: &Map<key::Lambda, Vec<Tr<hir::Pattern<'s>>>>,
    ) -> (Map<key::Lambda, Lambda>, Vec<FinError<'s>>) {
        for lkey in exprs.keys() {
            self.current.lambda = Some(lkey);

            let typing = &self.lambdas[lkey].typing;
            let ptypes = typing.params.clone();

            self.lambdas[lkey].expr =
                self.patterns_and_expr(&ptypes, &params[lkey], exprs[lkey].as_ref());

            info!(
                "lambda {lkey} expr finalised to:\n  {}",
                &self.lambdas[lkey].expr
            );
        }

        (self.lambdas, self.errors)
    }

    fn typing(
        &mut self,
        lambda: Option<key::Lambda>,
        typing: &hir::Typing<IType>,
    ) -> ConcreteTyping {
        assert_eq!(lambda, self.current.lambda);

        self.finalizer(|mut fin| {
            (
                ConcreteTyping {
                    params: typing.params.values().map(|ty| fin.apply(&*ty)).collect(),
                    returns: fin.apply(&typing.returns),
                    forall: {
                        let cons = fin.forall(lambda).1.clone();
                        fin.lower_constraints_of(&cons, |_| "_")
                    },
                },
                fin.errors,
            )
        })
    }

    fn str_to_ro(&mut self, str: &'s str) -> M<key::ReadOnly> {
        let mut buffer = Vec::with_capacity(str.len());
        let mut bytes = str.bytes().enumerate();

        loop {
            let Some((_, mut b)) = bytes.next() else {
                break;
            };

            match b {
                b'\\' => match bytes.next().map(|(_, b)| b) {
                    Some(b'n') => b = b'\n',
                    Some(b'r') => b = b'\r',
                    Some(b't') => b = b'\t',
                    Some(b'\\') => b = b'\\',
                    Some(b'"') => b = b'"',
                    Some(b'0') => b = b'\0',
                    b => {
                        panic!("ET: invalid escape sequence: {b:?}");
                    }
                },
                _ => {}
            }

            buffer.push(b);
        }

        let ty = Type::defined(self.items.pinfo.string, vec![]);

        self.read_only_table.push(
            self.current.fkey.module,
            (mir::ReadOnlyBytes(buffer.into_boxed_slice()), ty),
        )
    }

    pub fn patterns_and_expr(
        &mut self,
        params: &[Type],
        patterns: &[Tr<hir::Pattern<'s>>],
        expr: Tr<&hir::Expr<'s>>,
    ) -> Expr {
        assert_eq!(params.len(), patterns.len());

        // if params.is_empty() {
        //     return self.lower_expr(expr);
        // }

        let mut plower = ParamsLower {
            lower: self,
            params,
            patterns,
            tail: expr,
            current_param: 0,
            lowered_tail: None,
        };
        let _ = pat::Merge::generate_tail(&mut plower);

        plower.lowered_tail.unwrap()
    }
}

pub struct ParamsLower<'l, 'a, 's> {
    lower: &'l mut Lower<'a, 's>,
    params: &'l [Type],
    patterns: &'l [Tr<hir::Pattern<'s>>],
    current_param: usize,

    tail: Tr<&'l hir::Expr<'s>>,
    lowered_tail: Option<Expr>,
}

impl<'l, 'a, 's> pat::Merge<'s, key::DecisionTreeTail> for ParamsLower<'l, 'a, 's> {
    fn generate_tail(&mut self) -> key::DecisionTreeTail {
        match self.params.get(self.current_param) {
            None => {
                if self.lowered_tail.is_none() {
                    self.lowered_tail = Some(self.lower.lower_expr(self.tail));
                }

                key::DecisionTreeTail(0)
            }
            Some(ty) => {
                let i = self.current_param;
                self.current_param += 1;

                let tree = self.first(ty, self.patterns[i].as_ref());

                let missing = pat::MissingGeneration::new(pat::Init::new(
                    self.lower.rsolver.ftypes,
                    self.lower.vtypes,
                ))
                .run(&tree);

                if !missing.is_empty() {
                    self.lower
                        .errors
                        .push(FinError::MissingPatterns(self.patterns[i].span, missing));
                }

                let on = Expr::Yield(mir::func::Local::Param(key::Param(i as u32)));

                let mut tails = Map::new();
                tails.push(self.lowered_tail.take().unwrap());

                self.lowered_tail = Some(Expr::Match(Box::new(on), tree, tails));

                key::DecisionTreeTail(0)
            }
        }
    }

    fn name_of_field(&self, record: M<key::Record>, field: key::RecordField) -> &'s str {
        *self.lower.rsolver.fnames[record][field]
    }

    fn to_init(&self) -> pat::Init {
        pat::Init::new(self.lower.rsolver.ftypes, self.lower.vtypes)
    }
}

#[derive(new)]
pub struct MatchBranchLower<'l, 'a, 's> {
    lower: &'l mut Lower<'a, 's>,
    tail: Tr<&'l hir::Expr<'s>>,
    tail_key: key::DecisionTreeTail,
    #[new(default)]
    lowered_tail: Option<Expr>,
}

impl<'l, 'a, 's> pat::Merge<'s, key::DecisionTreeTail> for MatchBranchLower<'l, 'a, 's> {
    fn generate_tail(&mut self) -> key::DecisionTreeTail {
        if self.lowered_tail.is_none() {
            self.lowered_tail = Some(self.lower.lower_expr(self.tail));
        }

        self.tail_key
    }

    fn name_of_field(&self, record: M<key::Record>, field: key::RecordField) -> &'s str {
        *self.lower.rsolver.fnames[record][field]
    }

    fn to_init(&self) -> pat::Init {
        pat::Init::new(self.lower.rsolver.ftypes, self.lower.vtypes)
    }
}

pub fn emit_fin_error<'s, T>(
    sources: &ast::Sources,
    module: key::Module,
    tfmt: TyFmtState<'_, 's>,
    records: &ModMap<key::Record, (Tr<&'s str>, T)>,
    error: FinError<'s>,
) {
    match error {
        FinError::TS(lumina_typesystem::FinError::FieldNotFound(key, field)) => sources
            .error("field not found")
            .m(module)
            .eline(
                field.span,
                format!(
                    "type {} does not have a field named `{field}`",
                    &records[key].0
                ),
            )
            .emit(),
        FinError::TS(lumina_typesystem::FinError::Record(rerror)) => {
            emit_record_error(sources, module, records, rerror)
        }
        FinError::UnreachablePattern(span) => sources
            .error("unreachable pattern")
            .m(module)
            .eline(span, "")
            .emit(),
        FinError::MissingPatterns(span, missing) => {
            let init = sources
                .error("patterns not exhaustive")
                .m(module)
                .eline(span, "")
                .text("missing patterns");

            let name_of_var = |sum: M<key::Sum>, var: key::SumVariant| {
                tfmt.hir.vnames[sum][var].value.to_string()
            };
            let name_of_field = |record: M<key::Record>, field: key::RecordField| {
                tfmt.hir.fnames[record][field].value.to_string()
            };

            missing
                .into_iter()
                .fold(init, |err, pat| {
                    err.text(format!("  {}", pat.fmt(&name_of_var, &name_of_field)))
                })
                .emit();
        }
        FinError::InvalidCast(from, to) => {
            sources
                .error("invalid cast")
                .m(module)
                .eline(
                    from.span,
                    format!(
                        "cannot cast a value of type `{}` to `{}`",
                        tfmt.clone().fmt(&*from),
                        tfmt.fmt(to)
                    ),
                )
                .emit();
        }
        FinError::TS(lumina_typesystem::FinError::NotRecord(span, ty, fields)) => sources
            .error("type mismatch")
            .m(module)
            .eline(
                span,
                format!(
                    "expected a record with the fields {{{}}}",
                    fields.iter().format(", ")
                ),
            )
            .text(format!("but got `{}`", tfmt.fmt(ty)))
            .emit(),
    }
}

impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} {}\n  {}",
            "as".keyword(),
            &self.typing,
            '='.symbol(),
            self.expr.to_string().lines().format("\n  "),
        )
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} {}\n  {}",
            "as".keyword(),
            &self.typing,
            '='.symbol(),
            self.expr.to_string().lines().format("\n  ")
        )?;

        if !self.lambdas.is_empty() {
            write!(
                f,
                "\n {}\n  {}",
                "where".keyword(),
                self.lambdas
                    .iter()
                    .map(|(k, v)| {
                        format!("{} {k} {}", "fn".keyword(), v)
                            .to_string()
                            .lines()
                            .format("\n  ")
                            .to_string()
                    })
                    .format("\n  "),
            )
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for ConcreteTyping {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.forall.is_empty() {
            write!(
                f,
                "{} {} . ",
                '∀'.symbol(),
                self.forall
                    .iter()
                    .map(|(g, gdata)| {
                        if gdata.trait_constraints.is_empty() {
                            g.to_string()
                        } else {
                            format!(
                                "({g} {} {})",
                                "can".keyword(),
                                gdata
                                    .trait_constraints
                                    .iter()
                                    .map(|con| ParamFmt::new(&con.trait_, &con.params))
                                    .format(", ")
                            )
                        }
                    })
                    .format(" ")
            )?;
        }
        write!(
            f,
            "{}{} {} {}{}",
            '('.symbol(),
            self.params.iter().format(", "),
            "->".symbol(),
            &self.returns,
            ')'.symbol(),
        )
    }
}
