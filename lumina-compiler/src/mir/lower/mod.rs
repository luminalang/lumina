use super::func::InstInfo;
use super::tyfmt::TyFmtState;
use super::{Current, LangItems, ReadOnlyBytes};
use crate::prelude::*;
use crate::Target;
use ast::NFunc;
use std::ops::Not;

mod expr;
pub use expr::Expr;
pub mod pat;

use derive_new::new;
use lumina_typesystem::{
    Constraint, Forall, GenericMapper, IType, Inference, Static, TEnv, Transformer, Type, Upgrade,
    Var,
};
use lumina_util::Highlighting;
use std::fmt;

#[derive(new)]
pub struct Function {
    pub typing: ConcreteTyping,
    pub lambdas: Map<key::Lambda, Lambda>,
    pub lcaptures: Map<key::Lambda, Vec<key::Bind>>,
    #[new(default)]
    pub no_mangle: bool,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct ConcreteTyping {
    pub forall: Forall<'static, Static>,
    pub params: Vec<Type>,
    pub returns: Type,
}

impl ConcreteTyping {
    pub fn from_hir<T>(
        forall: Forall<'static, Static>,
        hir: &hir::Typing<T>,
        mut f: impl FnMut(Tr<&T>) -> Type,
    ) -> Self {
        Self {
            forall,
            params: hir.params.values().map(|ty| f(ty.as_ref())).collect(),
            returns: f(hir.returns.as_ref()),
        }
    }
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

#[derive(Clone, Debug)]
pub enum Callable {
    Func(M<NFunc>, GenericMapper<Static>),
    Lambda(key::Lambda, GenericMapper<Static>),
    Binding(key::Bind),
    Param(key::Param),
}

#[derive(new)]
pub struct Lower<'a, 's> {
    env: &'a TEnv<'s>,

    read_only_table: &'a mut MMap<key::ReadOnly, (ReadOnlyBytes, Type)>,

    pub current: &'a mut Current,

    items: LangItems,
    fnames: &'a MMap<key::Record, Map<key::Field, Tr<&'s str>>>,
    ftypes: &'a MMap<key::Record, Map<key::Field, Tr<Type>>>,
    vtypes: &'a MMap<key::Sum, Map<key::Variant, Vec<Tr<Type>>>>,

    target: Target,

    #[new(default)]
    pub errors: Vec<FinError>,
    #[new(default)]
    pub trait_object_cast_checks: Vec<(Tr<Type>, Constraint<Static>)>,
    // #[new(default)]
    // lambdas: Map<key::Lambda, Lambda>,
}

pub enum FinError {
    UnreachablePattern(Span),
    MissingPatterns(Span, Vec<pat::MissingPattern>),
    InvalidCast(Tr<Type>, Type),
    LargeCharLiteral(Span),
    BadArrayCount { got: Tr<usize>, exp: Tr<u64> },
    BadGenericArrayCount { got: Tr<usize> },
    DuplicateField(Tr<String>, Span),
    MissingField(String, Span),
}

impl<'a, 's> Lower<'a, 's> {
    pub(super) fn finalizer(&mut self) -> Upgrade {
        Upgrade(self.env)
    }

    fn fin_typing(&mut self, info: &InstInfo) -> (GenericMapper<Static>, CallTypes) {
        let inst = self.fin_inst(&info.inst);
        let mut fin = self.finalizer();
        let ptypes = info.ptypes.iter().map(|ty| fin.transform(&**ty)).collect();
        let returns = fin.transform(&info.ret);
        let call = CallTypes::new(ptypes, returns);
        (inst, call)
    }

    fn fin_inst(&mut self, info: &GenericMapper<Inference>) -> GenericMapper<Static> {
        info.map(|ty| self.finalizer().transform(ty))
    }

    pub fn typing(
        &mut self,
        forall: &Forall<'s, Inference>,
        typing: &hir::Typing<IType>,
    ) -> ConcreteTyping {
        let mut fin = self.finalizer();
        ConcreteTyping {
            params: typing
                .params
                .values()
                .map(|ty| fin.transform(&*ty))
                .collect(),
            returns: fin.transform(&typing.returns),
            forall: forall.map(|_, ty| fin.transform(ty), Forall::<()>::name_by_key),
        }
    }

    fn escape(&self, str: &'s str) -> Vec<u8> {
        let mut buffer = Vec::with_capacity(str.len());
        let mut bytes = str.bytes();

        loop {
            let Some(mut b) = bytes.next() else {
                break buffer;
            };

            match b {
                b'\\' => match bytes.next() {
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
    }

    fn str_to_ro(&mut self, str: &'s str) -> M<key::ReadOnly> {
        let str = self.escape(str);

        // We set the type to `u8` because when this data is accessed
        // with ReadOnly, it's treated by-reference so it'll become `*u8`
        let ty = Type::u8();

        self.read_only_table.push(
            self.current.fkey.0,
            (mir::ReadOnlyBytes(str.into_boxed_slice()), ty),
        )
    }

    pub fn patterns_and_expr(
        &mut self,
        params: &[Type],
        patterns: &[Tr<hir::Pattern<'s>>],
        expr: Tr<&hir::Expr<'s>>,
    ) -> Expr {
        assert_eq!(params.len(), patterns.len());

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

                let (string, maybe, list) = (
                    self.lower.items.pinfo.string,
                    self.lower.items.pinfo.maybe,
                    self.lower.items.list_default,
                );
                let tree = self.first(string, maybe, list, ty, self.patterns[i].as_ref());

                let missing = pat::MissingGeneration::new(pat::Init::new(
                    self.lower.ftypes,
                    self.lower.vtypes,
                ))
                .run(&tree);

                if !missing.is_empty() {
                    self.lower
                        .errors
                        .push(FinError::MissingPatterns(self.patterns[i].span, missing));
                }

                let on = Expr::Yield(Callable::Param(key::Param(i as u32)));

                let mut tails = Map::new();
                match self.lowered_tail.take() {
                    Some(tail) => {
                        tails.push(tail);
                        let pred = Map::from([1]);
                        self.lowered_tail = Some(Expr::Match(Box::new(on), tree, tails, pred));
                    }
                    None => {
                        self.lowered_tail = Some(Expr::Poison);
                    }
                }

                key::DecisionTreeTail(0)
            }
        }
    }

    fn type_of_bind(&mut self, bind: key::Bind) -> Type {
        let ty = self.lower.current.binds[&bind].value.clone();
        self.lower.finalizer().transform(&ty)
    }

    fn err_duplicate_field(&mut self, field: Tr<&'s str>, previous: Span) {
        self.lower.errors.push(FinError::DuplicateField(
            field.map(str::to_string),
            previous,
        ));
    }

    fn str_to_ro(&mut self, str: &'s str) -> M<lumina_key::ReadOnly> {
        self.lower.str_to_ro(str)
    }

    fn extractor_params(&mut self, params: &[Tr<hir::Expr<'s>>]) -> Vec<mir::Expr> {
        params
            .iter()
            .map(|expr| self.lower.lower_expr(expr.as_ref()))
            .collect()
    }

    fn fin_popped_inst(&mut self, span: Span) -> Option<(GenericMapper<Static>, CallTypes)> {
        self.lower
            .current
            .pop_inst(span)
            .map(|instinfo| self.lower.fin_typing(&instinfo))
    }

    fn fin_record(&mut self, rvar: Var) -> Option<(M<key::Record>, Vec<Type>)> {
        self.lower.transform_rvar(rvar)
    }

    fn name_of_field(&self, record: M<key::Record>, field: key::Field) -> &'s str {
        *self.lower.fnames[record][field]
    }

    fn to_init(&self) -> pat::Init {
        pat::Init::new(self.lower.ftypes, self.lower.vtypes)
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

    fn name_of_field(&self, record: M<key::Record>, field: key::Field) -> &'s str {
        *self.lower.fnames[record][field]
    }

    fn type_of_bind(&mut self, bind: key::Bind) -> Type {
        let ty = self.lower.current.binds[&bind].value.clone();
        self.lower.finalizer().transform(&ty)
    }

    fn err_duplicate_field(&mut self, field: Tr<&'s str>, previous: Span) {
        self.lower.errors.push(FinError::DuplicateField(
            field.map(str::to_string),
            previous,
        ));
    }

    fn str_to_ro(&mut self, str: &'s str) -> M<key::ReadOnly> {
        self.lower.str_to_ro(str)
    }

    fn fin_popped_inst(&mut self, span: Span) -> Option<(GenericMapper<Static>, CallTypes)> {
        self.lower
            .current
            .pop_inst(span)
            .map(|instinfo| self.lower.fin_typing(&instinfo))
    }

    fn fin_record(&mut self, rvar: Var) -> Option<(M<key::Record>, Vec<Type>)> {
        self.lower.transform_rvar(rvar)
    }

    fn extractor_params(&mut self, params: &[Tr<hir::Expr<'s>>]) -> Vec<mir::Expr> {
        params
            .iter()
            .map(|expr| self.lower.lower_expr(expr.as_ref()))
            .collect()
    }

    fn to_init(&self) -> pat::Init {
        pat::Init::new(self.lower.ftypes, self.lower.vtypes)
    }
}

pub fn emit_fin_error<'s>(
    sources: &ast::Sources,
    module: key::Module,
    tfmt: TyFmtState<'_, 's>,
    error: FinError,
) {
    match error {
        FinError::LargeCharLiteral(span) => sources
            .error("invalid char literal")
            .m(module)
            .eline(
                span.move_indice(1),
                "char literals may only be used to represent a single character",
            )
            .emit(),
        FinError::DuplicateField(field, previous) => sources
            .error("field assigned twice")
            .m(module)
            .iline(previous, "first assigned here")
            .iline(field.span, "later assigned again here")
            .emit(),
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

            let name_of_var =
                |sum: M<key::Sum>, var: key::Variant| tfmt.hir.vnames[sum][var].value.to_string();
            let name_of_field = |record: M<key::Record>, field: key::Field| {
                tfmt.hir.fnames[record][field].value.to_string()
            };

            missing
                .into_iter()
                .fold(init, |err, pat| {
                    err.text(format!("  {}", pat.fmt(&name_of_var, &name_of_field)))
                })
                .emit();
        }
        FinError::BadArrayCount { got, exp } => sources
            .error("incorrect array length")
            .m(module)
            .eline(got.span, format!("has {got} elements"))
            .iline(exp.span, format!("expected {exp}"))
            .emit(),
        FinError::BadGenericArrayCount { got } => sources
            .error("incorrect array length")
            .m(module)
            .eline(
                got.span,
                format!("arrays with generic length cannot be constructed with {got} elements",),
            )
            .emit(),
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
        // TODO: merge multiple
        FinError::MissingField(field, rspan) => sources
            .error("missing record field")
            .m(module)
            .eline(rspan, format!("field `{field}` is missing"))
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
        let constraints = self
            .typing
            .forall
            .generics
            .iter()
            .filter_map(|(generic, gdata)| {
                let cons = &gdata.trait_constraints;
                cons.is_empty().not().then(|| {
                    format!(
                        "{generic} {} {}",
                        "can".keyword(),
                        cons.iter().format(" + ")
                    )
                })
            })
            .collect::<Vec<_>>();

        let w = "when".keyword();

        match constraints.as_slice() {
            [] => Ok(()),
            [con] => writeln!(f, "{w} {con}"),
            many => writeln!(f, "{w}\n  {}", many.iter().format("\n  ")),
        }?;

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
        write!(
            f,
            "{}{}{} {} {}{}",
            &self.forall,
            '('.symbol(),
            self.params.iter().format(", "),
            "->".symbol(),
            &self.returns,
            ')'.symbol(),
        )
    }
}
