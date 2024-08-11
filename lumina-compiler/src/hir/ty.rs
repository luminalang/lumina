use crate::prelude::*;
use ast::Entity;
use lumina_parser as parser;
use lumina_parser::func::{Header as FuncHeader, Typing as ParserTyping};
use lumina_typesystem::{
    Constraint, Forall, Generic, GenericKind, IType, Inference, IntSize, Static, TEnv, Ty, Var,
};
use lumina_util::{Spanned, Tr};
use smallvec::SmallVec;

pub struct TypeLower<'t, 'a, 's> {
    module: key::Module,
    ast: &'a ast::AST<'s>,

    default_int_size: u8,

    pub type_info: &'t mut TypeEnvInfo<'s>,
}

pub struct TypeEnvInfo<'s> {
    pub(crate) iforalls: SmallVec<[(Forall<'s, Inference>, GenericKind); 2]>,
    pub(crate) cforalls: SmallVec<[(Forall<'s, Static>, GenericKind); 2]>,

    pub declare_generics: bool,
    pub list: M<key::TypeKind>,
    pub string: M<key::TypeKind>,
    pub self_handler: SelfHandler,
    inference: Option<TEnv<'s>>,
}

pub enum SelfHandler {
    Direct,
    Substituted(M<key::TypeKind>),
    Disallowed,
}

pub trait FromVar: Sized + Clone {
    fn var(var: Var) -> Ty<Self>;
}

impl FromVar for Inference {
    fn var(var: Var) -> Ty<Self> {
        Ty::infer(var)
    }
}

impl FromVar for Static {
    fn var(_: Var) -> Ty<Self> {
        unreachable!();
    }
}

impl<'s> TypeEnvInfo<'s> {
    pub fn new(declare_generics: bool, string: M<key::TypeKind>, list: M<key::TypeKind>) -> Self {
        Self {
            list,
            string,
            declare_generics,
            self_handler: SelfHandler::Disallowed,
            iforalls: SmallVec::new(),
            cforalls: SmallVec::new(),
            inference: None,
        }
    }

    pub fn lambda(&self) -> Option<key::Lambda> {
        match self.iforalls.last()?.1 {
            GenericKind::Lambda(key) => Some(key),
            _ => None,
        }
    }

    pub fn inference(mut self, vars: TEnv<'s>) -> Self {
        self.inference = Some(vars);
        self
    }

    pub fn take_inference(&mut self) -> Option<TEnv<'s>> {
        self.inference.take()
    }
    pub fn inference_mut(&mut self) -> Option<&mut TEnv<'s>> {
        self.inference.as_mut()
    }

    pub fn enter_lambda(&mut self, lkey: key::Lambda, lforall: Forall<'s, Inference>) {
        self.iforalls.push((lforall, GenericKind::Lambda(lkey)));
    }

    pub fn enter_type_or_impl_or_method(&mut self, forall: Forall<'s, Static>, kind: GenericKind) {
        self.cforalls.push((forall, kind));
    }
    pub fn leave_type_or_impl_or_method(&mut self) -> Forall<'s, Static> {
        self.cforalls.pop().unwrap().0
    }

    pub fn enter_function(&mut self, forall: Forall<'s, Inference>) {
        self.iforalls.push((forall, GenericKind::Entity));
    }
    pub fn leave_function(&mut self) -> Forall<'s, Inference> {
        self.iforalls.pop().unwrap().0
    }

    pub fn declare_generic(&mut self, name: &'s str, plen: usize) -> Option<Generic> {
        match self.iforalls.last_mut() {
            Some((forall, kind)) => {
                let gkey = forall.push(name);
                let generic = Generic::new(gkey, *kind);
                forall[generic.key].set_params(plen);
                return Some(generic);
            }
            None => {
                if let Some((forall, kind)) = self.cforalls.last_mut() {
                    let gkey = forall.push(name);
                    let generic = Generic::new(gkey, *kind);
                    forall[generic.key].set_params(plen);
                    return Some(generic);
                }
            }
        }

        None
    }
}

impl<'t, 'a, 's> TypeLower<'t, 'a, 's> {
    pub fn new(
        module: key::Module,
        ast: &'a ast::AST<'s>,
        default_int_size: u8,
        type_info: &'t mut TypeEnvInfo<'s>,
    ) -> Self {
        Self { module, ast, type_info, default_int_size }
    }

    pub fn typing<T: FromVar>(&mut self, typing: &ParserTyping<'s>) -> hir::Typing<Ty<T>> {
        hir::Typing {
            params: self.tys_spanned(&typing.ptypes),
            returns: self.ty_spanned(typing.returns.as_ref()),
        }
    }

    pub fn typing_or_emit_and_poison(
        &mut self,
        header: &FuncHeader<'s>,
        kind: &str,
    ) -> hir::Typing<Ty<Static>> {
        match header.typing.as_ref() {
            Some(typing) => self.typing(typing),
            None => {
                self.ast
                    .sources
                    .error("missing function type signature")
                    .m(self.module)
                    .eline(
                        header.name.span,
                        format!("type signatures for {kind} are mandatory"),
                    )
                    .emit();

                hir::Typing::poisoned(&header.params, header.name.span)
            }
        }
    }

    pub fn typing_or_inferred<T>(&mut self, header: &FuncHeader<'s>) -> hir::Typing<Ty<T>>
    where
        T: FromVar,
    {
        match header.typing.as_ref() {
            Some(typing) => self.typing(typing),
            None => {
                let vars = self
                    .type_info
                    .inference_mut()
                    .expect("optional inference for static signature");
                hir::Typing::inferred(&header.params, header.name.span, vars)
            }
        }
    }

    pub fn ty_spanned<T: FromVar>(&mut self, ty: Tr<&parser::Type<'s>>) -> Tr<Ty<T>> {
        self.ty::<T>(ty).tr(ty.span)
    }

    pub fn ty<T: FromVar>(&mut self, ty: Tr<&parser::Type<'s>>) -> Ty<T> {
        trace!("lowering type {ty}");

        match *ty {
            parser::Type::Closure(ptypes, returns) => {
                let ptypes = self.tys(ptypes);
                let returns = self.ty((**returns).as_ref());
                Ty::closure(ptypes, returns)
            }
            parser::Type::FnPointer(ptypes, returns) => {
                let ptypes = self.tys(ptypes);
                let returns = self.ty((**returns).as_ref());
                Ty::fn_pointer(ptypes, returns)
            }
            parser::Type::Pointer(inner) => {
                let inner = self.ty((**inner).as_ref());
                Ty::pointer(inner)
            }
            parser::Type::Tuple(elems) => {
                let elems = self.tys(elems);
                Ty::tuple(elems)
            }
            parser::Type::List(inner) => {
                if inner.len() != 1 {
                    self.ast
                        .sources
                        .error("invalid list type")
                        .m(self.module)
                        .eline(ty.span, "a list's elements must all be of the same type")
                        .emit();

                    Ty::poison()
                } else {
                    let params = self.tys(inner);
                    Ty::list(self.type_info.list, params)
                }
            }
            parser::Type::Poison => Ty::poison(),
            parser::Type::Defined(path, params) => self.defined(path.tr(ty.span), params),
        }
    }

    pub fn tys<T: FromVar>(&mut self, ty: &[Tr<parser::Type<'s>>]) -> Vec<Ty<T>> {
        ty.iter().map(|ty| self.ty(ty.as_ref())).collect()
    }

    pub fn tys_spanned<F, T: FromVar>(&mut self, ty: &[Tr<parser::Type<'s>>]) -> F
    where
        F: FromIterator<Tr<Ty<T>>>,
    {
        ty.iter().map(|ty| self.ty_spanned(ty.as_ref())).collect()
    }

    fn defined<T: FromVar>(
        &mut self,
        apath: Tr<&parser::AnnotatedPath<'s>>,
        params: &[Tr<parser::Type<'s>>],
    ) -> Ty<T> {
        if !apath.for_segments.is_empty() {
            self.ast.sources
                .warning("invalid type annotation")
                .m(self.module)
                .eline(apath.span, "this annotation doesn't do anything")
                .text("if you're attempting to annotate an associated type of a trait constraint, they are very experimental")
                .emit();
        }

        let span = apath.span;
        let path = apath.path.as_slice();

        match path {
            ["_"] => match self.type_info.inference.as_mut() {
                None => {
                    self.ast
                        .sources
                        .error("invalid type")
                        .m(self.module)
                        .eline(span, "type inference not allowed in this context")
                        .emit();

                    return Ty::poison();
                }
                Some(vars) => {
                    let ty = T::var(vars.var(span));
                    return self.forbid_params(span, ty, params);
                }
            },
            ["bool"] => {
                return self.forbid_params(span, Ty::bool(), params);
            }
            ["int"] => {
                let int = Ty::Int(IntSize::new(true, self.default_int_size));
                return self.forbid_params(span, int, params);
            }
            ["uint"] => {
                let int = Ty::Int(IntSize::new(false, self.default_int_size));
                return self.forbid_params(span, int, params);
            }
            ["float"] => {
                return self.forbid_params(span, Ty::f64(), params);
            }
            ["self"] => match self.type_info.self_handler {
                SelfHandler::Substituted(kind) => {
                    let (forall, gkind) = &self.type_info.cforalls.last().unwrap();
                    assert_eq!(*gkind, GenericKind::Entity);
                    let params = forall.to_types(*gkind);
                    return Ty::defined(kind, params);
                }
                SelfHandler::Direct => return Ty::Simple("self"),
                SelfHandler::Disallowed => {
                    self.ast
                        .sources
                        .error("invalid type")
                        .m(self.module)
                        .eline(
                            span,
                            "`self` is only allowed in type and implementation declarations",
                        )
                        .emit();

                    return Ty::poison();
                }
            },
            ["string"] => {
                let string = self.type_info.string;
                return self.forbid_params(span, Ty::string(string, vec![]), params);
            }
            [name] if name.starts_with('u') => {
                if let Ok(n) = name[1..].parse::<u8>() {
                    let int = Ty::Int(IntSize::new(false, n));
                    return self.forbid_params(span, int, params);
                }
            }
            [name] if name.starts_with('i') => {
                if let Ok(n) = name[1..].parse::<u8>() {
                    let int = Ty::Int(IntSize::new(true, n));
                    return self.forbid_params(span, int, params);
                }
            }
            [name] => match self.try_generic(name, params.len()) {
                Ok(gen) => {
                    return self.forbid_params(span, Ty::Generic(gen), params);
                }
                Err(GenericError::InconsistentHKT(exp)) => {
                    self.emit_inconsistent_hkt(span, exp, params.len());
                    return Ty::poison();
                }
                Err(GenericError::NotFound) => {}
            },
            _ => {}
        }

        match self.ast.lookups.resolve_type(self.module, path) {
            Ok(entity) => match entity.key {
                Entity::Type(tkey) => {
                    let key = entity.module.m(tkey);
                    let mut tparams = self.tys(params);
                    let expected_type_params =
                        &self.ast.entities.header_of_ty(key).header.type_params;

                    let err = |span, msg| {
                        self.ast
                            .sources
                            .error("invalid type")
                            .m(self.module)
                            .eline(span, msg)
                            .emit()
                    };

                    let header = || &self.ast.entities.header_of_ty(key).header;

                    match (params.len(), expected_type_params.len()) {
                        (x, y) if x < y => {
                            err(
                                span,
                                format!("missing {} type parameters for `{}`", y - x, header()),
                            );

                            while tparams.len() != expected_type_params.len() {
                                tparams.push(Ty::poison());
                            }
                        }
                        (x, y) if x > y => {
                            err(
                                params[params.len() - 1].span,
                                format!("excess parameter for {}", header()),
                            );

                            tparams.truncate(expected_type_params.len());
                        }
                        (_, _) => {}
                    }

                    Ty::defined(key, tparams)
                }
                _ => {
                    let name = path.last().unwrap();
                    self.ast
                        .sources
                        .emit_wrong_entity(self.module, span, name, "type", entity.key);

                    Ty::poison()
                }
            },

            Err(err) => {
                self.ast
                    .sources
                    .emit_lookup_err(span, self.module, "type", err);

                Ty::poison()
            }
        }
    }

    fn forbid_params<T: FromVar>(
        &self,
        span: Span,
        ty: Ty<T>,
        params: &[Tr<parser::Type<'s>>],
    ) -> Ty<T> {
        if params.len() == 0 {
            ty
        } else {
            self.ast
                .sources
                .error("invalid type")
                .m(self.module)
                .eline(span, "this type can not take any type parameters")
                .emit();

            Ty::poison()
        }
    }

    fn emit_inconsistent_hkt(&mut self, span: Span, exp: usize, got: usize) {
        self.ast
            .sources
            .error("invalid type")
            .m(self.module)
            .eline(
                span,
                format!("generic expects {exp} parameters but was given {}", got,),
            )
            .emit();
    }

    fn try_generic(&mut self, name: &'s str, plen: usize) -> Result<Generic, GenericError> {
        for (forall, kind) in self.type_info.iforalls.iter_mut().rev() {
            if let Some(generic) = forall.find(name) {
                check_param_len(&mut forall[generic].params, plen)
                    .map_err(GenericError::InconsistentHKT)?;
                return Ok(Generic::new(generic, *kind));
            }
        }

        for (forall, kind) in self.type_info.cforalls.iter_mut().rev() {
            if let Some(generic) = forall.find(name) {
                check_param_len(&mut forall[generic].params, plen)
                    .map_err(GenericError::InconsistentHKT)?;
                return Ok(Generic::new(generic, *kind));
            }
        }

        // Implicitly declare if able
        if name.len() == 1 && self.type_info.declare_generics {
            if let Some(generic) = self.type_info.declare_generic(name, plen) {
                return Ok(generic);
            }
        }

        Err(GenericError::NotFound)
    }

    pub fn add_constraint<T: FromVar>(
        &mut self,
        generic: Generic,
        ty: Tr<&lumina_parser::Type<'s>>,
    ) -> Result<(), NotATrait> {
        match self.type_info.iforalls.last_mut() {
            Some((..)) => {
                let (trait_, params) =
                    self.ty::<Inference>(ty).as_trait().map_err(|_| NotATrait)?;

                let (forall, kind) = self.type_info.iforalls.last_mut().unwrap();
                assert_eq!(generic.kind, *kind);

                forall[generic.key].trait_constraints.push(Constraint {
                    span: ty.span,
                    trait_,
                    params,
                });
            }
            None => {
                let (trait_, params) = self.ty::<Static>(ty).as_trait().map_err(|_| NotATrait)?;
                let (forall, kind) = self.type_info.cforalls.last_mut().unwrap();
                assert_eq!(generic.kind, *kind);

                forall[generic.key].trait_constraints.push(Constraint {
                    span: ty.span,
                    trait_,
                    params,
                });
            }
        }

        Ok(())
    }
}

fn check_param_len(exp: &mut usize, got: usize) -> Result<(), usize> {
    if *exp == usize::MAX {
        *exp = got;
        Ok(())
    } else {
        (got == *exp).then_some(()).ok_or(*exp)
    }
}

pub struct NotATrait;

enum GenericError {
    NotFound,
    InconsistentHKT(usize),
}

#[derive(Clone)]
pub struct TypeAnnotation<'s> {
    pub for_entity: Vec<(Tr<&'s str>, IType)>,
    pub for_type: Vec<(Tr<&'s str>, IType)>,
}

impl<'s> TypeAnnotation<'s> {
    pub fn new() -> Self {
        Self { for_type: vec![], for_entity: vec![] }
    }
}
