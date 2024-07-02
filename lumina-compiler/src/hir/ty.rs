use crate::prelude::*;
use ast::Entity;
use lumina_parser as parser;
use lumina_typesystem::{
    Bitsize, Constraint, Container, Forall, FuncKind, Generic, GenericData, GenericKind, IType,
    Prim, TEnv, Type, Var,
};
use lumina_util::{Spanned, Tr};
use smallvec::SmallVec;

pub struct TypeLower<'t, 'a, 's> {
    module: key::Module,
    ast: &'a ast::AST<'s>,

    pub type_info: &'t mut TypeEnvInfo<'s>,
}

pub struct TypeEnvInfo<'s> {
    pub(crate) iforalls: SmallVec<[(Forall<'s, IType>, GenericKind); 2]>,
    pub(crate) cforalls: SmallVec<[(Forall<'s, Type>, GenericKind); 2]>,

    pub declare_generics: bool,
    pub list: Option<M<key::TypeKind>>,
    pub allow_self: bool,
    pub self_params: usize,
    inference: Option<TEnv<'s>>,
}

impl<'s> TypeEnvInfo<'s> {
    pub fn new(declare_generics: bool) -> Self {
        Self {
            list: None,
            declare_generics,
            allow_self: false,
            iforalls: SmallVec::new(),
            cforalls: SmallVec::new(),
            self_params: usize::MAX,
            inference: None,
        }
    }

    pub fn list(mut self, list: Option<M<key::TypeKind>>) -> Self {
        self.list = list;
        self
    }

    pub fn lambda(&self) -> Option<key::Lambda> {
        match self.iforalls.last()?.1 {
            GenericKind::Lambda(key) => Some(key),
            _ => None,
        }
    }

    pub fn declare(&mut self) {
        todo!();
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

    pub fn enter_lambda(&mut self, lkey: key::Lambda, lforall: Forall<'s, IType>) {
        self.iforalls.push((lforall, GenericKind::Lambda(lkey)));
    }

    pub fn enter_type_or_impl_or_method(&mut self, forall: Forall<'s, Type>, kind: GenericKind) {
        self.cforalls.push((forall, kind));
    }
    pub fn leave_type_or_impl_or_method(&mut self) -> Forall<'s, Type> {
        self.cforalls.pop().unwrap().0
    }

    pub fn enter_function(&mut self, forall: Forall<'s, IType>) {
        self.iforalls.push((forall, GenericKind::Entity));
    }
    pub fn leave_function(&mut self) -> Forall<'s, IType> {
        self.iforalls.pop().unwrap().0
    }

    pub fn declare_generic(&mut self, name: &'s str, plen: usize) -> Option<Generic> {
        match self.iforalls.last_mut() {
            Some((forall, kind)) => {
                let gkey = forall.push(GenericData::new(name));
                let generic = Generic::new(gkey, *kind);
                forall[generic.key].set_params(plen);
                return Some(generic);
            }
            None => {
                if let Some((forall, kind)) = self.cforalls.last_mut() {
                    let gkey = forall.push(GenericData::new(name));
                    let generic = Generic::new(gkey, *kind);
                    forall[generic.key].set_params(plen);
                    return Some(generic);
                }
            }
        }

        None
    }
}

pub trait Ty: Clone + From<Prim> {
    fn var(_: Var) -> Self {
        panic!("invalid var");
    }
    fn defined(key: M<key::TypeKind>, params: Vec<Self>) -> Self;
    fn container(con: Container<Self>) -> Self;
    fn generic(generic: Generic) -> Self;
    fn as_trait(self) -> Result<(M<key::Trait>, Vec<Self>), Self>;
    fn primary_generics<'v, 't, 'a, 's>(
        this: &'v mut TypeEnvInfo<'s>,
    ) -> Option<&'v mut (Forall<'s, Self>, GenericKind)>;
    fn self_() -> Self;
}

#[rustfmt::skip]
impl Ty for IType {
    fn var(var: Var) -> Self { IType::Var(var) }
    fn defined(key: M<key::TypeKind>, params: Vec<Self>) -> Self { IType::Defined(key, params) }
    fn container(con: Container<Self>) -> Self { IType::Container(con) }
    fn generic(generic: Generic) -> Self { IType::Generic(generic) }
    fn as_trait(self) -> Result<(M<key::Trait>, Vec<Self>), Self> {
        match self {
            IType::Defined(M { value: key::TypeKind::Trait(key) , module}, params) => Ok((module.m(key), params)),
            other => Err(other),
        }
    }
    fn primary_generics<'v, 't, 'a, 's>(
        this: &'v mut TypeEnvInfo<'s>,
    ) -> Option<&'v mut (Forall<'s, Self>, GenericKind)> {
        this.iforalls.last_mut()
    }
    fn self_() -> Self { Self::Self_ }
}

#[rustfmt::skip]
impl Ty for Type {
    fn container(con: Container<Self>) -> Self { Type::Container(con) }
    fn defined(key: M<key::TypeKind>, params: Vec<Self>) -> Self { Type::Defined(key, params) }
    fn generic(generic: Generic) -> Self { Type::Generic(generic) }
    fn as_trait(self) -> Result<(M<key::Trait>, Vec<Self>), Self> {
        match self {
            Type::List(M { module, value: key::TypeKind::Trait(key) }, params)
            | Type::Defined(M { module, value: key::TypeKind::Trait(key) }, params) => Ok((module.m(key), params)),
            other => Err(other),
        }
    }
    fn primary_generics<'v, 't, 'a, 's>(
        this: &'v mut TypeEnvInfo<'s>,
    ) -> Option<&'v mut (Forall<'s, Self>, GenericKind)> {
        this.cforalls.last_mut()
    }
    fn self_() -> Self { Self::Self_ }
}

impl<'t, 'a, 's> TypeLower<'t, 'a, 's> {
    pub fn new(
        module: key::Module,
        ast: &'a ast::AST<'s>,
        type_info: &'t mut TypeEnvInfo<'s>,
    ) -> Self {
        Self { module, ast, type_info }
    }

    pub fn ty_spanned<T: Ty>(&mut self, ty: Tr<&parser::Type<'s>>) -> Tr<T> {
        self.ty::<T>(ty).tr(ty.span)
    }

    pub fn ty<T: Ty>(&mut self, ty: Tr<&parser::Type<'s>>) -> T {
        trace!("lowering type {ty}");

        match *ty {
            parser::Type::Closure(ptypes, returns) => {
                let ptypes = self.tys(ptypes);
                let returns = self.ty((**returns).as_ref());
                let con = Container::Func(FuncKind::Closure, ptypes, Box::new(returns));
                T::container(con)
            }
            parser::Type::FnPointer(ptypes, returns) => {
                let ptypes = self.tys(ptypes);
                let returns = self.ty((**returns).as_ref());
                let con = Container::Func(FuncKind::FnPointer, ptypes, Box::new(returns));
                T::container(con)
            }
            parser::Type::Pointer(inner) => {
                let con = Container::Pointer(Box::new(self.ty((**inner).as_ref())));
                T::container(con)
            }
            parser::Type::Tuple(elems) => T::container(Container::Tuple(self.tys(elems))),
            parser::Type::List(inner) => {
                if inner.len() != 1 {
                    self.ast
                        .sources
                        .error("invalid list type")
                        .m(self.module)
                        .eline(ty.span, "a list's elements must all be of the same type")
                        .emit();

                    Prim::Poison.into()
                } else {
                    match self.type_info.list.clone() {
                        None => {
                            self.ast
                                .sources
                                .error("missing lang item")
                                .m(self.module)
                                .eline(ty.span, "no list type")
                                .emit();

                            Prim::Poison.into()
                        }
                        Some(constr) => {
                            let params = self.tys(inner);
                            T::defined(constr, params)
                        }
                    }
                }
            }
            parser::Type::Poison => T::from(Prim::Poison),
            parser::Type::Defined(path, params) => self.defined(path.tr(ty.span), params),
        }
    }

    pub fn tys<T: Ty>(&mut self, ty: &[Tr<parser::Type<'s>>]) -> Vec<T> {
        ty.iter().map(|ty| self.ty(ty.as_ref())).collect()
    }

    pub fn tys_spanned<T: Ty>(&mut self, ty: &[Tr<parser::Type<'s>>]) -> Vec<Tr<T>> {
        ty.iter().map(|ty| self.ty_spanned(ty.as_ref())).collect()
    }

    fn defined<T: Ty>(
        &mut self,
        apath: Tr<&parser::AnnotatedPath<'s>>,
        params: &[Tr<parser::Type<'s>>],
    ) -> T {
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
        let lambda = self.type_info.lambda();

        match path {
            ["_"] => match self.type_info.inference.as_mut() {
                None => {
                    self.ast
                        .sources
                        .error("invalid type")
                        .m(self.module)
                        .eline(span, "type inference not allowed in this context")
                        .emit();

                    return T::from(Prim::Poison);
                }
                Some(vars) => {
                    let ty = T::var(vars.var(span, lambda));
                    return self.forbid_params(span, ty, params);
                }
            },
            ["bool"] => {
                return self.forbid_params(span, T::from(Prim::Bool), params);
            }
            ["int"] => {
                let int = T::from(Prim::Int(true, Bitsize::default()));
                return self.forbid_params(span, int, params);
            }
            ["float"] => {
                return self.forbid_params(span, T::from(Prim::Float), params);
            }
            ["self"] => {
                if self.type_info.allow_self {
                    return T::self_();
                } else {
                    self.ast
                        .sources
                        .error("invalid type")
                        .m(self.module)
                        .eline(
                            span,
                            "`self` is only allowed in type and implementation declarations",
                        )
                        .emit();

                    return T::from(Prim::Poison);
                }
            }
            [name] if name.starts_with('u') => {
                if let Ok(n) = name[1..].parse::<u8>() {
                    let int = T::from(Prim::Int(false, Bitsize(n)));
                    return self.forbid_params(span, int, params);
                }
            }
            [name] if name.starts_with('i') => {
                if let Ok(n) = name[1..].parse::<u8>() {
                    let int = T::from(Prim::Int(true, Bitsize(n)));
                    return self.forbid_params(span, int, params);
                }
            }
            [name] => match self.try_generic(name, params.len()) {
                Ok(gen) => {
                    return self.forbid_params(span, T::generic(gen), params);
                }
                Err(GenericError::InconsistentHKT(exp)) => {
                    self.emit_inconsistent_hkt(span, exp, params.len());
                    return T::from(Prim::Poison);
                }
                Err(GenericError::NotFound) => {}
            },
            _ => {}
        }

        match self.ast.lookups.resolve_type(self.module, path) {
            Ok(entity) => match entity.key {
                Entity::Type(tkey) => {
                    let params = self.tys(params);
                    T::defined(entity.module.m(tkey), params)
                }
                _ => {
                    let name = path.last().unwrap();
                    self.ast
                        .sources
                        .emit_wrong_entity(self.module, span, name, "type", entity.key);

                    T::from(Prim::Poison)
                }
            },

            Err(err) => {
                self.ast
                    .sources
                    .emit_lookup_err(span, self.module, "type", err);

                T::from(Prim::Poison)
            }
        }
    }

    fn forbid_params<T: Ty>(&self, span: Span, ty: T, params: &[Tr<parser::Type<'s>>]) -> T {
        if params.len() == 0 {
            ty
        } else {
            self.ast
                .sources
                .error("invalid type")
                .m(self.module)
                .eline(span, "this type can not take any type parameters")
                .emit();

            T::from(Prim::Poison)
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
            if let Some(generic) = forall.find(|gdata| gdata.name == name) {
                check_param_len(&mut forall[generic].params, plen)
                    .map_err(GenericError::InconsistentHKT)?;
                return Ok(Generic::new(generic, *kind));
            }
        }

        for (forall, kind) in self.type_info.cforalls.iter_mut().rev() {
            if let Some(generic) = forall.find(|gdata| gdata.name == name) {
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

    pub fn add_constraint<T: Ty>(
        &mut self,
        generic: Generic,
        ty: Tr<&lumina_parser::Type<'s>>,
    ) -> Result<(), NotATrait<T>> {
        let (trait_, params) = self.ty::<T>(ty).as_trait().map_err(NotATrait)?;

        let (forall, kind) = T::primary_generics(self.type_info).unwrap();
        assert_eq!(generic.kind, *kind);

        forall[generic.key]
            .trait_constraints
            .push(Constraint { span: ty.span, trait_, params });

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

pub struct NotATrait<T>(pub T);

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
