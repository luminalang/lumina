use crate::prelude::*;
use ast::Entity;
use lumina_parser as parser;
use lumina_parser::func::{Header as FuncHeader, Typing as ParserTyping};
use lumina_typesystem::{
    ConstGeneric, Forall, Generic, GenericKind, IType, Inference, IntSize, Static, TEnv, Ty, Var,
};
use lumina_util::{Spanned, Tr};
use smallvec::SmallVec;
use std::fmt;

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

    pub fn declare_generic(&mut self, name: &'s str, con: Option<ConstGeneric>) -> Option<Generic> {
        match self.iforalls.last_mut() {
            Some((forall, kind)) => {
                let gkey = forall.push(name);
                forall[gkey].const_ = con;
                let generic = Generic::new(gkey, *kind);
                return Some(generic);
            }
            None => {
                if let Some((forall, kind)) = self.cforalls.last_mut() {
                    let gkey = forall.push(name);
                    let generic = Generic::new(gkey, *kind);
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
            params: self.ty_sequence(&typing.ptypes),
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

    pub fn typing_or_inferred<T>(
        &mut self,
        header: &FuncHeader<'s>,
        lifted: bool,
    ) -> hir::Typing<Ty<T>>
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
                hir::Typing::inferred(&header.params, header.name.span, vars, lifted)
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
            parser::Type::List(inner, arr) => {
                if inner.len() != 1 {
                    self.ast
                        .sources
                        .error("invalid list type")
                        .m(self.module)
                        .eline(ty.span, "a list's elements must all be of the same type")
                        .emit();

                    Ty::poison()
                } else {
                    let inner = self.ty(inner[0].as_ref());
                    match arr {
                        parser::ListLength::Name(name) => self
                            .resolve_array_generic(*name)
                            .map(|generic| Ty::const_array(generic, inner))
                            .unwrap_or_else(Ty::poison),
                        parser::ListLength::Exact(len) => Ty::array(**len, inner),
                        parser::ListLength::None => Ty::list(self.type_info.list, vec![inner]),
                    }
                }
            }
            parser::Type::Poison => Ty::poison(),
            parser::Type::Defined(apath, params) => {
                if let Some(name) = apath.path.as_name() {
                    if let Some(nty) = self.try_builtin(name.tr(ty.span), params.len()) {
                        self.check_param_count(&ty, params.len().tr(ty.span), 0);
                        return nty;
                    }
                }

                match self.try_resolved(ty.span, apath.path.as_slice()) {
                    Resolved::Some(key) => {
                        let header = &self.ast.entities.header_of_ty(key).header;
                        let ok = self.check_param_count(
                            header,
                            params.len().tr(ty.span),
                            header.type_params.len(),
                        );

                        if ok {
                            let params = self.tys(params);
                            Ty::defined(key, params)
                        } else {
                            Ty::poison()
                        }
                    }
                    Resolved::Alias(nty) => {
                        self.check_param_count(&ty, params.len().tr(ty.span), 0);
                        return nty;
                    }
                    Resolved::Poison => Ty::poison(),
                }
            }
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

    // In some cases (such as function signature) we want to both allow
    // `int int -> int`
    // and
    // `Maybe int -> int`
    //
    // However; these are ambigious and need type resolution to handle properly.
    // So; we do a sort of ad-hoc currying to resolve it if there's only a single type.
    fn ty_sequence<T: FromVar, F>(&mut self, tys: &[Tr<parser::Type<'s>>]) -> F
    where
        F: FromIterator<Tr<Ty<T>>>,
    {
        if tys.is_empty() {
            return [].into_iter().collect();
        }

        match &tys[0].value {
            parser::Type::Defined(apath, params) if params.is_empty() => {
                let as_first = |this: &mut Self, nty: Ty<T>| {
                    this.check_param_count(&tys[0], params.len().tr(tys[0].span), 0);
                    let then = this.tys_spanned::<Vec<_>, T>(&tys[1..]);
                    [nty.tr(tys[0].span)].into_iter().chain(then).collect()
                };

                if let Some(name) = apath.path.as_name() {
                    if let Some(nty) = self.try_builtin(name.tr(tys[0].span), params.len()) {
                        return as_first(self, nty);
                    }
                }

                match self.try_resolved::<T>(tys[0].span, apath.path.as_slice()) {
                    Resolved::Some(key) => {
                        let header = &self.ast.entities.header_of_ty(key).header;
                        if header.type_params.is_empty() {
                            as_first(self, Ty::defined(key, vec![]))
                        } else {
                            let params = self.tys::<T>(&tys[1..]);
                            let tspan = Span::from_elems(tys, |v| v.span);
                            [Ty::defined(key, params).tr(tspan)].into_iter().collect()
                        }
                    }
                    Resolved::Alias(ty) => as_first(self, ty),
                    Resolved::Poison => as_first(self, Ty::poison()),
                }
            }
            _ => self.tys_spanned(tys),
        }
    }

    pub fn resolve_array_generic(&mut self, name: Tr<&'s str>) -> Option<Generic> {
        let usize_ = IntSize::new(false, self.default_int_size);
        self.try_generic(*name, 0, Some(ConstGeneric::Int(usize_)))
            .inspect_err(|_| {
                self.ast
                    .sources
                    .error("unknown const generic")
                    .m(self.module)
                    .eline(
                        name.span,
                        format!("no const generic named {name} declared in scope"),
                    )
                    .emit()
            })
            .ok()
    }

    fn try_builtin<T: FromVar>(&mut self, name: Tr<&'s str>, plen: usize) -> Option<Ty<T>> {
        self.try_prim(name)
            .or_else(|| self.try_generic(*name, plen, None).ok().map(Ty::Generic))
    }

    fn try_prim<T: FromVar>(&mut self, path: Tr<&'s str>) -> Option<Ty<T>> {
        match *path {
            "_" => Some(self.inference(path.span)),
            "bool" => Some(Ty::bool()),
            "int" => Some(Ty::Int(IntSize::new(true, self.default_int_size))),
            "uint" => Some(Ty::Int(IntSize::new(false, self.default_int_size))),
            "float" => Some(Ty::f64()),
            "self" => match self.type_info.self_handler {
                SelfHandler::Substituted(kind) => {
                    let (forall, gkind) = &self.type_info.cforalls.last().unwrap();
                    assert_eq!(*gkind, GenericKind::Entity);
                    let params = forall.to_types(*gkind);
                    Some(Ty::defined(kind, params))
                }
                SelfHandler::Direct => return Some(Ty::Simple("self")),
                SelfHandler::Disallowed => {
                    self.ast
                        .sources
                        .error("invalid type")
                        .m(self.module)
                        .eline(
                            path.span,
                            "`self` is only allowed in type and implementation declarations",
                        )
                        .emit();

                    Some(Ty::poison())
                }
            },
            "string" => Some(Ty::string(self.type_info.string, vec![])),
            name if name.starts_with('u') => name[1..]
                .parse::<u8>()
                .ok()
                .map(|n| Ty::Int(IntSize::new(false, n))),
            name if name.starts_with('i') => name[1..]
                .parse::<u8>()
                .ok()
                .map(|n| Ty::Int(IntSize::new(true, n))),
            _ => None,
        }
    }

    fn inference<T: FromVar>(&mut self, span: Span) -> Ty<T> {
        match self.type_info.inference.as_mut() {
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
                let var = vars.var(span);
                if self.type_info.declare_generics {
                    vars.enable_lift_to_generic(var);
                }
                // TODO: rememember to fix forbid_params in upper scope
                return T::var(var);
            }
        }
    }

    fn warn_useless_annotation(&self, apath: Tr<&parser::AnnotatedPath<'s>>) {
        if !apath.for_segments.is_empty() {
            self.ast.sources
                .warning("invalid type annotation")
                .m(self.module)
                .eline(apath.span, "this annotation doesn't do anything")
                .text("if you're attempting to annotate an associated type of a trait constraint, they are very experimental")
                .emit();
        }
    }

    fn try_resolved<T: FromVar>(&mut self, span: Span, path: &[&'s str]) -> Resolved<T> {
        match self.ast.lookups.resolve_type(self.module, path) {
            Ok(entity) => match entity.key {
                Entity::Alias(ty) => Resolved::Alias(self.ty(ty.as_ref())),
                Entity::Type(tkey) => {
                    let key = M(entity.module, tkey);
                    Resolved::Some(key)
                }
                _ => {
                    let name = path.last().unwrap();
                    self.ast
                        .sources
                        .emit_wrong_entity(self.module, span, name, "type", entity.key);

                    Resolved::Poison
                }
            },

            Err(err) => {
                self.ast
                    .sources
                    .emit_lookup_err(span, self.module, "type", err);

                Resolved::Poison
            }
        }
    }

    fn check_param_count(&mut self, header: impl fmt::Display, got: Tr<usize>, exp: usize) -> bool {
        let err = |span, msg| {
            self.ast
                .sources
                .error("invalid type")
                .m(self.module)
                .eline(span, msg)
                .emit();
            false
        };

        if *got < exp {
            err(
                got.span,
                format!("missing {} type parameters for `{header}`", exp - *got),
            )
        } else if *got > *got {
            err(got.span, format!("excess parameter for `{header}`"))
        } else {
            true
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

    pub fn try_generic(
        &mut self,
        name: &'s str,
        plen: usize,
        const_: Option<ConstGeneric>,
    ) -> Result<Generic, GenericError> {
        for (forall, kind) in self.type_info.iforalls.iter_mut().rev() {
            if let Some(generic) = forall.find(name) {
                return if plen != 0 {
                    Err(GenericError::InconsistentHKT(plen))
                } else {
                    Ok(Generic::new(generic, *kind))
                };
            }
        }

        for (forall, kind) in self.type_info.cforalls.iter_mut().rev() {
            if let Some(generic) = forall.find(name) {
                return if plen != 0 {
                    Err(GenericError::InconsistentHKT(plen))
                } else {
                    Ok(Generic::new(generic, *kind))
                };
            }
        }

        // Implicitly declare if able
        if name.len() == 1 && self.type_info.declare_generics && plen == 0 {
            if let Some(generic) = self.type_info.declare_generic(name, const_) {
                return Ok(generic);
            }
        }

        Err(GenericError::NotFound)
    }
}

enum Resolved<T> {
    Some(M<key::TypeKind>),
    Alias(Ty<T>),
    Poison,
}

pub enum GenericError {
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
