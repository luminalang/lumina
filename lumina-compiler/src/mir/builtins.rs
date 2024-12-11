use crate::{hir, mir, mir::Expr};
use lumina_typesystem::{Container, Transformer, Ty, Var};
use lumina_util::{Span, Spanned, Tr};
use mir::func::InstCall;
use std::collections::HashMap;

pub fn signature<'t, 's>(lower: &mut mir::Verify<'t, 's>, span: Span, name: &str) -> InstCall {
    macro_rules! sig {
        ($($param:tt),* => $ret:tt) => {
            {
            let ptypes = [$(ty!($param)),*].into();
            let ret = ty!($ret);
            InstCall::LocalCall(span, ptypes, ret, Container::FnPointer)
            }
        };
        (direct $param:tt) => {
            {
            let param = ty!($param);
            InstCall::Local(param.tr(span))
            }
        }
    }

    let mut generics: HashMap<char, Var> = HashMap::new();

    macro_rules! ty {
        (bool) => { Ty::bool() };
        (uint) => { Ty::Int(lower.target.uint()) };
        ((pointer $inner:tt)) => { Ty::pointer(ty!($inner)) };
        (($($param:tt),*)) => { Ty::tuple(vec![$(ty!($param)),*]) };
        ($generic:literal) => {
            Ty::Special(
                *generics
                    .entry($generic)
                    .or_insert_with(|| lower.vars().var(span)),
            )
        };
    }

    match name {
        "plus" | "minus" | "mul" | "div" => {
            sig! { 'a', 'a' => 'a' }
        }
        "plus_checked" | "minus_checked" | "mul_checked" | "div_checked" => {
            sig! { 'a', 'a' => ('a', bool) }
        }

        "array_len" => sig! { 'a' => uint },
        "array_get" => sig! { uint, 'a' => 'b' },
        "iabs" => sig! { 'n' => 'n' },
        "eq" | "lt" | "gt" => sig! { 'a', 'a' => bool },
        "deref" => sig! { (pointer 'a') => 'a' },
        "memcpy" => sig! { (pointer 'a'), (pointer 'a'), uint => () },

        "write" => sig! { (pointer 'a'), 'a' => () },
        "offset" => sig! { (pointer 'a'), uint => (pointer 'a') },
        "reflect_type" => {
            InstCall::Local(Ty::defined(lower.items.pinfo.reflect_type, vec![]).tr(span))
        }
        "size_of" => sig! { direct uint },
        "align_of" => sig! { direct uint },
        "alloca" => sig! { direct (pointer 'a') },
        "unreachable" => sig! { direct 'a' },
        "transmute" => sig! { 'a' => 'b' },
        "val_to_ref" => sig! { 'a' => (pointer 'a') },
        _ => {
            lower.error("unrecognised builtin").eline(span, "").emit();
            InstCall::Local(Ty::poison().tr(span))
        }
    }
}

pub fn lower<'t, 's>(
    lower: &mut mir::Lower<'t, 's>,
    name: &str,
    params: &[Tr<hir::Expr<'s>>],
    tanot: &hir::TypeAnnotation<'s>,
) -> Expr {
    match name {
        "plus" => lower.lower_builtin(params, |p| Expr::Num("plus", Box::new(p))),
        "minus" => lower.lower_builtin(params, |p| Expr::Num("minus", Box::new(p))),
        "mul" => lower.lower_builtin(params, |p| Expr::Num("mul", Box::new(p))),
        "div" => lower.lower_builtin(params, |p| Expr::Num("div", Box::new(p))),
        "plus_checked" => lower.lower_builtin(params, |p| Expr::Num("plus_checked", Box::new(p))),
        "minus_checked" => lower.lower_builtin(params, |p| Expr::Num("minus_checked", Box::new(p))),
        "mul_checked" => lower.lower_builtin(params, |p| Expr::Num("mul_checked", Box::new(p))),
        "div_checked" => lower.lower_builtin(params, |p| Expr::Num("div_checked", Box::new(p))),
        "array_len" => lower.lower_builtin(params, |[p]| Expr::ArrayLen(Box::new(p))),
        "array_get" => lower.lower_builtin(params, |p| Expr::ArrayAccess(Box::new(p))),
        "iabs" => lower.lower_builtin(params, |[p]| Expr::IntAbs(Box::new(p))),
        "eq" => lower.lower_builtin(params, |p| Expr::Cmp("eq", Box::new(p))),
        "lt" => lower.lower_builtin(params, |p| Expr::Cmp("lt", Box::new(p))),
        "gt" => lower.lower_builtin(params, |p| Expr::Cmp("gt", Box::new(p))),
        "deref" => lower.lower_builtin(params, |[inner]| Expr::Deref(Box::new(inner))),
        "memcpy" => lower.lower_builtin(params, |params| Expr::MemCpy(Box::new(params))),
        "write" => lower.lower_builtin(params, |p| Expr::Write(Box::new(p))),
        "offset" => lower.lower_builtin(params, |p| Expr::Num("plus", Box::new(p))),
        "unreachable" => {
            let (name, ty) = tanot.for_entity[0].clone();
            assert_eq!(*name, "self");
            let ty = lower.finalizer().transform(&ty);
            lower.lower_builtin::<0>(params, |_| Expr::Unreachable(ty))
        }
        "transmute" => lower.lower_builtin(params, |[inner]| inner),
        "val_to_ref" => lower.lower_builtin(params, |[inner]| Expr::ValToRef(Box::new(inner))),
        "reflect_type" => {
            let (name, ty) = tanot.for_entity[0].clone();
            assert_eq!(*name, "self");
            let ty = lower.finalizer().transform(&ty);
            Expr::ReflectTypeOf(ty)
        }
        "size_of" => {
            let (name, ty) = tanot.for_entity[0].clone();
            assert_eq!(*name, "self");
            let ty = lower.finalizer().transform(&ty);
            Expr::SizeOf(ty)
        }
        "align_of" => {
            let (name, ty) = tanot.for_entity[0].clone();
            assert_eq!(*name, "self");
            let ty = lower.finalizer().transform(&ty);
            Expr::AlignOf(ty)
        }
        "alloca" => {
            let (name, ty) = tanot.for_entity[0].clone();
            assert_eq!(*name, "self");
            let ty = lower.finalizer().transform(&ty);
            Expr::Alloca(ty)
        }
        _ => panic!("unknown builtin: {name}"),
    }
}

impl<'t, 's> mir::Lower<'t, 's> {
    fn lower_builtin<const N: usize>(
        &mut self,
        params: &[Tr<hir::Expr<'s>>],
        constructor: impl FnOnce([Expr; N]) -> Expr,
    ) -> Expr {
        match <[Expr; N]>::try_from(self.lower_exprs(params)) {
            Ok(params) => constructor(params),
            Err(_) => Expr::Poison,
        }
    }
}
