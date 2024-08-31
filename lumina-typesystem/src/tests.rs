use super::{Finalizer, Forall, TEnv, Transformer, Ty, Type, TypeSystem, Upgrade};
use key::M;
use key::{MapExt, ModMap};
use lumina_key as key;
use lumina_util::{Span, Spanned};
use std::collections::HashMap;

#[allow(non_upper_case_globals)]
const span: Span = Span::null();

fn i64<T>() -> Ty<T> {
    Ty::int(true, 64)
}
fn u64<T>() -> Ty<T> {
    Ty::int(false, 64)
}
fn u8<T>() -> Ty<T> {
    Ty::int(false, 8)
}

#[test]
fn concrete_types() {
    lumina_util::test_logger();

    let mut tenv = TEnv::new();
    let mut ts = system(&mut tenv);

    let got = Ty::tuple(vec![i64(), Ty::bool(), Ty::pointer(u64())]);

    assert!(ts.unify(span, &got, &got));
    assert!(ts.env.vars.is_empty());
}

fn fin<'a, 's>(ts: TypeSystem<'a, 's>) -> Finalizer<'a, 's> {
    Finalizer { ts, default_to_generic: None }
}

const VEC: M<key::Record> = M { value: key::Record(0), module: key::Module(0) };

fn system<'a, 's>(tenv: &'a mut TEnv<'s>) -> TypeSystem<'a, 's> {
    let mut fnames = ModMap::new();
    let mut ftypes = ModMap::new();
    let mut records = ModMap::new();
    let mut field_lookup = HashMap::new();
    let m = key::Module(0);

    fnames.add_module(0);
    ftypes.add_module(0);
    records.add_module(0);

    let mut add =
        |name: &'static str, generics: &[&'static str], fn_: &[&'static str], ft: &[Type]| {
            let record =
                records[m].push((name.tr(span), Forall::from_names(generics.iter().copied())));
            fnames[m].push_as(record, fn_.iter().copied().map(|n| n.tr(span)).collect());
            ftypes[m].push_as(record, ft.iter().cloned().map(|ty| ty.tr(span)).collect());
            fn_.iter().for_each(|&name| {
                field_lookup
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push(M { value: record, module: m })
            });
        };

    add("Vec", &["a"], &["len", "ptr"], &[u64(), Ty::pointer(u8())]);

    TypeSystem::new(
        tenv,
        64,
        Box::leak(Box::new(fnames)),
        Box::leak(Box::new(ftypes)),
        Box::leak(Box::new(records)),
        Box::leak(Box::new(field_lookup)),
    )
}

#[test]
fn tuple_of_type_vars() {
    lumina_util::test_logger();

    let mut env = TEnv::new();
    let mut ts = system(&mut env);

    let a = ts.env.var(span);
    let b = ts.env.var(span);

    let got = Ty::tuple(vec![Ty::infer(a), Ty::infer(a), Ty::infer(b)]);

    let exp = Ty::tuple(vec![u64(), i64(), Ty::bool()]);
    assert!(!ts.unify(span, &got, &exp));

    let exp = Ty::tuple(vec![i64(), i64(), Ty::infer(b)]);
    assert!(ts.unify(span, &got, &exp));

    let c = ts.env.var(span);
    let exp = Ty::tuple(vec![i64(), i64(), Ty::infer(c)]);
    assert!(ts.unify(span, &got, &exp));

    fin(ts).infer_all_unknown_types();

    let got = Upgrade(&env).transform(&got);
    let exp = Upgrade(&env).transform(&exp);

    assert_eq!(&got, &Ty::tuple(vec![i64(), i64(), Ty::tuple(vec![])]));
    assert_eq!(got, exp);
}

#[test]
fn replicated_issue() {
    lumina_util::test_logger();

    let mut env = TEnv::new();
    let mut ts = system(&mut env);

    let vec = Ty::defined(VEC, vec![u8()]);

    // {ptr, len} = ...
    let pat = ts.env.var(span);
    let ptr = ts.env.add_field(pat, "ptr".tr(span));
    let len = ts.env.add_field(pat, "len".tr(span));

    assert!(ts.unify(span, &vec, &Ty::infer(pat)));

    // TODO:
    //
    // It could be because of merging vars?
    //
    // should we 'fix' all the fields when merging field vars?
    // I think so. Both should point to the new one

    fin(ts).infer_all_unknown_types();

    assert_eq!(Ty::defined(VEC, vec![u8()]), Upgrade(&env).special(&pat));
    assert_eq!(u64(), Upgrade(&env).special(&len));
    assert_eq!(Ty::pointer(u8()), Upgrade(&env).special(&ptr));
}
