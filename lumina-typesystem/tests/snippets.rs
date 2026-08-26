use lumina_key as key;
use lumina_key::{EntityList, Map};
use lumina_typesystem::{
    Annotation, CallableKind, Checker, Environment, Error, Finalizer, ForallEnv, GenericTag,
    InferenceUnifier, IntSize, KnownType as T, KnownType, MismatchKind, Prim, ReceiverLookupResult,
    Resolver, TaggedGeneric, Var,
};
use std::collections::HashMap;
use std::sync::Once;
use tracing::info;
use tracing_subscriber::{EnvFilter, layer::SubscriberExt, registry::Registry};

static LOGGER: Once = Once::new();

type Ident = &'static str;
type Type = KnownType<Ident>;

struct TestResolver {
    fenv: ForallEnv<Ident>,
}

impl Default for TestResolver {
    fn default() -> Self {
        let mut fenv = HashMap::new();
        fenv.entry(GenericTag::Func).or_default();
        Self { fenv }
    }
}

fn instantiate_function(
    env: &mut Environment<Ident>,
    generics: usize,
    params: &[Type],
    ret: &Type,
) -> Var {
    let mut anot = Annotation::new();
    for _ in 0..generics {
        let var = env.unknown();
        anot.push(GenericTag::Func, var, &mut env.var_pool);
    }
    let params = env.instantiate_types(&anot, params, &mut |ident| *ident);
    let ret = env.instantiate(&anot, ret, &mut |ident| *ident);
    env.function(CallableKind::FnPointer, params, ret)
}

impl Resolver<Ident> for TestResolver {
    type ResolvedMetadata = &'static str;

    fn implicitly_declare_generic(&mut self, tag: GenericTag) -> TaggedGeneric {
        lumina_typesystem::implicitly_declare(&mut self.fenv, tag)
    }

    fn receiver_lookup(
        &mut self,
        env: &mut Environment<Ident>,
        root: lumina_typesystem::KnownTypeRoot<Ident>,
        name: &str,
    ) -> ReceiverLookupResult {
        match root {
            lumina_typesystem::KnownTypeRoot::Pointer => match name {
                "offset" => {
                    let a = env.unknown();
                    let ptr = env.pointer(a);
                    let i64 = env.i(64);
                    let mut params = EntityList::new();
                    params.push(i64, &mut env.var_pool);
                    params.push(ptr, &mut env.var_pool);
                    ReceiverLookupResult::Func { params, ret: ptr }
                }
                _ => ReceiverLookupResult::NotFound,
            },
            _ => ReceiverLookupResult::NotFound,
        }
    }

    fn type_of_field(
        &self,
        name: &Ident,
        field: key::Field,
    ) -> Result<(Self::ResolvedMetadata, T<Ident>), lumina_typesystem::ResolverError<Ident>> {
        match *name {
            "Point" => match field {
                key::Field(0) => Ok(("Point instantiation", T::Generic(tgeneric('a')))),
                key::Field(1) => Ok(("Point instantiation", T::Generic(tgeneric('a')))),
                _ => Err(lumina_typesystem::ResolverError::FieldNotFound(
                    *name, field,
                )),
            },
            "string" => match field {
                key::Field(0) => Ok(("string ptr", T::pointer(T::i(8)))),
                key::Field(1) => Ok(("string len", T::i(32))),
                _ => Err(lumina_typesystem::ResolverError::FieldNotFound(
                    *name, field,
                )),
            },
            "T" => match field {
                _ => Err(lumina_typesystem::ResolverError::FieldNotFound(
                    *name, field,
                )),
            },
            _ => Err(lumina_typesystem::ResolverError::RecordNotFound(*name)),
        }
    }

    fn guess_by_fields<'a, I>(&self, record: Var, mut fields: I) -> Option<Ident>
    where
        I: Iterator<Item = &'a str> + Clone,
    {
        let fst = fields.next()?;

        match fst {
            "x" | "y" => Some("Point"),
            _ => None,
        }
    }

    fn get_field(&self, record: &Ident, name: &str) -> Option<key::Field> {
        match *record {
            "Point" => match name {
                "x" => Some(key::Field(0)),
                "y" => Some(key::Field(1)),
                _ => None,
            },
            "string" => match name {
                "ptr" => Some(key::Field(0)),
                "len" => Some(key::Field(1)),
                _ => None,
            },
            "T" => match name {
                _ => None,
            },
            _ => panic!("unknown record: {record}"),
        }
    }

    fn for_each_type_parameter<F>(&self, name: &Ident, mut f: F)
    where
        F: FnMut(key::Generic),
    {
        match *name {
            "Point" => {
                f(tgeneric('a').key);
            }
            _ => panic!("unknown defined type: {name}"),
        }
    }

    fn map_type_key(&self, _: Self::ResolvedMetadata, ident: &Ident) -> Ident {
        *ident
    }
}

fn process(mut env: Environment<Ident>, expected_errors: Vec<Error<Ident>>) -> Map<Var, Type> {
    let resolver = TestResolver::default();
    InferenceUnifier::new(
        &mut env,
        resolver,
        IntSize::new(true, 32),
        vec!["List"],
        Some("List"),
    )
    .infer();

    let assignments = Finalizer::new(&mut env).finalize_all();

    #[cfg(debug_assertions)]
    for (k, v) in assignments.iter() {
        println!("  {k} -> {v}");
    }

    let errors = Checker::new(&assignments, &mut env).type_check();

    let mut fail = false;

    for err in &errors {
        #[cfg(debug_assertions)]
        eprintln!("{err:#?}");
        fail |= !expected_errors.contains(&err);
    }

    for err in expected_errors {
        if !errors.contains(&err) {
            #[cfg(debug_assertions)]
            eprintln!("missing error:\n{err:#?}");
            fail = true;
        }
    }

    if fail {
        panic!("one or more error checks failed");
    }

    assignments
}

fn generic(c: char) -> TaggedGeneric {
    let i = (c as u8) - b'a';
    TaggedGeneric::new(GenericTag::Func, key::Generic(i as u32))
}

fn tgeneric(c: char) -> TaggedGeneric {
    let i = (c as u8) - b'a';
    TaggedGeneric::new(GenericTag::Type, key::Generic(i as u32))
}

fn logger() {
    LOGGER.call_once(|| {
        let filter = EnvFilter::from_default_env();

        let layer = tracing_tree::HierarchicalLayer::default()
            .with_writer(std::io::stdout)
            .with_indent_lines(true)
            .with_indent_amount(2)
            .with_verbose_entry(false)
            .with_verbose_exit(false)
            .with_targets(true);

        let subscriber = Registry::default().with(layer).with(filter);

        tracing::subscriber::set_global_default(subscriber).unwrap();
    });
}

// #[test]
// fn depgraph_troubleshooting() {
//     logger();
//     let mut env = Environment::new();

//     let f = {
//         let uint = env.u(64);
//         let u8 = env.u(8);
//         let params = EntityList::from_slice(&[uint], &mut env.var_pool);
//         env.function(CallableKind::Closure, params, u8)
//     };

//     let ret = env.u(16);

//     env.leave_signature_enter_expression();

//     let alloc = {
//         let uint = env.u(64);
//         let a = env.generic(generic('a'));
//         let ta_params = EntityList::from_slice(&[a], &mut env.var_pool);
//         let ta = env.defined("T", ta_params);
//         let params = EntityList::from_slice(&[uint], &mut env.var_pool);
//         // let f = env.function(CallableKind::FnPointer, params, ta);
//         instantiate_function(&mut env, 1, &params, ret)
//     };

//     let set = {
//         let a = env.generic(generic('a'));
//         let aa = env.generic(generic('a'));
//         let u16 = env.u(16);
//         let ta_params = EntityList::from_slice(&[aa], &mut env.var_pool);
//         let ta = env.defined("T", ta_params);
//         let params = EntityList::from_slice(&[a, ta], &mut env.var_pool);
//         env.function(CallableKind::FnPointer, params, u16)
//     };

//     let _0 = env.numeric();
//     let _1 = env.numeric();

//     let alloc_ret = {
//         let apply = env.apply(alloc);
//         env.apply_next_parameter(apply, _1);
//         env.get_return_type(apply)
//     };

//     let f_ret = {
//         let apply = env.apply(f);
//         env.apply_next_parameter(apply, _0);
//         env.get_return_type(apply)
//     };

//     let set_ret = {
//         let apply = env.apply(set);
//         env.apply_next_parameter(apply, f_ret);
//         env.apply_next_parameter(apply, alloc_ret);
//         env.get_return_type(apply)
//     };

//     env.assign_return(set_ret, ret);

//     println!("{env:?}");
//     process(env, vec![]);
// }

// #[test]
// fn lambda() {
//     logger();
//     let mut env = Environment::new();

//     let folder = {
//         env.function(CallableKind::Closure, params, ret);
//         instantiate_
//     };

//     println!("{env:?}");
//     process(env, vec![]);
// }

#[test]
fn weird_apply_bug() {
    logger();
    let mut env = Environment::new();

    let ptr = {
        let a = env.generic(generic('a'));
        env.pointer(a)
    };
    let ret = env.generic(generic('a'));

    env.leave_signature_enter_expression();

    let pat_ptr = env.unknown();

    // Unify patterns against signature
    env.assign(pat_ptr, ptr);

    // builtin:deref
    let func = {
        let params = EntityList::from_slice(&[env.unknown()], &mut env.var_pool);
        let ret = env.unknown();

        env.function(CallableKind::FnPointer, params, ret)
    };

    // ptr
    let ptr_param = {
        let appl = env.apply(pat_ptr);
        env.get_return_type(appl)
    };

    let appl = env.apply(func);
    env.apply_next_parameter(appl, ptr_param);

    let func_ret = env.get_return_type(appl);
    env.assign_return(func_ret, ret);

    println!("{env:?}");

    process(env, vec![]);
}

#[test]
fn weird_field_bug() {
    logger();
    let mut env = Environment::new();

    let str = env.defined("string", EntityList::new());
    let ret_len = env.i(64);

    env.leave_signature_enter_expression();

    let pattern = env.unknown();
    env.assign(str, pattern);

    let len = env.add_field(pattern, "len");

    env.assign_return(len, ret_len);

    process(env, vec![]);
}

#[test]
fn static_function_application() {
    logger();
    let mut env = Environment::new();
    env.leave_signature_enter_expression();

    // let f = |x: i32, y: i32| -> i32
    let func = {
        let params = EntityList::from_slice(&[env.i(32), env.i(32)], &mut env.var_pool);
        let ret = env.i(32);
        env.function(CallableKind::Closure, params, ret)
    };

    // let r = f(1_i32, 2_i32)
    let ret = {
        let application = env.apply(func);
        let i32 = env.i(32);
        env.apply_next_parameter(application, i32);
        env.apply_next_parameter(application, i32);
        env.get_return_type(application)
    };

    // return r
    let expected_return_type = env.i(64);
    env.assign_return(ret, expected_return_type);

    process(
        env,
        vec![Error::Mismatch {
            var: ret,
            expected: T::i(64),
            given: T::i(32),
            kind: MismatchKind::Assignment,
        }],
    );
}

#[test]
fn generic_function_application() {
    logger();
    let mut env = Environment::new();
    env.leave_signature_enter_expression();

    // let map = |list: [a], f: (a -> b)| -> [b]
    let func = {
        let a = || T::generic(generic('a'));
        let b = || T::generic(generic('b'));
        let f = T::function(CallableKind::Closure, [a()], b());
        instantiate_function(&mut env, 2, &[T::list(a()), f], &T::list(b()))
    };

    // let r = map([5_i32], |n: i32| n as i64);
    let ret = {
        let _i32 = env.i(32);
        let _i64 = env.i(64);
        let list = env.list(_i32);
        let params = EntityList::from_slice(&[_i32], &mut env.var_pool);
        let f = env.function(CallableKind::Closure, params, _i64);

        let application = env.apply(func);
        env.apply_next_parameter(application, list);
        env.apply_next_parameter(application, f);

        env.get_return_type(application)
    };

    // return r
    let expected_return_type = {
        let _i8 = env.i(8);
        env.list(_i8)
    };
    env.assign_return(ret, expected_return_type);

    process(
        env,
        vec![Error::Mismatch {
            var: ret,
            expected: T::Defined("List", vec![T::i(8)].into()),
            given: T::Defined("List", vec![T::i(64)].into()),
            kind: MismatchKind::Assignment,
        }],
    );
}

#[test]
fn list() {
    logger();
    let mut env = Environment::new();
    env.leave_signature_enter_expression();

    // [1_i32, 2_i32, 1.0]
    let (sameas, _list, float) = {
        let (sameas_key, list, _) = env.list_sameas();

        let _i32 = env.i(32);
        let float = env.prim(Prim::Float);

        env.add_sameas_member(sameas_key, _i32);
        env.add_sameas_member(sameas_key, _i32);
        env.add_sameas_member(sameas_key, float);

        (sameas_key, list, float)
    };

    process(
        env,
        vec![Error::Mismatch {
            var: float,
            expected: T::i(32),
            given: KnownType::Prim(Prim::Float),
            kind: MismatchKind::SameasList(sameas),
        }],
    );
}

#[test]
fn if_expr() {
    logger();
    let mut env = Environment::new();
    env.leave_signature_enter_expression();

    // if true { 20_i32 } else { 1.0 }
    let (sameas, _expr, float) = {
        let (sameas_key, expr) = env.expr_sameas(None);

        let _i32 = env.i(32);
        let float = env.prim(Prim::Float);

        env.add_sameas_member(sameas_key, _i32);
        env.add_sameas_member(sameas_key, float);

        (sameas_key, expr, float)
    };

    process(
        env,
        vec![Error::Mismatch {
            var: float,
            expected: T::i(32),
            given: T::Prim(Prim::Float),
            kind: MismatchKind::SameasExpr(sameas),
        }],
    );
}

#[test]
fn field_inference() {
    logger();
    let mut env = Environment::new();
    env.leave_signature_enter_expression();

    // let point;
    // point.x = 4_i32;
    let (_record, x, _i32) = {
        let point = env.unknown();
        let x = env.add_field(point, "x");
        let _i32 = env.i(32);
        env.assign(_i32, x);
        (point, x, _i32)
    };

    // return point.x
    let expected_return_type = env.i(64);
    env.assign_return(x, expected_return_type);

    process(
        env,
        vec![Error::Mismatch {
            var: x,
            expected: T::i(64),
            given: T::i(32),
            kind: MismatchKind::Assignment,
        }],
    );
}

#[test]
fn defaulting() {
    logger();
    let mut env = Environment::new();

    // x: _, y: _ // in function signature
    let x = env.unknown();
    let y = env.unknown();

    env.leave_signature_enter_expression();

    // let n = 4 // this number isn't used. So; we don't know which numeric type it is
    let n = env.numeric();

    // let z
    let z = env.unknown();

    let assignments = process(env, vec![]);
    assert_eq!(assignments[x], T::Generic(generic('a')));
    assert_eq!(assignments[y], T::Generic(generic('b')));
    assert_eq!(assignments[n], T::Prim(Prim::Int(IntSize::new(true, 32))));
    assert_eq!(assignments[z], T::default_unit_type());
}

#[test]
fn type_sensitive_nameres() {
    logger();
    let mut env = Environment::new();
    let unknown = env.unknown();
    let param0 = env.pointer(unknown);
    let float = env.prim(Prim::Float);
    let expected_return_type = env.pointer(float);

    env.leave_signature_enter_expression();

    let func = env.type_resolved_function("offset");

    let appl = env.apply(func);

    env.apply_next_parameter(appl, float);
    env.apply_next_parameter(appl, param0);

    let name_sensitive_ret = env.get_return_type(appl);

    env.assign_return(name_sensitive_ret, expected_return_type);

    process(
        env,
        vec![Error::Mismatch {
            var: float,
            expected: T::i(64),
            given: T::Prim(Prim::Float),
            kind: MismatchKind::Parameter(appl, 0),
        }],
    );
}

#[test]
fn article_showcase_example() {
    println!("{:#?}", article_showcase_example_go());
}

#[test]
#[ignore]
fn inspect_article_showcase_example() {
    panic!("{:#?}", article_showcase_example_go());
}

fn article_showcase_example_go() -> Map<Var, Type> {
    logger();
    let mut env = Environment::new();
    let param0 = env.unknown();
    info!("{param0} = param0");
    let param1 = env.i(32);
    info!("{param1} = param1");
    let return_ = {
        let forall = EntityList::from_slice(&[env.prim(Prim::Float)], &mut env.var_pool);
        env.defined("Just", forall)
    };
    info!("{return_} = return_");
    env.leave_signature_enter_expression();

    // let z = {
    let z = {
        // let n = 4;
        let n = env.numeric();
        info!("{n} = n");

        // let fst(param0, { x = n, y = n });

        let fst = instantiate_function(
            &mut env,
            2,
            &[T::Generic(generic('a')), T::Generic(generic('b'))],
            &T::Generic(generic('a')),
        );
        info!("{fst} = fst");

        let record = {
            let var = env.unknown();
            let x = env.add_field(var, "x");
            let y = env.add_field(var, "y");

            info!("{var} = var\n{x} = x\n{y} = y");

            env.assign(n, x);
            env.assign(n, y);

            var
        };
        info!("{record} = record");

        let appl = env.apply(fst);
        info!("{appl} = appl");
        env.apply_next_parameter(appl, param0);
        env.apply_next_parameter(appl, record);

        env.get_return_type(appl)
    };
    info!("{z} = z");

    // let _ = [z, param1];
    let sameas = {
        let (sameas_key, list, elem) = env.list_sameas();
        info!("{list} = list");
        info!("{elem} = elem");
        env.add_sameas_member(sameas_key, z);
        env.add_sameas_member(sameas_key, param1);
        sameas_key
    };

    // Maybe::Just(param0);
    let just = {
        let constructor = instantiate_function(
            &mut env,
            1,
            &[T::Generic(generic('a'))],
            &T::Defined("Just", vec![T::Generic(generic('a'))].into()),
        );
        info!("{constructor} = constructor");

        let appl = env.apply(constructor);
        info!("{appl} = appl");
        env.apply_next_parameter(appl, param0);

        env.get_return_type(appl)
    };
    info!("{just} = just");

    // return Maybe::Just(param0);
    env.assign_return(just, return_);

    process(
        env,
        vec![Error::Mismatch {
            var: param1,
            expected: T::Prim(Prim::Float),
            given: T::Prim(Prim::Int(IntSize::new(true, 32))),
            kind: MismatchKind::SameasList(sameas),
        }],
    )
}

#[test]
fn article_feature_complete_example() {
    println!("{:#?}", article_feature_complete_example_go());
}

#[test]
#[ignore]
fn inspect_article_feature_complete_example() {
    panic!("{:#?}", article_feature_complete_example_go());
}

fn article_feature_complete_example_go() -> Map<Var, Type> {
    logger();
    let mut env = Environment::new();

    let a = env.unknown();
    let _b = env.unknown();
    let return_ = env.unknown();

    env.leave_signature_enter_expression();

    // let point = Point(a, 200);
    let [_let_point, x] = {
        let point_param_a = env.unknown();
        let func_params =
            EntityList::from_slice(&[point_param_a, point_param_a], &mut env.var_pool);
        let point_params = EntityList::from_slice(&[point_param_a], &mut env.var_pool);
        let func_ret = env.defined("Point", point_params);
        let func = env.function(CallableKind::FnPointer, func_params.clone(), func_ret);
        let _200 = env.numeric();

        let appl = env.apply(func);
        env.apply_next_parameter(appl, a);
        env.apply_next_parameter(appl, _200);

        let point = env.get_return_type(appl);

        let let_point = env.unknown();
        env.assign(point, let_point);
        let x = env.add_field(let_point, "x");

        [let_point, x]
    };

    // let list = [point.x, 300];
    {
        let list_literal = {
            let (same_as, list, _elem) = env.list_sameas();
            let _300 = env.numeric();
            env.add_sameas_member(same_as, x);
            env.add_sameas_member(same_as, _300);
            list
        };

        let list = env.unknown();
        env.assign(list_literal, list);
        list
    };

    // let record = { x = 100, y = 200 };
    let [_let_record, y] = {
        let record = env.unknown();
        let _100 = env.numeric();
        let _200 = env.numeric();
        let x = env.add_field(record, "x");
        let y = env.add_field(record, "y");
        env.assign(_100, x);
        env.assign(_200, y);

        let let_record = env.unknown();
        env.assign(record, let_record);
        [let_record, y]
    };

    // point.y == record.y
    let comparison = {
        let bool = env.i(8);
        let param = env.unknown();
        let func_params = EntityList::from_slice(&[param, param], &mut env.var_pool);
        let func = env.function(CallableKind::FnPointer, func_params, bool);

        let appl = env.apply(func);
        env.apply_next_parameter(appl, x);
        env.apply_next_parameter(appl, y);
        env.get_return_type(appl)
    };

    // return point == { x = 100, y = 200 };
    env.assign_return(comparison, return_);

    process(env, vec![])
}

#[test]
#[ignore]
fn bench() {
    fn iterations(n: usize) {
        let time = std::time::Instant::now();
        for _ in 0..n {
            defaulting();
            list();
            if_expr();
            field_inference();
            article_feature_complete_example_go();
            article_showcase_example_go();
            static_function_application();
            generic_function_application();
        }

        println!("{n} took {:#?}", time.elapsed());
    }

    iterations(100);
    iterations(10000);
    iterations(100000);

    panic!();
}
