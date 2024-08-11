use super::*;
use insta::assert_snapshot;
use lumina_typesystem::{Generic, GenericKind};

macro_rules! snapshot_tree_and_missing {
    ($lower:ident, $tree:expr) => {{
        println!("{}", $tree);
        let missing = MissingGeneration::new($lower.to_init()).run(&$tree);
        assert_snapshot!(format!(
            "{}\n\nmissing patterns {{\n  {}\n}}",
            $tree,
            missing
                .into_iter()
                .map(|p| p.fmt(&name_of_var, &name_of_field).to_string())
                .format("\n  ")
        ));
    }};
}

struct Lower {
    tail: Tail,

    ftypes: ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    vtypes: ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,
}

const MAYBE: key::Sum = key::Sum(0);
const LIST: key::Sum = key::Sum(1);
const JUST: key::SumVariant = key::SumVariant(0);
const NONE: key::SumVariant = key::SumVariant(1);

fn m<T>(v: T) -> M<T> {
    key::Module(0).m(v)
}

fn u8() -> Tr<Type> {
    Type::u8().tr(Span::null())
}

impl Lower {
    fn new() -> Self {
        test_logger();

        fn map<K: EntityRef, T, const N: usize>(values: [T; N]) -> Map<K, T> {
            values.into_iter().collect()
        }

        let variants = map([map([
            vec![
                Type::Generic(Generic::new(key::Generic(0), GenericKind::Entity)).tr(Span::null()),
            ],
            vec![],
        ])]);
        let records = map([]);

        let mut ftypes = ModMap::new();
        ftypes.add_module(0);
        ftypes[key::Module(0)] = records;

        let mut vtypes = ModMap::new();
        vtypes.add_module(0);
        vtypes[key::Module(0)] = variants;

        Lower { tail: 0, ftypes, vtypes }
    }
}

fn name_of_var(sum: M<key::Sum>, var: key::SumVariant) -> String {
    assert_eq!(sum.module, key::Module(0));

    match (sum.value, var) {
        (MAYBE, NONE) => "None",
        (MAYBE, JUST) => "Just",
        _ => panic!("unknown sum variant: {sum}:{var}"),
    }
    .to_string()
}
fn name_of_field(record: M<key::Record>, field: key::RecordField) -> String {
    match (record, field) {
        _ => panic!("unknown record field: {record}.{field}"),
    }
}

type Tail = usize;

impl<'a, 's> Merge<'s, Tail> for Lower {
    fn generate_tail(&mut self) -> Tail {
        self.tail
    }

    fn name_of_field(&self, _: M<key::Record>, _: key::RecordField) -> &'s str {
        todo!();
    }

    fn to_init(&self) -> Init<'_> {
        Init::new(&self.ftypes, &self.vtypes)
    }

    fn str_to_ro(&mut self, _: &'s str) -> M<key::ReadOnly> {
        todo!()
    }

    fn pop_inst(&mut self, _: Span) -> Option<mir::func::InstInfo> {
        todo!()
    }
}

impl Lower {
    fn patterns<'s>(&mut self, ty: Type, pats: &[(Tr<hir::Pattern<'s>>, bool)]) -> DecTree<Tail> {
        let mut iter = pats.iter();

        let (pat, _) = iter.next().unwrap();
        let mut tree = self.first(&ty, pat.as_ref());

        for (p, expected) in iter {
            self.tail += 1;
            let old = tree.clone();
            let reachable = self.branch(&mut tree, p.as_ref());

            if *expected != reachable {
                panic!("{p} had expected reachability of {expected}\nbefore merge:\n{old}\nafter merge:\n{tree}");
            } else {
                println!("ok");
            }
        }

        tree
    }
}

#[test]
fn instant_wildcard() {
    let mut lower = Lower::new();

    let tuple = Type::tuple(vec![u8().value, u8().value]);
    let any = hir::Pattern::Any.tr(Span::null());
    let tree = lower.first(&tuple, any.as_ref());
    snapshot_tree_and_missing!(lower, tree);

    let bind = hir::Pattern::Bind(key::Bind(0), Box::new(any.value.clone())).tr(Span::null());
    let tree = lower.first(&tuple, bind.as_ref());
    snapshot_tree_and_missing!(lower, tree);
}

fn just<'s>(p: Tr<hir::Pattern<'s>>) -> Tr<hir::Pattern<'s>> {
    var(MAYBE, JUST, [p])
}
fn none<'s>() -> Tr<hir::Pattern<'s>> {
    var(MAYBE, NONE, [])
}

fn cons<'s>(x: Tr<hir::Pattern<'s>>, xs: Tr<hir::Pattern<'s>>) -> Tr<hir::Pattern<'s>> {
    hir::Pattern::Cons(Box::new([x, xs]), lumina_typesystem::Var(0)).tr(Span::null())
}
fn nil<'s>() -> Tr<hir::Pattern<'s>> {
    hir::Pattern::Nil(lumina_typesystem::Var(0)).tr(Span::null())
}

fn var<'s, const N: usize>(
    key: key::Sum,
    var: key::SumVariant,
    params: [Tr<hir::Pattern<'s>>; N],
) -> Tr<hir::Pattern<'s>> {
    hir::Pattern::Constructor(m(key), var, params.to_vec()).tr(Span::null())
}

fn tuple<'s, const N: usize>(elems: [Tr<hir::Pattern<'s>>; N]) -> Tr<hir::Pattern<'s>> {
    hir::Pattern::Tuple(elems.to_vec()).tr(Span::null())
}

fn bind<'s>(bind: u32, then: Tr<hir::Pattern<'s>>) -> Tr<hir::Pattern<'s>> {
    hir::Pattern::Bind(key::Bind(bind), Box::new(then.value)).tr(Span::null())
}

fn any() -> Tr<hir::Pattern<'static>> {
    hir::Pattern::Any.tr(Span::null())
}

fn r<'s>(range: std::ops::Range<u8>) -> Tr<hir::Pattern<'s>> {
    use lumina_parser::pat::Bound;

    let start = if range.start == u8::MIN {
        Bound::Excess
    } else {
        Bound::Pos(range.start as u128)
    };

    let end = if range.end == u8::MAX {
        Bound::Excess
    } else {
        Bound::Pos(range.end as u128)
    };

    hir::Pattern::Int([start, end], lumina_typesystem::Var(0)).tr(Span::null())
}

fn maybe(ty: Tr<Type>) -> Tr<Type> {
    Type::defined(m(MAYBE), vec![ty.value]).tr(Span::null())
}

fn list(ty: Tr<Type>) -> Tr<Type> {
    Type::list(m::<key::Sum>(LIST.into()), vec![ty.value]).tr(Span::null())
}

fn tuplet<const N: usize>(types: [Tr<Type>; N]) -> Type {
    Type::tuple(types.map(|ty| ty.value).to_vec())
}

#[test]
fn bind_inside_tuple_nested() {
    let mut lower = Lower::new();
    let tree = lower.patterns(
        tuplet([
            u8(),
            tuplet([u8(), u8(), u8()]).tr(Span::null()),
            u8(),
            maybe(u8()),
        ]),
        &[(
            bind(
                0,
                tuple([
                    bind(1, r(0..255)),
                    bind(
                        2,
                        tuple([bind(3, r(0..255)), bind(4, r(0..255)), bind(5, r(0..255))]),
                    ),
                    bind(6, any()),
                    bind(7, any()),
                ]),
            ),
            true,
        )],
    );

    snapshot_tree_and_missing!(lower, tree);
}

#[test]
fn just_x_nothing() {
    let mut lower = Lower::new();
    let tree = lower.patterns(
        maybe(u8()).value,
        &[
            (just(bind(0, any())), true),
            (just(bind(1, any())), false),
            (none(), true),
        ],
    );

    snapshot_tree_and_missing!(lower, tree);
}

#[test]
fn int_matrix() {
    let mut lower = Lower::new();
    let tree = lower.patterns(
        tuplet([u8(), u8(), u8()]),
        &[
            (tuple([r(0..6), r(50..255), r(0..128)]), true),
            (tuple([r(0..6), r(50..200), r(129..255)]), true),
            (tuple([r(2..3), r(50..200), r(0..255)]), false),
            (tuple([r(2..3), r(50..210), r(0..255)]), true),
        ],
    );

    snapshot_tree_and_missing!(lower, tree);
}

#[test]
fn missing_maybes() {
    let mut lower = Lower::new();
    let tree = lower.patterns(
        tuplet([maybe(u8()), maybe(u8())]),
        &[
            (tuple([none(), just(r(0..255))]), true),
            (tuple([just(r(0..255)), just(r(0..255))]), true),
            (tuple([any(), just(r(0..100))]), false),
            (tuple([none(), none()]), true),
            (tuple([none(), any()]), false),
        ],
    );

    snapshot_tree_and_missing!(lower, tree);
}

#[test]
fn list_x_xs() {
    let mut lower = Lower::new();
    let tree = lower.patterns(
        list(u8()).value,
        &[
            (cons(bind(10, any()), bind(20, any())), true),
            (nil(), true),
        ],
    );

    snapshot_tree_and_missing!(lower, tree);
}
