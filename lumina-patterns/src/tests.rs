use crate::DeduplicatedStringCheck;

use super::IfGuard;
use lumina_key as key;
use lumina_typesystem::IntSize;
use std::sync::Once;
use tracing_subscriber::{EnvFilter, layer::SubscriberExt, registry::Registry};

use super::{LIST_CONS, LIST_NIL, TailExpr, reachable};

type Branching<K> = super::Branching<K>;
type DecTree = super::DecTree;

fn tail(n: u32) -> super::DecTree {
    super::DecTree::Tail(TailExpr(n))
}

static LOGGER: Once = Once::new();

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

macro_rules! snapshot_tree {
    ($tree:expr, $missing:expr) => {
        let missing = match $missing {
            None => "".to_string(),
            Some(missing) => missing.to_string(),
        };
        let str = format!("{}\n\ninverted missing tree:\n{}", $tree, missing);
        insta::assert_snapshot!(str);
    };
}

// true
// false
// _
#[test]
fn wildcard_into_bool() {
    logger();

    let mut just_true: DecTree = Branching::bools().bool(true, tail(0)).into();
    let just_false = Branching::bools().bool(false, tail(1)).into();

    let wc = DecTree::Wildcard { next: Box::new(tail(2)) };

    assert_eq!(just_true.merge(just_false), reachable::YES);
    assert_eq!(just_true.merge(wc), reachable::NO);

    let missing = just_true.invert_to_missing();

    snapshot_tree!(just_true, missing);
}

#[test]
fn bool_into_wildcard() {
    logger();

    let mut wc = DecTree::Wildcard { next: Box::new(tail(0)) };

    let just_true = Branching::bools().bool(true, tail(1)).into();
    let just_false = Branching::bools().bool(false, tail(2)).into();

    assert_eq!(wc.merge(just_true), reachable::NO);
    assert_eq!(wc.merge(just_false), reachable::NO);

    let missing = wc.invert_to_missing();

    snapshot_tree!(wc, missing);
}

#[test]
fn simple_ints() {
    logger();

    let u8 = IntSize::new(false, 8);

    dbg!(&u8.max_value());

    let mut num = DecTree::Ints {
        size: u8,
        branching: Branching::ints(u8.min_value() as i128, u8.max_value() as i128)
            .range(0..0, tail(0)),
    };

    let one = DecTree::Ints {
        size: u8,
        branching: Branching::ints(u8.min_value() as i128, u8.max_value() as i128)
            .range(1..1, tail(1)),
    };

    let wc = DecTree::wildcard(tail(2));

    assert_eq!(num.merge(one), reachable::YES);
    assert_eq!(num.merge(wc), reachable::YES);

    let missing = num.invert_to_missing();

    snapshot_tree!(num, missing);
}

#[test]
fn int_matrix() {
    logger();

    let u8 = IntSize::new(false, 8);

    let mut tuple = DecTree::tuple(3, ints(u8, [0..6, 50..255, 0..128], tail(0)));

    let then = DecTree::tuple(3, ints(u8, [0..6, 50..200, 129..255], tail(1)));
    assert_eq!(tuple.merge(then), reachable::YES);

    let then = DecTree::tuple(3, ints(u8, [2..3, 50..200, 0..255], tail(2)));
    assert_eq!(tuple.merge(then), reachable::NO);

    let then = DecTree::tuple(3, ints(u8, [2..3, 50..210, 0..255], tail(3)));
    assert_eq!(tuple.merge(then), reachable::YES);

    let missing = tuple.invert_to_missing();

    snapshot_tree!(tuple, missing);
}

// (None, Some 0..255)
// (Some 0..255, Some 0..255)
// (_, Some 0..100)
// (None, None)
// (None, _)
#[test]
fn missing_maybes() {
    logger();

    let u8 = IntSize::new(false, 8);

    let mut tuple = DecTree::tuple(2, none(some(ints(u8, [0..255], tail(0))).into()).into());

    let then = DecTree::tuple(
        2,
        some(ints(u8, [0..255], some(ints(u8, [0..255], tail(1))).into())).into(),
    );
    assert_eq!(tuple.merge(then), reachable::YES);

    let then = DecTree::tuple(
        2,
        some(DecTree::wildcard(some(ints(u8, [0..100], tail(2))))).into(),
    );
    assert_eq!(tuple.merge(then), reachable::NO);

    let then = DecTree::tuple(2, none(none(tail(3))).into());
    assert_eq!(tuple.merge(then), reachable::YES);

    let then = DecTree::tuple(2, none(some(DecTree::wildcard(tail(3)))).into());
    assert_eq!(tuple.merge(then), reachable::NO);

    let missing = tuple.invert_to_missing();

    snapshot_tree!(tuple, missing);
}

// Some _
// Some _
// None
#[test]
fn just_x_nothing() {
    logger();

    let mut variant = some(DecTree::wildcard(tail(0)));

    let then = some(DecTree::wildcard(tail(1)));
    assert_eq!(variant.merge(then), reachable::NO);

    let then = none(tail(2));
    assert_eq!(variant.merge(then), reachable::YES);

    let missing = variant.invert_to_missing();

    snapshot_tree!(variant, missing);
}

// Cons _ _
// Nil
#[test]
fn list_x_xs() {
    logger();

    let mut list: DecTree = Branching::list()
        .variant(LIST_CONS, DecTree::wildcard(DecTree::wildcard(tail(0))))
        .into_list_dectree();

    let then = Branching::list()
        .variant(LIST_NIL, tail(1))
        .into_list_dectree();
    assert_eq!(list.merge(then), reachable::YES);

    let missing = list.invert_to_missing();

    snapshot_tree!(list, missing);
}

// Cons _ _
// Cons (Cons () Nil) Nil // unreachable
// _
#[test]
fn nested_lists() {
    logger();

    let mut list = Branching::list()
        .variant(LIST_CONS, DecTree::wildcard(DecTree::wildcard(tail(0))))
        .into_list_dectree();

    let then = Branching::list()
        .variant(
            LIST_CONS,
            Branching::list()
                .variant(
                    LIST_CONS,
                    DecTree::tuple(
                        0,
                        Branching::list()
                            .variant(
                                LIST_NIL,
                                Branching::list()
                                    .variant(LIST_NIL, tail(1))
                                    .into_list_dectree(),
                            )
                            .into_list_dectree(),
                    ),
                )
                .into_list_dectree(),
        )
        .into_list_dectree();
    assert_eq!(list.merge(then), reachable::NO);

    let then = DecTree::wildcard(tail(2));
    assert_eq!(list.merge(then), reachable::YES);

    let missing = list.invert_to_missing();

    snapshot_tree!(list, missing);
}

// match x {
//   0 if false => 0
//   0 => 1
//   1.. => 2
// }
#[test]
fn if_guards() {
    logger();

    let u8 = IntSize::new(false, 8);

    let mut if_guarded = ints(
        u8,
        [0..0],
        DecTree::IfGuarded {
            guard: IfGuard(0),
            true_next: Box::new(tail(0)),
            false_next: Box::new(DecTree::Unreached),
        },
    );

    let then = ints(u8, [0..0], tail(1));
    assert_eq!(if_guarded.merge(then), reachable::YES);

    let then = ints(u8, [1..u8.max_value() as i128], tail(2));
    assert_eq!(if_guarded.merge(then), reachable::YES);

    let missing = if_guarded.invert_to_missing();

    snapshot_tree!(if_guarded, missing);
}

#[test]
fn strings() {
    logger();

    let strings = [0, 1, 2, 3, 4].map(DeduplicatedStringCheck);

    let mut pat: DecTree = Branching::strings().string(strings[0], tail(0)).into();

    let then = Branching::strings().string(strings[1], tail(1)).into();
    assert_eq!(pat.merge(then), reachable::YES);

    let then = Branching::strings().string(strings[0], tail(2)).into();
    assert_eq!(pat.merge(then), reachable::NO);

    let missing = pat.invert_to_missing();

    snapshot_tree!(pat, missing);
}

fn option() -> Branching<key::Variant> {
    let vars = [(NONE, 0), (SOME, 1)];
    Branching::variants(vars)
}

fn none(next: DecTree) -> DecTree {
    option().variant(NONE, next).into()
}
fn some(next: DecTree) -> DecTree {
    option().variant(SOME, next).into()
}

const NONE: key::Variant = key::Variant(0);
const SOME: key::Variant = key::Variant(1);

fn ints<const N: usize>(
    size: IntSize,
    chain: [std::ops::Range<i128>; N],
    next: DecTree,
) -> DecTree {
    let (min, max) = (size.min_value() as i128, size.max_value() as i128);

    chain.into_iter().rev().fold(next, |next, range| {
        Branching::ints(min, max)
            .range(range, next)
            .into_ints_dectree(size)
    })
}
