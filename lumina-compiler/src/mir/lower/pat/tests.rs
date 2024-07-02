use super::{
    BranchOf, DecTree, DecTreeBranch, Expr, Formatter, IsReachable, ListConstr, Merger,
    MissingGeneration, ModMap, Params, PatPoint, PointToBindTranslationTable, Range, Tr, Type,
    REACHABLE, UNREACHABLE,
};
use crate::prelude::Map;
use ibig::IBig;
use itertools::Itertools;
use lumina_key as key;
use lumina_typesystem::{Bitsize, Prim};
use tracing::info;

// TODO: let's just construct dectrees directly instead. No HIR stuff

fn patmerger() -> Merger<'static> {
    let vtypes = Box::leak(Box::new(ModMap::new()));
    Merger { vtypes }
}

fn formatter() -> Formatter<'static, 'static> {
    let fnames = Box::leak(Box::new(ModMap::new()));
    let vnames = Box::leak(Box::new(ModMap::new()));
    Formatter { fnames, vnames }
}

fn missing() -> MissingGeneration<'static> {
    let vtypes = Box::leak(Box::new(ModMap::new()));
    MissingGeneration { vtypes }
}

const CONS: key::SumVariant = key::SumVariant(0);
const NIL: key::SumVariant = key::SumVariant(1);
const LIST: key::Sum = key::Sum(0);

macro_rules! tree {
    ($init:expr, $($tree:expr => $reachable:expr),*) => {
        merge_trees([($init, REACHABLE), $(($tree, $reachable)),*])
    };
    (exhaustive ; $init:expr, $($tree:expr => $reachable:expr),*) => {
        let tree = tree!($init, $($tree => $reachable),*);

        let missing = missing().invert(&tree, 1);
        if let Some(missing) = missing {
            let fmtpatterns = formatter().tree_to_fmt_patterns(&missing);
            panic!("missing patterns:\n  {}", fmtpatterns.iter().format("\n  "));
        }
    };
    (missing: [$($name:literal),*] ; $init:expr, $($tree:expr => $reachable:expr),*) => {
        let tree = tree!($init, $($tree => $reachable),*);
        let missing = missing().invert(&tree, 1).map(|m| formatter().tree_to_fmt_patterns(&m));

        let expected = [$($name),*];

        let mut ok = false;

        if let Some(missing) = &missing {
            ok = missing.len() == expected.len();

            for m in missing {
                let str = m.to_string();
                if expected.iter().all(|e| str.as_str() != *e) {
                    ok = false;
                }
            }
        }

        if !ok {
            panic!(
               "expected a different set of missing patterns\nexpected:\n  {}\ngot:\n  {}",
               expected.iter().format("\n  "),
               missing.unwrap_or(vec![]).iter().format("\n  "),
            );
        }
    }
}

#[test]
fn int_tuples() {
    tree! { exhaustive ;
        tuple(2, range(0..100, range(0..50, tail(0)))),
        tuple(2, range(0..100, range(50..100, tail(1)))) => REACHABLE,
        tuple(2, range(0..100, range(0..100, tail(2)))) => UNREACHABLE
    };

    tree! {
        missing: ["(.., _)"] ;
        tuple(2, range(0..100, range(1..50, tail(0)))),
        tuple(2, range(0..100, range(50..100, tail(1)))) => REACHABLE,
        tuple(2, range(0..100, range(1..100, tail(2)))) => UNREACHABLE
    };

    tree! {
        missing: ["(_, _)"] ;
        tuple(2, range(4..8, range(50..100, tail(0)))),
        tuple(2, range(4..6, range(0..100, tail(1)))) => REACHABLE,
        tuple(2, range(4..7, range(0..100, tail(2)))) => REACHABLE
    };
}

#[test]
fn lists() {
    tree! { exhaustive ;
        cons(wildcard(wildcard(tail(0)))), // [x : xs]
        nil(tail(1)) => REACHABLE, // []
        wildcard(tail(2)) => UNREACHABLE // _
    };

    tree! { missing: ["[_ : _]"] ;
        cons(wildcard(nil(tail(0)))), // [x]
        nil(tail(1)) => REACHABLE // []
    };
}

#[test]
fn wildcards() {
    tree! { exhaustive ;
        tuple(2, wildcard(wildcard(tail(0)))),
        wildcard(tail(1)) => UNREACHABLE
    };

    tree! { missing: ["(_, _)"] ;
        tuple(2, range(0..50, wildcard(tail(0)))),
    };

    tree! { exhaustive ;
        wildcard(tail(0)),
    };
}

fn dump_patterns(tree: &DecTree) -> impl std::fmt::Display {
    formatter()
        .tree_to_fmt_patterns(tree)
        .into_iter()
        .format("\n")
}

#[track_caller]
fn merge_trees<const N: usize>(trees: [(DecTree, IsReachable); N]) -> DecTree {
    let mut iter = trees.into_iter();
    let (mut init, _) = iter.next().unwrap();

    println!("init pattern: {}", dump_patterns(&init));

    let mut merger = patmerger();
    for (tree, reachable) in iter {
        let untouched = init.clone();

        let ok = merger.merge(&mut init, tree.clone()) == reachable;
        println!(
            "\nmerging {} into\n{}\nturned into\n{}\n",
            dump_patterns(&tree),
            dump_patterns(&untouched),
            dump_patterns(&init)
        );

        if !ok {
            let pat = formatter().tree_to_fmt_patterns(&tree).pop().unwrap();

            panic!(
                "{pat} was expected to be {}",
                if reachable {
                    "reachable"
                } else {
                    "unreachable"
                },
            );
        }
    }

    init
}

fn int(n: u8, then: DecTree) -> DecTree {
    range(n..n, then)
}

fn range(r: std::ops::Range<u8>, mut then: DecTree) -> DecTree {
    then.offset_binds(1);
    entry(DecTreeBranch::Ints(
        Bitsize(8),
        IBig::from(0),
        IBig::from(100),
        BranchOf::one(
            Range { start: IBig::from(r.start), end: IBig::from(r.end) },
            0,
            then,
        ),
    ))
}

fn wildcard(mut then: DecTree) -> DecTree {
    then.offset_binds(1);
    entry(DecTreeBranch::Wildcard(Box::new(then)))
}

fn tuple(params: Params, mut then: DecTree) -> DecTree {
    then.offset_binds(1);
    entry(DecTreeBranch::Tuple(params, Box::new(then)))
}

fn nil(mut then: DecTree) -> DecTree {
    then.offset_binds(1);
    entry(DecTreeBranch::List(
        key::Module(0).m(key::TypeKind::Sum(LIST)),
        Type::Prim(Prim::Int(true, Bitsize::default())),
        BranchOf { branches: vec![(ListConstr::Nil, 0, then)] },
    ))
}

fn cons(mut then: DecTree) -> DecTree {
    then.offset_binds(1);
    entry(DecTreeBranch::List(
        key::Module(0).m(key::TypeKind::Sum(LIST)),
        Type::Prim(Prim::Int(true, Bitsize::default())),
        BranchOf { branches: vec![(ListConstr::Cons, 2, then)] },
    ))
}

fn entry(branch: DecTreeBranch) -> DecTree {
    DecTree { branch, point: PatPoint(0) }
}

fn tail(n: i32) -> DecTree {
    entry(DecTreeBranch::Reached(
        PointToBindTranslationTable { map: vec![] },
        Box::new(Expr::UInt(Bitsize::default(), n as u128)),
    ))
}
