use super::{
    BranchOf, DecTree, DecTreeBranch, Expr, ListConstr, Params, PatPoint,
    PointToBindTranslationTable, Range, Type,
};
use derive_new::new;
use ibig::IBig;
use itertools::Itertools;
use lumina_key as key;
use lumina_key::{Map, ModMap};
use lumina_util::{ParamFmt, Tr};
use std::cell::RefCell;
use std::fmt;
use std::ops::Not;
use std::rc::Rc;

#[derive(Clone)]
pub enum FmtPattern {
    Wildcard,
    Ident(String, Vec<Self>),
    Range(IBig, IBig, Range),
    Record(Vec<(String, Self)>),
    Tuple(Vec<Self>),
    EmptyList,
    ElemList,
    Poison,
}

impl fmt::Display for FmtPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FmtPattern::Record(fields) => write!(
                f,
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(name, value)| format!("{name} = {value}"))
                    .format(", ))")
            ),
            FmtPattern::EmptyList => "[]".fmt(f),
            FmtPattern::ElemList => "[_ : _]".fmt(f),
            FmtPattern::Poison => "???".fmt(f),
            FmtPattern::Tuple(elems) => write!(f, "({})", elems.iter().format(", ")),
            FmtPattern::Wildcard => "_".fmt(f),
            FmtPattern::Ident(name, params) => ParamFmt::new(name, params).fmt(f),
            FmtPattern::Range(_, _, r) if r.start == r.end => r.start.fmt(f),
            FmtPattern::Range(min, max, r) => {
                if r.start != *min {
                    r.start.fmt(f)?;
                }
                write!(f, "..")?;
                if r.end != *max {
                    r.end.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(new)]
pub struct Formatter<'a, 's> {
    pub fnames: &'a ModMap<key::Record, Map<key::RecordField, Tr<&'s str>>>,
    pub vnames: &'a ModMap<key::Sum, Map<key::SumVariant, Tr<&'s str>>>,
}

impl<'a, 's> Formatter<'a, 's> {
    pub fn tree_to_fmt_patterns(&self, tree: &DecTree) -> Vec<FmtPattern> {
        let state = Rc::new(RefCell::new(vec![]));
        self.tree(tree, vec![State::Collect(state.clone())]);
        state.take()
    }

    fn tree<'f>(&self, tree: &DecTree, mut states: Vec<State<'f>>) {
        match &tree.branch {
            DecTreeBranch::Ints(_, min, max, vars) => {
                for (v, _, next) in &vars.branches {
                    let constr = &|_| FmtPattern::Range(min.clone(), max.clone(), v.clone());
                    self.of_next(next, 0, constr, states.clone())
                }
            }
            DecTreeBranch::Bools(vars) => {
                for (v, _, next) in &vars.branches {
                    let constr = &|_| FmtPattern::Ident(v.to_string(), vec![]);
                    self.of_next(next, 0, constr, states.clone())
                }
            }
            DecTreeBranch::Sum(inst, _, key, vars) => {
                for (v, _, next) in &vars.branches {
                    let name = self.vnames[*key][*v];
                    let constr = &|_| FmtPattern::Ident(name.to_string(), vec![]);
                    self.of_next(next, 0, constr, states.clone())
                }
            }
            DecTreeBranch::List(key, inner, vars) => {
                let vars = vars.branches.iter();

                for (constr, _, next) in vars {
                    match constr {
                        ListConstr::Cons => {
                            self.of_next(next, 2, &|_| FmtPattern::ElemList, states.clone());
                        }
                        ListConstr::Nil => {
                            let mut states = states.clone();
                            self.consume(FmtPattern::EmptyList, &mut states);
                            self.tree(next, states);
                        }
                    }
                }
            }
            DecTreeBranch::Tuple(params, next) => {
                self.of_next(&*next, *params, &FmtPattern::Tuple, states)
            }
            DecTreeBranch::Record(key, _, params, next) => self.of_next(
                &*next,
                *params,
                &|fields| {
                    FmtPattern::Record(
                        self.fnames[*key]
                            .values()
                            .map(|name| name.to_string())
                            .zip(fields)
                            .collect(),
                    )
                },
                states,
            ),
            DecTreeBranch::Wildcard(next) => {
                self.consume(FmtPattern::Wildcard, &mut states);
                self.tree(next, states)
            }
            DecTreeBranch::Poison => {}
            DecTreeBranch::Reached(_, _) => {
                assert_eq!(states.len(), 1, "unfinished pattern constructors")
            }
        }
    }

    fn of_next<'f>(
        &self,
        next: &DecTree,
        params: Params,
        constr: &'f dyn Fn(Vec<FmtPattern>) -> FmtPattern,
        mut states: Vec<State<'f>>,
    ) {
        if params == 0 {
            let pat = constr(vec![]);
            self.consume(pat, &mut states);
            self.tree(next, states);
        } else {
            states.push(State::Params(params, vec![], constr));
            self.tree(next, states)
        }
    }

    fn consume<'f>(&self, pat: FmtPattern, states: &mut Vec<State<'f>>) {
        match states.last_mut().unwrap() {
            State::Params(params, buf, constr) => {
                *params -= 1;
                buf.push(pat);

                if *params == 0 {
                    let pat = constr(std::mem::take(buf));
                    states.pop();
                    self.consume(pat, states);
                }
            }
            State::Collect(missing) => missing.borrow_mut().push(pat),
        }
    }
}

#[derive(Clone)]
pub enum State<'a> {
    Params(
        Params,
        Vec<FmtPattern>,
        &'a dyn Fn(Vec<FmtPattern>) -> FmtPattern,
    ),

    // patterns can fork into multiple branches, and when that happens we clone the state stack
    //
    // but we still need this buffer to remain the same.
    Collect(Rc<RefCell<Vec<FmtPattern>>>),
}

pub struct MissingGeneration<'a> {
    pub vtypes: &'a ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,
}

impl<'a> MissingGeneration<'a> {
    pub fn invert(&mut self, tree: &DecTree, p: Params) -> Option<DecTree> {
        match &tree.branch {
            DecTreeBranch::Ints(bitsize, min, max, ints) => {
                let total = ints.branches.iter().fold(IBig::from(0), |acc, (n, _, _)| {
                    acc + n.end.clone() - n.start.clone() + 1
                }) - 1;

                let ranges_are_exhaustive = total == max.clone() - min.clone();

                if ranges_are_exhaustive {
                    let missing = ints
                        .branches
                        .iter()
                        .filter_map(|(int, _, next)| {
                            self.invert(next, p - 1).map(|next| (int.clone(), 0, next))
                        })
                        .collect::<Vec<_>>();

                    missing.is_empty().not().then(|| {
                        DecTreeBranch::Ints(
                            *bitsize,
                            min.clone(),
                            max.clone(),
                            BranchOf { branches: missing },
                        )
                    })
                } else {
                    Some(DecTreeBranch::Wildcard(Box::new(params_to_extree(
                        p - 1,
                        tail(),
                    ))))
                }
            }
            DecTreeBranch::List(key, inner, variants) => self.invert_branches(
                variants,
                p,
                [ListConstr::Cons, ListConstr::Nil],
                |_, var| if *var == ListConstr::Nil { 0 } else { 2 },
                |next| DecTreeBranch::List(*key, inner.clone(), next),
                PartialEq::eq,
            ),
            DecTreeBranch::Tuple(params, next) => self
                .invert(next, p + *params - 1)
                .map(Box::new)
                .map(|next| DecTreeBranch::Tuple(*params, next)),
            DecTreeBranch::Record(key, ptypes, params, next) => self
                .invert(next, p + *params - 1)
                .map(Box::new)
                .map(|next| DecTreeBranch::Record(*key, ptypes.clone(), *params, next)),
            DecTreeBranch::Bools(variants) => {
                let con = DecTreeBranch::Bools;
                self.invert_branches(variants, p, [false, true], |_, _| 0, con, PartialEq::eq)
            }
            DecTreeBranch::Sum(inst, ctypes, key, variants) => {
                let all_variants = self.vtypes[*key].keys();
                self.invert_branches(
                    variants,
                    p,
                    all_variants,
                    |this, var| this.vtypes[*key][*var].len(),
                    |next| DecTreeBranch::Sum(inst.clone(), ctypes.clone(), *key, next),
                    PartialEq::eq,
                )
            }
            DecTreeBranch::Reached(_, _) => None,
            DecTreeBranch::Wildcard(next) => self
                .invert(next, p - 1)
                .map(Box::new)
                .map(DecTreeBranch::Wildcard),
            DecTreeBranch::Poison => None,
        }
        .map(|branch| DecTree { point: PatPoint(0), branch })
    }

    fn invert_branches<T>(
        &mut self,
        variants: &BranchOf<T>,
        params: Params,
        all: impl IntoIterator<Item = T>,
        param_of: impl Fn(&Self, &T) -> Params,
        con: impl FnOnce(BranchOf<T>) -> DecTreeBranch,
        cmp: impl Fn(&T, &T) -> bool,
    ) -> Option<DecTreeBranch> {
        let missing = all
            .into_iter()
            .filter_map(|var| {
                let p = param_of(self, &var);
                variants
                    .branches
                    .iter()
                    .find(|(o, _, _)| cmp(&var, o))
                    .map(|(_, _, next)| self.invert(next, params + p - 1))
                    .unwrap_or_else(|| Some(params_to_extree(params + p - 1, tail())))
                    .map(|next| (var, params, next))
            })
            .collect::<Vec<_>>();

        missing
            .is_empty()
            .not()
            .then(|| con(BranchOf { branches: missing }))
    }
}

fn tail() -> DecTreeBranch {
    DecTreeBranch::Reached(
        PointToBindTranslationTable { map: vec![] },
        Box::new(Expr::Poison),
    )
}

fn params_to_extree(params: Params, tail: DecTreeBranch) -> DecTree {
    DecTree {
        point: PatPoint(0),
        branch: if params == 0 {
            tail
        } else {
            DecTreeBranch::Wildcard(Box::new(params_to_extree(params - 1, tail)))
        },
    }
}
