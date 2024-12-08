use super::*;
use crate::LISTABLE_SPLIT;
use lumina_typesystem::{Container, GenericMapper, IntSize, Transformer};
use mir::pat::{DecTree, Range, StrCheck, StrChecks, TreeTail};
use ssa::{Block, Value};
use std::collections::VecDeque;

impl<'a> FuncLower<'a> {
    pub fn to_pat_lower<'f, 'v>(
        &'f mut self,
        branches: &'v Map<key::DecisionTreeTail, mir::Expr>,
        pred: &'v Map<key::DecisionTreeTail, u16>,
    ) -> PatLower<'f, 'v, 'a> {
        PatLower {
            continuation_block: None,
            continuation_value: None,

            expressions: branches.keys().map(|_| None).collect(),

            branches,
            predecessors: pred,
            visits: vec![0; pred.len()].into(),

            f: self,

            constructors: vec![],

            map: vec![],

            can_skip_continuation: true,
        }
    }
}

pub struct PatLower<'f, 'v, 'a> {
    f: &'f mut FuncLower<'a>,

    // All branches put their yielded value as a parameter to this block and jump to it
    //
    // This desugars `f (match ...)`
    continuation_block: Option<(ssa::Block, MonoType)>,
    continuation_value: Option<ssa::Value>,

    branches: &'v Map<key::DecisionTreeTail, mir::Expr>,
    predecessors: &'v Map<key::DecisionTreeTail, u16>,
    visits: Map<key::DecisionTreeTail, u16>,
    expressions: Map<key::DecisionTreeTail, Option<Block>>,

    constructors: Vec<VecDeque<Value>>,
    map: Vec<ssa::Value>,

    can_skip_continuation: bool,
}

impl<'f, 'v, 'a> PatLower<'f, 'v, 'a> {
    fn ssa(&mut self) -> &mut ssa::SSA {
        self.f.ssa()
    }

    fn block(&self) -> Block {
        self.f.lir.functions[self.f.current.mfkey].ssa.block()
    }

    pub fn run(mut self, on: ssa::Value, tree: &mir::DecTree) -> Value {
        self.tree(on, tree);

        if self.can_skip_continuation {
            assert_eq!(self.continuation_block, None);
            self.continuation_value.unwrap()
        } else {
            assert_eq!(self.continuation_value, None);
            let (block, con_ty) = self.continuation_block.take().unwrap();
            self.ssa().switch_to_block(block);
            self.ssa().add_block_param(block, con_ty);
            self.ssa().get_block_param(block, 0).value()
        }
    }

    fn make_reset(&self) -> ResetPoint {
        ResetPoint {
            constructors: self.constructors.clone(),
            map: self.map.clone(),
        }
    }
    fn reset(&mut self, block: Block, point: ResetPoint) {
        self.ssa().switch_to_block(block);
        self.map = point.map;
        self.constructors = point.constructors;
    }

    fn tree(&mut self, on: ssa::Value, tree: &mir::DecTree) {
        self.map.push(on);

        match tree {
            DecTree::Record { next, .. } => self.record(on, next),
            DecTree::Tuple { next, .. } => self.tuple(on, next),
            DecTree::Array { next, .. } => self.array(on, next),
            DecTree::List { next, ty } => self.list(on, ty, next),
            DecTree::Ints { intsize, next } => self.ints(on, *intsize, next),
            DecTree::Bools(next) => self.bools(on, next),
            DecTree::Sum { sum, params, next } => self.sum(on, *sum, params, next),
            DecTree::String { next, wildcard_next } => self.string(on, next, wildcard_next),
            DecTree::Wildcard { next, .. } | DecTree::Opaque { next, .. } => self.next(next),
            DecTree::End(tail) => self.tail(tail),
        }
    }

    fn tail(&mut self, tail: &TreeTail<key::DecisionTreeTail>) {
        match tail {
            TreeTail::Poison => {}
            TreeTail::Unreached(_) => {}
            TreeTail::Reached(table, _excess, tail) => {
                let branch_expr_block = match &mut self.expressions[*tail] {
                    Some(existing) => existing,
                    None => {
                        let branch_expr_block = self.ssa().new_block();
                        self.expressions[*tail] = Some(branch_expr_block);
                        self.expressions[*tail].as_mut().unwrap()
                    }
                };
                let branch_expr_block = *branch_expr_block;

                let branch_expr_params = table
                    .binds
                    .iter()
                    .map(|(_, depth)| self.map[*depth])
                    .collect();

                self.ssa().jump(branch_expr_block, branch_expr_params);

                self.lower_expr_branch_if_last_predecessor(*tail, table);
            }
        }
    }

    fn lower_expr_branch_if_last_predecessor(
        &mut self,
        tail: key::DecisionTreeTail,
        table: &mir::pat::PointTable,
    ) {
        self.visits[tail] += 1;

        let branch_expr_block = self.expressions[tail].unwrap();

        // If this is the last predecessor then jump to and lower the branch expr
        if self.visits[tail] == self.predecessors[tail] {
            for (bind, depth) in table.binds.iter() {
                let v = self.map[*depth];
                let ty = self.f.type_of_value(v);
                let bparam = self.ssa().add_block_param(branch_expr_block, ty);
                self.f.current.bindmap.insert(*bind, bparam.value());
            }

            self.ssa().switch_to_block(branch_expr_block);
            let expr = &self.branches[tail];
            let v = self.f.expr_to_value(expr);

            if self.can_skip_continuation {
                self.continuation_value = Some(v);
            } else {
                let ty = self.f.type_of_value(v);
                let con = self.get_continuation(ty);
                self.ssa().jump(con, vec![v]);
            }
        }
    }

    fn next(&mut self, tree: &mir::DecTree) {
        match self.constructors.last_mut() {
            Some(params) => match params.pop_front() {
                Some(v) => self.tree(v, tree),
                None => {
                    self.constructors.pop();
                    self.next(tree)
                }
            },
            None => match &tree {
                DecTree::End(tail) => self.tail(tail),
                other => unreachable!("misaligned constructor ordering:\n{other}"),
            },
        }
    }

    fn ints(&mut self, on: ssa::Value, intsize: IntSize, next: &mir::Branching<Range>) {
        self.can_skip_continuation &= next.branches.len() == 1;

        let resetpoint = self.make_reset();

        for (range, next) in &next.branches {
            if range.end == range.con.max {
                return self.next(next);
            }

            let [on_true, on_false] = [self.ssa().new_block(), self.ssa().new_block()];

            let check = if range.end == range.start {
                // TODO: jump-table optimisation for adjecent single-numbers
                self.ssa().eq([on, Value::Int(range.end, intsize)], intsize)
            } else {
                let mut check = self
                    .ssa()
                    .lti([on, Value::Int(range.end, intsize)], intsize);
                if range.con.min != range.start {
                    let high_enough = self
                        .ssa()
                        .gti([on, Value::Int(range.start, intsize)], intsize);
                    check = self.ssa().bit_and([check, high_enough], MonoType::bool());
                }
                check
            };

            self.ssa()
                .select(check, [(on_true, vec![]), (on_false, vec![])]);

            self.ssa().switch_to_block(on_true);
            self.next(&next);

            self.reset(on_false, resetpoint.clone());
        }
    }

    fn tuple(&mut self, on: Value, next: &mir::DecTree) {
        let mk = self.f.type_of_value(on).as_key();

        let constructor = self.f.types()[mk]
            .as_record()
            .keys()
            .map(|field| {
                let ty = self.f.types()[mk].as_record()[field].clone();
                self.ssa().field(on, mk, field, ty)
            })
            .collect();

        self.constructors.push(constructor);

        self.next(next)
    }

    fn array(&mut self, on: Value, next: &mir::DecTree) {
        let (len, ity) = self.f.type_of_value(on).as_array();

        let size_t = IntSize::new(false, self.f.lir.target.int_size());

        let constructor = (0..len as usize)
            .map(|i| Value::Int(i as i128, size_t))
            .map(|i| self.ssa().indice(on, i, ity.clone()))
            .collect();

        self.constructors.push(constructor);

        self.next(next)
    }

    fn bools(&mut self, on: Value, v: &mir::Branching<bool>) {
        self.can_skip_continuation = false;

        let [fst, snd] = v.branches.as_slice() else {
            panic!("incorrect bool count");
        };

        let [truthy, falsey] = [
            fst.0.then_some(fst).unwrap_or(snd),
            fst.0.then_some(snd).unwrap_or(fst),
        ];

        let resetpoint = self.make_reset();

        let [on_true, on_false] = [self.ssa().new_block(), self.ssa().new_block()];

        self.ssa()
            .select(on, [(on_true, vec![]), (on_false, vec![])]);

        self.ssa().switch_to_block(on_true);
        self.next(&truthy.1);

        self.reset(on_false, resetpoint);
        self.next(&falsey.1);
    }

    fn is_just(&mut self, maybe: Value) -> Value {
        let maybe_mk = self.f.type_of_value(maybe).as_key();

        let (tagsize, _, _) = self.f.types()[maybe_mk].as_sum();
        let tag = self.ssa().tag_of(maybe, tagsize);

        self.ssa().eq([tag, Value::maybe_just()], tagsize)
    }

    fn list(&mut self, on: Value, ty: &Type, vars: &SumBranches) {
        self.can_skip_continuation = false;

        let oblock = self.block();

        let mut morph = to_morphization!(self.f.lir, self.f.mir, &mut self.f.current.tmap);
        let listmt = morph.apply(&ty);
        let list = morph.apply_weak(&ty);
        let (_, inner) = match &ty {
            Type::Container(Container::Defined(kind, _), params) => {
                let inner = params[0].clone();
                assert_eq!(params.len(), 1);
                (kind, inner)
            }
            _ => unreachable!(),
        };

        let innermt = morph.apply(&inner);
        let inner = morph.apply_weak(&inner);

        let (ikey, tmap) = self.f.find_implementation(
            self.f.info.listable,
            &[inner.clone()],
            list.clone(),
            listmt.clone(),
        );

        let split = Item::Method(ikey, LISTABLE_SPLIT);
        let split = self.f.call_to_mfunc(split, tmap);
        let ret = self.f.lir.functions[split].returns.clone();

        let maybe = self.ssa().call(split, vec![on], ret);

        let is_just = self.is_just(maybe);
        let lvars = [mir::pat::LIST_CONS, mir::pat::LIST_NIL];

        let [con_block, nil_block] = lvars.map(|_| self.ssa().new_block());

        self.ssa()
            .select(is_just, [(con_block, vec![]), (nil_block, vec![])]);

        for (vblock, constr) in [con_block, nil_block].into_iter().zip(lvars) {
            self.ssa().switch_to_block(vblock);

            let resetpoint = self.make_reset();

            let mut vparams = VecDeque::new();

            // Add parameters matching the MIR pattern of `Cons x xs`
            if constr == mir::pat::LIST_CONS {
                let x_xs_ty = self
                    .f
                    .lir
                    .mono
                    .get_or_make_tuple(vec![innermt.clone(), listmt.clone()]);

                let params = self.ssa().cast_payload(maybe, MonoType::from(x_xs_ty));

                let x = self
                    .ssa()
                    .field(params, x_xs_ty, key::Field(0), innermt.clone());

                let xs = self
                    .ssa()
                    .field(params, x_xs_ty, key::Field(1), listmt.clone());

                vparams.push_back(x);
                vparams.push_back(xs);
            }

            self.constructors.push(vparams);

            let next = vars
                .branches
                .iter()
                .find_map(|(con, n)| (*con == constr).then_some(n))
                .unwrap();

            self.next(next);
            self.reset(oblock, resetpoint);
        }
    }

    fn record(&mut self, on: Value, next: &mir::DecTree) {
        let mk = self.f.type_of_value(on).as_key();

        let constructor = self.f.types()[mk]
            .as_record()
            .keys()
            .map(|field| {
                let ty = self.f.types()[mk].as_record()[field].clone();
                self.ssa().field(on, mk, field, ty)
            })
            .collect();

        self.constructors.push(constructor);

        self.next(next);
    }

    fn string(
        &mut self,
        on: ssa::Value,
        next: &mir::Branching<StrChecks>,
        wc_next: &DecTree<key::DecisionTreeTail>,
    ) {
        // The folding of the strcheck's will map the values instead. So let's undo the generalised
        // one from `tree`.
        self.map.pop();

        self.can_skip_continuation = false;

        let reset = self.make_reset();

        let mut falsely;

        for (str, next) in &next.branches {
            falsely = self.ssa().new_block();
            self.string_branch((on, falsely), (&str.checks, next));
            self.reset(falsely, reset.clone());
        }

        // Undo the edge-case of removing an extra bind for this case
        self.map.push(on);
        self.next(wc_next);
    }

    fn string_branch(
        &mut self,
        (on, falsely): (ssa::Value, Block),
        (checks, next): (&[StrCheck], &DecTree<key::DecisionTreeTail>),
    ) {
        // TODO: we should perform a single `len` check and then re-use it
        //
        // let len = Option<Value>;

        assert_ne!(checks.len(), 0);

        checks.iter().enumerate().fold(on, |mut on, (i, check)| {
            let is_last = i == checks.len() - 1;

            match check {
                StrCheck::Literal(key) => {
                    let (str, slen_arg, _) = self.f.string_from_ro(*key);
                    self.map.push(str);

                    let eq = if is_last {
                        self.f.string_equals([on, str])
                    } else {
                        let [lhs, rhs] = self.f.string_split_at(on, slen_arg);
                        on = rhs;
                        self.f.string_equals([lhs, str])
                    };

                    let next_check_block = self.ssa().new_block();

                    self.ssa()
                        .select(eq, [(next_check_block, vec![]), (falsely, vec![])]);

                    self.ssa().switch_to_block(next_check_block);

                    on
                }
                StrCheck::TakeExcess => {
                    assert!(is_last);
                    self.map.push(on);
                    on
                }
                StrCheck::TakeByte => {
                    let [x, xs] = self.f.string_split_first(on);
                    self.map.push(x);

                    let u8 = IntSize::new(false, 8);
                    let null = Value::Int(0, u8);
                    let is_null = self.ssa().eq([x, null], u8);
                    let ok = self.ssa().not(is_null);

                    let next_check_block = self.ssa().new_block();

                    self.ssa()
                        .select(ok, [(next_check_block, vec![]), (falsely, vec![])]);

                    self.ssa().switch_to_block(next_check_block);

                    xs
                }
                StrCheck::TakeWhile(call, params) => {
                    let params = self.f.params_to_values(params);
                    let closure = self.f.pass(call, params);

                    let [lhs, rhs] = self.f.string_split_while(on, closure);
                    self.map.push(lhs);

                    rhs
                }
                StrCheck::TakeBySplit(call, _, params) => {
                    let params = self.f.params_to_values(params);
                    let closure = self.f.pass(call, params);
                    let objty = self.f.type_of_value(closure).as_key();

                    let maybe = self.f.call_closure(objty, closure, vec![on]);

                    let is_just = self.is_just(maybe);

                    let next_check_block = self.ssa().new_block();

                    self.ssa()
                        .select(is_just, [(next_check_block, vec![]), (falsely, vec![])]);

                    self.ssa().switch_to_block(next_check_block);

                    let string = self.f.string_type();
                    let tuple_ty = self
                        .f
                        .lir
                        .mono
                        .get_or_make_tuple(vec![string.into(), string.into()]);

                    let tuple_ty_ty = self.f.lir.mono.get_or_make_tuple(vec![tuple_ty.into()]);

                    let tuple = self.ssa().cast_payload(maybe, tuple_ty_ty.into());

                    let [x, xs] = [0, 1]
                        .map(key::Field)
                        .map(|field| self.ssa().field(tuple, tuple_ty, field, string.into()));

                    self.map.push(x);

                    xs
                }
            }
        });

        // And now the continuation for the case of all string checks being successfull
        self.next(next);
    }

    fn sum(&mut self, on: Value, sum: M<key::Sum>, params: &[Type], v: &SumBranches) {
        self.can_skip_continuation &= v.branches.len() == 1;

        let oblock = self.block();
        let on_mk = self.f.type_of_value(on).as_key();

        let (tag_size, _, _) = self.f.types()[on_mk].as_sum();
        let tag = self.ssa().tag_of(on, tag_size);

        assert!(
            v.branches
                .windows(2)
                .all(|branch| branch[0].0 .0 == branch[1].0 .0 - 1),
            "sum variants in decision tree are meant to be sorted"
        );

        let jmp_table_blocks = v
            .branches
            .iter()
            .map(|(..)| self.ssa().new_block())
            .collect::<Vec<_>>();

        self.ssa().jump_table(tag, jmp_table_blocks.clone());

        for (vblock, (var, next)) in jmp_table_blocks.into_iter().zip(&v.branches) {
            self.ssa().switch_to_block(vblock);

            let resetpoint = self.make_reset();

            let finst = GenericMapper::from_types(GenericKind::Entity, params.iter().cloned());
            let raw_var_types = &self.f.mir.variant_types[sum][*var];

            let param_types: Vec<_> = raw_var_types
                .iter()
                .map(|ty| {
                    let ty = (&finst).transform(ty);
                    to_morphization!(self.f.lir, self.f.mir, &mut self.f.current.tmap).apply(&ty)
                })
                .collect();
            let param_tuple = self.f.lir.mono.get_or_make_tuple(param_types.clone());

            let params = self.ssa().cast_payload(on, param_tuple.into());
            let params = (0..param_types.len() as u32)
                .map(key::Field)
                .zip(param_types)
                .map(|(field, ty)| self.ssa().field(params, param_tuple, field, ty))
                .collect();

            self.constructors.push(params);

            self.next(next);
            self.reset(oblock, resetpoint);
        }
    }

    pub fn get_continuation(&mut self, ty: MonoType) -> Block {
        match self.continuation_block {
            Some((block, _)) => block,
            None => {
                let block = self.ssa().new_block();
                self.continuation_block = Some((block, ty));
                block
            }
        }
    }
}

#[derive(Clone)]
struct ResetPoint {
    constructors: Vec<VecDeque<Value>>,
    map: Vec<ssa::Value>,
}

type SumBranches = mir::Branching<key::Variant>;
