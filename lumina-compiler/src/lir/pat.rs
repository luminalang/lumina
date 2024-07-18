use super::*;
use crate::LISTABLE_SPLIT;
use ibig::IBig;
use mir::ListConstr;
use mir::Range;
use ssa::{Block, Value};
use std::cmp::Ordering as Ord;
use std::collections::VecDeque;

impl<'a> FuncLower<'a> {
    pub fn to_pat_lower<'f>(&'f mut self) -> PatLower<'f, 'a> {
        PatLower {
            continuation_block: None,
            continuation_value: None,

            f: self,

            constructors: vec![],

            map: vec![],

            can_skip_continuation: true,
        }
    }
}

pub struct PatLower<'f, 'a> {
    f: &'f mut FuncLower<'a>,

    // All branches put their yielded value as a parameter to this block and jump to it
    //
    // This desugars `f (match ...)`
    continuation_block: Option<ssa::Block>,
    continuation_value: Option<ssa::Value>,

    constructors: Vec<VecDeque<Value>>,
    map: Vec<(mir::PatPoint, ssa::Value)>,

    can_skip_continuation: bool,
}

impl<'f, 'a> PatLower<'f, 'a> {
    fn ssa(&mut self) -> &mut ssa::Blocks {
        self.f.ssa()
    }

    fn block(&self) -> Block {
        self.f.lir.functions[self.f.current.mfkey].blocks.block()
    }

    pub fn run(&mut self, on: ssa::Value, tree: &mir::DecTree) -> Value {
        self.tree(on, tree);

        if self.can_skip_continuation {
            assert_eq!(self.continuation_block, None);
            self.continuation_value.unwrap()
        } else {
            assert_eq!(self.continuation_value, None);
            let block = self.continuation_block.unwrap();
            self.ssa().switch_to_block(block);
            ssa::Value::BlockParam(ssa::BlockParam(0))
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
        let on = self.f.ensure_no_scope_escape(on);
        self.map.push((tree.point, on));

        match &tree.branch {
            mir::DecTreeBranch::Ints(bitsize, min, _, vars) => {
                self.can_skip_continuation &= vars.branches.len() == 1;

                let signed = *min < IBig::from(0);

                let mut vars = vars.branches.clone();
                vars.sort_by(|(range, _, _), (orange, _, _)| range.start.cmp(&orange.start));
                let checks = vars_to_check_blocks(vars.clone());

                self.ints(on, checks, signed, *bitsize)
            }
            mir::DecTreeBranch::Tuple(_, next) => self.tuple(on, next),
            mir::DecTreeBranch::Bools(v) => self.bools(on, v),
            mir::DecTreeBranch::Sum(_, typings, _, vars) => self.sum(on, typings, vars),
            mir::DecTreeBranch::List(kind, ty, vars) => self.list(on, *kind, ty, vars),
            mir::DecTreeBranch::Record(key, params, _, tree) => self.record(on, *key, params, tree),
            mir::DecTreeBranch::Wildcard(next) => self.next(next),
            mir::DecTreeBranch::Reached(..) => unreachable!(),
            mir::DecTreeBranch::Poison => todo!("?"),
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
            None => match &tree.branch {
                mir::DecTreeBranch::Reached(table, expr) => {
                    for (bind, point) in &table.map {
                        let (_, v) = self.map.iter().find(|(p, _)| p == point).unwrap();
                        trace!("binding {bind} -> {v}");
                        self.f.current.bindmap.insert(*bind, *v);
                    }

                    let v = self.f.expr_to_value(expr);

                    let ty = self.f.type_of_value(v);

                    if self.can_skip_continuation {
                        self.continuation_value = Some(v);
                    } else {
                        let con = self.get_continuation(ty);
                        self.ssa().jump_continuation(con, vec![v]);
                    }
                }
                other => unreachable!("misaligned constructor ordering:\n{other}"),
            },
        }
    }

    fn tuple(&mut self, on: Value, next: &mir::DecTree) {
        let mk = self.f.type_of_value(on).as_key();

        let constructor = self
            .f
            .lir
            .types
            .fields(mk)
            .map(|field| {
                let ty = self.f.lir.types.types.type_of_field(mk, field);
                self.ssa().field(on, mk, field, ty).into()
            })
            .collect();

        self.constructors.push(constructor);

        self.next(next)
    }

    fn bools(&mut self, on: Value, v: &mir::BranchOf<bool>) {
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
        self.next(&truthy.2);

        self.reset(on_false, resetpoint);
        self.next(&falsey.2);
    }

    fn list(
        &mut self,
        on: Value,
        _: M<key::TypeKind>,
        ty: &Type,
        vars: &mir::BranchOf<ListConstr>,
    ) {
        self.can_skip_continuation = false;

        let oblock = self.block();
        let on = self.f.ensure_no_scope_escape(on);

        let mut morph = to_morphization!(self.f, &mut self.f.current.tmap);
        let listmt = morph.apply(&ty);
        let list = morph.apply_weak(&ty);
        let (_, inner) = match &ty {
            Type::Defined(kind, params) | Type::List(kind, params) => {
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

        let split = FuncOrigin::Method(ikey, LISTABLE_SPLIT);
        let (split, ret) = self.f.call_to_mfunc(split, tmap);

        let maybe = self.ssa().call(split, vec![on], ret).value();
        let maybe_mk = self.f.type_of_value(maybe).as_key();

        let tag_ty = MonoType::UInt(mono::TAG_SIZE);
        let tag = self
            .ssa()
            .field(maybe, maybe_mk, key::RecordField(0), tag_ty);

        let data_ty = MonoType::SumDataCast {
            largest: self.f.lir.types.types.size_of_defined(maybe_mk) - mono::TAG_SIZE.0 as u32,
        };
        let data = self
            .ssa()
            .field(maybe, maybe_mk, key::RecordField(1), data_ty)
            .into();

        let is_just = self
            .ssa()
            .eq([tag.value(), Value::maybe_just()], mono::TAG_SIZE);

        let [con_block, nil_block] = [ListConstr::Cons, ListConstr::Nil].map(|constr| {
            let vblock = self.ssa().new_block();
            self.ssa().switch_to_block(vblock);

            let resetpoint = self.make_reset();

            let mut vparams = VecDeque::new();

            // Add parameters matching the MIR pattern of `Cons x xs`
            if constr == ListConstr::Cons {
                let mut offset = BitOffset(0);

                let x = self.ssa().sum_field(data, offset, innermt.clone());
                offset.0 += self.f.lir.types.types.size_of(&innermt) as u32;

                let xs = self.ssa().sum_field(data, offset, listmt.clone());

                vparams.push_back(x.value());
                vparams.push_back(xs.value());
            }

            self.constructors.push(vparams);

            let next = vars
                .branches
                .iter()
                .find_map(|(con, _, n)| (*con == constr).then_some(n))
                .unwrap();

            self.next(next);
            self.reset(oblock, resetpoint);

            vblock
        });

        self.ssa()
            .select(is_just.value(), [(con_block, vec![]), (nil_block, vec![])]);
    }

    fn record(&mut self, on: Value, _: M<key::Record>, _: &[Type], next: &mir::DecTree) {
        let mk = self.f.type_of_value(on).as_key();

        let constructor = self
            .f
            .lir
            .types
            .fields(mk)
            .map(|field| {
                let ty = self.f.lir.types.types.type_of_field(mk, field);
                self.ssa().field(on, mk, field, ty).into()
            })
            .collect();

        self.constructors.push(constructor);

        self.next(next);
    }

    fn sum(
        &mut self,
        on: Value,
        typings: &Map<key::SumVariant, mir::CallTypes>,
        v: &mir::BranchOf<key::SumVariant>,
    ) {
        self.can_skip_continuation &= typings.len() == 1;

        let oblock = self.block();
        let on = self.f.ensure_no_scope_escape(on);
        let on_mk = self.f.type_of_value(on).as_key();

        let tag_ty = MonoType::UInt(mono::TAG_SIZE);
        let copy_tag = self.ssa().field(on, on_mk, key::RecordField(0), tag_ty);

        let data = self
            .f
            .lir
            .types
            .types
            .type_of_field(on_mk, key::RecordField(1));

        let data_field = self.ssa().field(on, on_mk, key::RecordField(1), data);

        let jmp_table_blocks = typings
            .iter()
            .map(|(variant, typing)| {
                let vblock = self.ssa().new_block();
                self.ssa().switch_to_block(vblock);

                let resetpoint = self.make_reset();

                let mut base_offset = BitOffset(0);
                let params = typing
                    .params
                    .values()
                    .map(|ty| {
                        let mut morph = mono::Monomorphization::new(
                            &mut self.f.lir.types,
                            &self.f.mir.field_types,
                            &self.f.mir.variant_types,
                            &self.f.mir.methods,
                            &self.f.mir.funcs,
                            &self.f.mir.trait_objects,
                            &mut self.f.current.tmap,
                        );
                        let ty = morph.apply(ty);

                        let size = self.f.lir.types.types.size_of(&ty) as u32;
                        let offset = base_offset;
                        base_offset.0 += size;

                        self.ssa().sum_field(data_field.into(), offset, ty).into()
                    })
                    .collect();

                self.constructors.push(params);

                let (_, _, next) = v.branches.iter().find(|(v, _, _)| *v == variant).unwrap();

                self.next(next);
                self.reset(oblock, resetpoint);

                vblock
            })
            .collect();

        self.ssa().jump_table(copy_tag.into(), jmp_table_blocks);
    }

    fn bound_checks(
        &mut self,
        on: Value,
        start: &IBig,
        end: &IBig,
        signed: bool,
        bitsize: Bitsize,
    ) -> Option<Value> {
        let max = bitsize.maxi(signed);
        let min = bitsize.mini(signed);

        match (-IBig::from(min) == *start, IBig::from(max) == *end) {
            (true, false) => {
                let v = ibig_to_value(end, signed, bitsize);
                let v = self.ssa().lti([on, v], bitsize).into();
                Some(v)
            }
            (false, true) => {
                let v = ibig_to_value(start, signed, bitsize);
                let v = self.ssa().gti([on, v], bitsize).into();
                Some(v)
            }
            (false, false) if start == end => {
                let v = ibig_to_value(start, signed, bitsize);
                let v = self.ssa().eq([on, v], bitsize).into();
                Some(v)
            }
            (false, false) => {
                let bounds = [start, end].map(|n| ibig_to_value(n, signed, bitsize));
                let ty = self.f.type_of_value(on);
                let v = self
                    .ssa()
                    .cmps(on, [Ord::Greater, Ord::Less], bounds, bitsize, ty);
                Some(v)
            }
            (true, true) => None,
        }
    }

    fn ints(&mut self, on: Value, checks: Vec<NCheck>, signed: bool, bitsize: Bitsize) {
        let resetpoint = self.make_reset();

        for check in checks {
            let [on_true, on_false] = [self.ssa().new_block(), self.ssa().new_block()];

            match check {
                NCheck::LongRange(range, next) | NCheck::SmolRange(range, next) => {
                    match self.bound_checks(on, &range.start, &range.end, signed, bitsize) {
                        Some(check) => {
                            self.ssa()
                                .select(check, [(on_true, vec![]), (on_false, vec![])]);

                            self.ssa().switch_to_block(on_true);
                            self.next(&next);

                            self.reset(on_false, resetpoint.clone());
                        }
                        None => {
                            todo!();
                        }
                    }
                }
                NCheck::JmpTable { start, cons } => {
                    let [Some(initnext), xs @ ..] = cons.as_slice() else {
                        unreachable!();
                    };

                    let within_bounds = {
                        let end = start.clone() + cons.len();
                        self.bound_checks(on, &start, &end, signed, bitsize)
                            .unwrap()
                    };

                    self.ssa()
                        .select(within_bounds, [(on_true, vec![]), (on_false, vec![])]);

                    self.ssa().switch_to_block(on_true);

                    let by = ibig_to_value(&start, signed, bitsize);
                    let ty = signed
                        .then_some(MonoType::Int(bitsize))
                        .unwrap_or(MonoType::UInt(bitsize));
                    let normalised = self.ssa().sub(on, by, ty);

                    let mut pblock = self.ssa().new_block();
                    self.ssa().switch_to_block(pblock);
                    self.next(&initnext);

                    self.reset(pblock, resetpoint.clone());

                    let blocks = std::iter::once(pblock)
                        .chain(xs.iter().map(|next| match next {
                            None => pblock,
                            Some(next) => {
                                let block = self.ssa().new_block();
                                self.ssa().switch_to_block(block);
                                self.next(next);
                                self.reset(on_true, resetpoint.clone());
                                pblock = block;
                                block
                            }
                        }))
                        .collect();

                    // We should've already reset back to the on_true block which is what
                    // should contain the jump table since on_true is for the bound checks
                    self.ssa().jump_table(normalised.into(), blocks);

                    // The next check should occur in the on_false block
                    self.ssa().switch_to_block(on_false);
                }
            }
        }
    }

    pub fn get_continuation(&mut self, ty: MonoType) -> Block {
        match self.continuation_block {
            Some(block) => block,
            None => {
                let block = self.ssa().new_block();
                self.ssa().add_block_param(block, ty);
                self.continuation_block = Some(block);
                block
            }
        }
    }
}

#[derive(Debug)]
enum NCheck {
    SmolRange(Range, mir::DecTree),
    LongRange(Range, mir::DecTree),
    JmpTable {
        start: IBig,
        // None means that it has the same continuation as the number before it
        cons: Vec<Option<mir::DecTree>>,
    },
}

impl NCheck {
    fn new(range: Range, next: mir::DecTree) -> Self {
        let diff = range.end.clone() - range.start.clone();
        if diff < IBig::from(4) {
            NCheck::SmolRange(range, next)
        } else {
            NCheck::LongRange(range, next)
        }
    }
}

fn vars_to_check_blocks<T>(vars: Vec<(Range, T, mir::DecTree)>) -> Vec<NCheck> {
    let mut buf = vec![];

    for (range, _, next) in vars {
        let check = NCheck::new(range, next);

        match (buf.last_mut(), check) {
            (Some(NCheck::SmolRange(prev, pnext)), NCheck::SmolRange(mut n, next))
                if n.start == prev.end.clone() + 1 =>
            {
                let mut cons = vec![];
                cons.push(Some(pnext.clone()));
                while prev.end != prev.start {
                    prev.end -= 1;
                    cons.push(None);
                }

                cons.push(Some(next));
                while n.end != n.start {
                    n.end -= 1;
                    cons.push(None);
                }

                let start = prev.start.clone();
                let lasti = buf.len() - 1;
                buf[lasti] = NCheck::JmpTable { start, cons };
            }
            (Some(NCheck::JmpTable { start, cons }), NCheck::SmolRange(mut n, next))
                if start.clone() + cons.len() == n.start =>
            {
                cons.push(Some(next));

                while n.end != n.start {
                    n.end -= 1;
                    cons.push(None);
                }
            }
            (_, check) => buf.push(check),
        }
    }

    buf
}

#[derive(Clone)]
struct ResetPoint {
    constructors: Vec<VecDeque<Value>>,
    map: Vec<(mir::PatPoint, ssa::Value)>,
}

fn ibig_to_value(n: &IBig, signed: bool, bitsize: Bitsize) -> Value {
    if signed {
        let n = n.to_string().parse::<i128>().unwrap();
        Value::Int(n, bitsize)
    } else {
        let n = n.to_string().parse::<u128>().unwrap();
        Value::UInt(n, bitsize)
    }
}
