//! Optimizations that are more specific to Lumina and more appropriate for a higher-level IR than
//! our backends.

use crate::lir::{
    ssa::rewrite::{
        for_entry_mut, for_v_mut, for_value_mut, insert_buf, offset_predecessors, Rewrite,
    },
    Block, BlockJump, Entry, Function, MonoFunc, MonoType, Value, LIR, SSA, V,
};
use smallvec::SmallVec;
use std::mem::take;
use tracing::{info, info_span, trace};

pub const ENABLE_OPTS: bool = true;

impl LIR {
    pub fn perform_optimizations(&mut self) {
        let mut opt = Optimizer { lir: self };
        opt.optimizations();
    }
}

struct Optimizer<'a> {
    lir: &'a mut LIR,
}

type Changed = bool;

impl<'a> Optimizer<'a> {
    fn optimizations(&mut self) {
        if !ENABLE_OPTS {
            return;
        }

        self.block_opts(1000);
        self.func_opts(1000);
        self.block_opts(5);
    }

    fn block_opts(&mut self, fuel: usize) {
        for fkey in self.lir.functions.keys() {
            let _span = info_span!(
                "running block optimizations",
                entity = self.lir.functions[fkey].symbol.clone(),
            );
            let _handle = _span.enter();

            let any_change = self.with_fuel(fuel, |lir| {
                let func = &mut lir.functions[fkey];
                let changed = func
                    .ssa
                    .blocks
                    .keys()
                    .any(|block| block_opt_iter(func, fkey, block));

                if changed {
                    trace!(
                        "mid-optimization {fkey} ({})\n{}",
                        &lir.functions[fkey].symbol,
                        lir.mono.fmt(&lir.functions[fkey])
                    );
                }

                changed
            });

            if any_change {
                info!(
                    "post-optimizations {fkey} ({}):\n{}",
                    &self.lir.functions[fkey].symbol,
                    self.lir.mono.fmt(&self.lir.functions[fkey])
                );
            }
        }
    }

    fn func_opts(&mut self, fuel: usize) {
        for fkey in self.lir.functions.keys() {
            let _span = info_span!(
                "running func optimizations",
                entity = self.lir.functions[fkey].symbol.clone(),
            );
            let _handle = _span.enter();

            let any_change = self.with_fuel(fuel, |lir| {
                let ssa = &lir.functions[fkey].ssa;
                let changed = ssa
                    .blocks
                    .keys()
                    .any(|block| func_opt_iter(lir, fkey, block));

                if changed {
                    trace!(
                        "mid-optimization func {fkey} ({})\n{}",
                        &lir.functions[fkey].symbol,
                        lir.mono.fmt(&lir.functions[fkey])
                    );
                }

                changed
            });

            if any_change {
                info!(
                    "post-optimizations func {fkey} ({}):\n{}",
                    &self.lir.functions[fkey].symbol,
                    self.lir.mono.fmt(&self.lir.functions[fkey])
                );
            }
        }
    }

    fn with_fuel(&mut self, mut fuel: usize, mut f: impl FnMut(&'_ mut LIR) -> Changed) -> Changed {
        let mut changed = true;
        let mut any_change = false;

        while fuel != 0 && changed {
            changed = f(self.lir);
            any_change |= changed;

            fuel -= 1;
        }

        any_change
    }
}

fn block_opt_iter(func: &mut Function, _: MonoFunc, block: Block) -> Changed {
    if func.ssa.blocks[block].predecessors == 0 {
        info!("purging {block}");
        func.ssa.purge_block(block);
        return true;
    }

    let start = func.ssa.blocks[block].start;

    for v in func.ssa.ventries.range_to_end(start) {
        match &func.ssa.ventries[v] {
            Entry::JmpBlock(jump) if fully_inlinable(&func.ssa, jump.id) && jump.id != block => {
                info!("inlining {} in {} regardless of size", jump.id, block);
                let jump = jump.clone(); // TODO: we don't have to clone
                full_block_inline(&mut func.ssa, v, &jump);
                return true;
            }
            // Substitute tail call to self with a jump to the entry block instead
            // TODO: re-add this but fix the cranelift limitation.
            // Entry::JmpFunc(mfunc, _) if *mfunc == fkey => {
            //     info!("substiuting call to self with jump to entry block");
            //     let Entry::JmpFunc(_, params) = &mut func.ssa.ventries[v] else {
            //         unreachable!()
            //     };
            //     // TODO: if we increment predecessors here, we can use that information to work
            //     // around cranelift optimizations technically not supporting jump to entrypoint.
            //     let jump = BlockJump::new(Block::entry(), take(params));
            //     func.ssa.blocks[Block::entry()].predecessors += 1;
            //     func.ssa.ventries[v] = Entry::JmpBlock(jump);
            //     return true;
            // }
            Entry::Select { on_true, on_false, .. } => {
                let new_on_true = try_inline_blockjump(&func.ssa, on_true);
                let new_on_false = try_inline_blockjump(&func.ssa, on_false);

                let Entry::Select { on_true, on_false, .. } = &mut func.ssa.ventries[v] else {
                    unreachable!()
                };

                let mut changed = false;
                if let Some(jump) = new_on_true {
                    changed = true;
                    info!("jump-inlining {} => {}", on_true.id, jump.id);
                    func.ssa.blocks[on_true.id].predecessors -= 1;
                    func.ssa.blocks[jump.id].predecessors += 1;
                    *on_true = jump;
                }
                if let Some(jump) = new_on_false {
                    changed = true;
                    info!("jump-inlining {} => {}", on_false.id, jump.id);
                    func.ssa.blocks[on_false.id].predecessors -= 1;
                    func.ssa.blocks[jump.id].predecessors += 1;
                    *on_false = jump;
                }

                return changed;
            }

            // v0 = call mfunc0()
            // return v0
            //   ----
            // jump mfunc0()
            Entry::CallStatic(..)
                if func.ssa.ventries[V(v.0 + 1)] == Entry::Return(Value::V(v)) =>
            {
                info!("substituting call+ret into tail call");

                let Entry::CallStatic(mfunc, params) = &mut func.ssa.ventries[v] else {
                    unreachable!();
                };

                let params = take(params);
                func.ssa.ventries[v] = Entry::JmpFunc(*mfunc, params);

                let retv = V(v.0 + 1);
                func.ssa.delete_range_no_offset(retv, 1);
                let mut r = Rewrite::new(retv, Block(0));
                r.voff = -1;
                func.ssa.apply(retv, &r);

                return true;
            }
            Entry::JmpTable(_, blocks) => {
                let rejumps = blocks
                    .iter()
                    .map(|block| {
                        let jmp = BlockJump::new(*block, vec![]);
                        try_inline_blockjump(&func.ssa, &jmp)
                    })
                    .collect::<SmallVec<[_; 5]>>();

                let Entry::JmpTable(_, blocks) = &mut func.ssa.ventries[v] else {
                    unreachable!();
                };

                let mut changed = false;

                for (rejump, jump) in rejumps.into_iter().zip(blocks) {
                    if let Some(new) = rejump {
                        changed = true;
                        info!("jump-inlining {} => {} inside jump table", jump, new.id);
                        *jump = new.id;
                    }
                }

                return changed;
            }
            entry if entry.is_terminator() => break,
            _ => {}
        }
    }

    false
}

// When blocks contain binds, only single-use or scope-pure blocks are inlineable.
//
// TODO: avoid inlining things like jump tables, instead of just going by bind count
fn fully_inlinable(ssa: &SSA, block: Block) -> bool {
    ssa.usages_outside_this_block(block) == 0
        && (ssa.blocks[block].predecessors == 1 || ssa.block_info(block).binds < 3)
}

// Fully inline a block, assume its valid to inline.
//
// 1. copy all `V` into a local buffer
// 2. in that local one, offset `V` by the *diff* between `atv` while substituting parameters
// 3. offset all `V` occuring after the inlined data by `inlined.len()`
fn full_block_inline(ssa: &mut SSA, atv: V, jump: &BlockJump) {
    ssa.blocks[jump.id].predecessors -= 1;
    let binfo = ssa.block_info(jump.id);

    let mut r = Rewrite::new(V(atv.0 + 1), Block(0));
    r.new_block_params = Some((binfo.start, binfo.end, &jump.params));

    let (mut inlinedv, mut inlinedt) =
        get_inlined_entries(ssa, r.clone(), V(binfo.start.0 + binfo.params), binfo.end);

    offset_predecessors(ssa, binfo.end, 1);

    // Offset all values occuring after the inline by the inline size
    // We haven't inserted the values yet. So; `V(atv+1)` still represents "all after inline".
    r.voff = inlinedv.len() as i32 - 1;
    // we don't remove the `new_block_params` because we still inline impure blocks if there's
    // a single predecessor. `apply` will check and make sure we're not corrupting the inlined block.
    ssa.apply(V(atv.0 + 1), &r);

    // Actually perform the inline
    insert_buf(atv, &mut ssa.ventries, inlinedv.drain(..), true);
    insert_buf(atv, &mut ssa.vtypes, inlinedt.drain(..), true);
}

fn full_func_inline(func: &mut SSA, ofunc: &SSA, atv: V, params: Vec<Value>) -> Vec<MonoFunc> {
    let conblock = func.new_block();
    let iblock = func.blocks.next_key();

    // Offset for `V`s in `ofunc` that we inline
    fn ioffset(injected: &[V], atv: V, v: V) -> V {
        let mut offset = atv.0 + 1;
        for inj in injected {
            // If this value occurs after this injected value
            if v.0 > inj.0 {
                offset += 1;
            }
        }
        V(v.0 + offset)
    }

    let mut to_bump = vec![];

    // Copy and compatible-ize the entries from the function we're inlining.
    let mut injected: Vec<V> = vec![];
    let (entries, types): (Vec<_>, Vec<_>) = ofunc
        .ventries
        .iter()
        .zip(ofunc.vtypes.values().cloned())
        .flat_map(|((v, entry), ty)| match entry.clone() {
            // Substitute return by jump to post-inline continuation
            Entry::Return(mut v) => {
                for_value_mut(&mut v, &mut |v| ioffset(&injected, atv, v).value());
                func.blocks[conblock].predecessors += 1;
                vec![(Entry::JmpBlock(BlockJump::new(conblock, vec![v])), ty)]
            }
            // Substitute JmpFunc by call+jump
            Entry::JmpFunc(mfunc, mut params) => {
                info!("injecting additional call instruction");
                to_bump.push(mfunc);

                for_v_mut(&mut params, &mut |v| ioffset(&injected, atv, v).value());
                func.blocks[conblock].predecessors += 1;

                let call = Entry::CallStatic(mfunc, params);
                let con_params = vec![ioffset(&injected, atv, v).value()];
                let con_jump = Entry::JmpBlock(BlockJump::new(conblock, con_params));

                injected.push(v);

                vec![(call, ty.clone()), (con_jump, ty)]
            }
            mut entry => {
                match entry {
                    Entry::CallStatic(mfunc, _) => to_bump.push(mfunc),
                    _ => {}
                }
                for_entry_mut(
                    &mut entry,
                    &mut |v| ioffset(&injected, atv, v).value(),
                    &mut |b| Block(b.0 + iblock.0),
                );
                vec![(entry, ty)]
            }
        })
        .unzip();

    let ty = func.vtypes[atv].clone();

    fn offset_by_inlined(v: V, atv: V, len: usize) -> V {
        if v.0 >= atv.0 {
            V(v.0 + 1 + len as u32)
        } else {
            v
        }
    }

    // Offset all values occuring after the inline
    for entry in func.ventries.values_mut() {
        for_entry_mut(
            entry,
            &mut |v| offset_by_inlined(v, atv, entries.len()).value(),
            &mut |b| b,
        );
    }

    for (block, bb) in func.blocks.iter_mut() {
        if block != conblock && bb.start != atv {
            bb.start = offset_by_inlined(bb.start, atv, entries.len());
        }
    }

    let constart = atv.0 + 1 + entries.len() as u32;

    fn insertion<Value>(buf: &mut Vec<Value>, extra: Vec<Value>, atv: V, [a, b]: [Value; 2]) {
        let rhs = buf.split_off(atv.0 as usize);
        buf.push(a);
        buf.extend(extra);
        buf.push(b);
        buf.extend(rhs.into_iter().skip(1));
    }

    // Split the entries at the call we inline
    // Inbetween:
    //   add the jump to the inlined entry block for the func we inlined
    //   insert all the inlined data (entries, types)
    //     before appending the right-hand side of the split;
    //     put the continuation parameter declaration which now acts as the replacement for `atv = call F`
    let injection = [
        Entry::JmpBlock(BlockJump::new(iblock, params)),
        Entry::BlockParam(conblock, 0),
    ];
    insertion(func.ventries.as_mut_vec(), entries, atv, injection);
    insertion(func.vtypes.as_mut_vec(), types, atv, [ty.clone(), ty]);

    func.blocks[conblock].start = V(constart);

    // Copy over the block information from the function we inline, offsetting the start of the
    // blocks by `atv`
    for mut bb in ofunc.blocks.values().cloned() {
        bb.start = ioffset(&injected, atv, bb.start);
        func.blocks.push(bb);
    }

    to_bump
}

fn get_inlined_entries<'p>(
    ssa: &SSA,
    mut r: Rewrite<'p>,
    start: V,
    end: V,
) -> (Vec<Entry>, Vec<MonoType>) {
    let range = start.0 as usize..=end.0 as usize;

    let mut entries = ssa.ventries.as_slice()[range.clone()].to_vec();
    let types = ssa.vtypes.as_slice()[range].to_vec();

    // Offset the new inlined to start at the current V.
    //
    // since voff is signed, this should work regardless of whether `atv` is before or after
    // `jump.id` is defined.
    r.voff = (r.atv.0 as i32 - 1) - start.0 as i32;

    entries.iter_mut().for_each(|entry| r.entry(false, entry));

    (entries, types)
}

fn try_inline_blockjump(ssa: &SSA, ijump: &BlockJump) -> Option<BlockJump> {
    let binfo = ssa.block_info(ijump.id);
    if binfo.binds == 0 {
        if let Entry::JmpBlock(jump) = binfo.tail {
            if ssa.usages_outside_this_block(ijump.id) == 0 {
                // Substitute the parameters in iblock with the parameters given to the
                // jump to iblock, to be used for the jump to the block jumped to by iblock.
                let params = jump
                    .params
                    .iter()
                    .map(|p| match p {
                        Value::V(v) => {
                            if let Some(i) = binfo.is_param(*v) {
                                ijump.params[i]
                            } else {
                                *p
                            }
                        }
                        _ => *p,
                    })
                    .collect();

                return Some(BlockJump::new(jump.id, params));
            }
        }
    }

    None
}

fn func_opt_iter(lir: &mut LIR, func: MonoFunc, _: Block) -> Changed {
    for v in lir.functions[func].ssa.ventries.keys() {
        match &lir.functions[func].ssa.ventries[v] {
            Entry::CallStatic(mfunc, params) if should_inline(lir, *mfunc) => {
                info!(
                    "inlining the call to {} inside of {}",
                    &lir.functions[*mfunc].symbol, &lir.functions[func].symbol
                );
                let params = params.clone();
                let [func, cfunc] = lir.functions.get_many_mut([func, *mfunc]);
                let to_bump = full_func_inline(&mut func.ssa, &cfunc.ssa, v, params);
                for func in to_bump {
                    lir.functions[func].invocations += 1;
                }
                return true;
            }
            _ => {}
        }
    }
    false
}

// inline small functions or those who are only invoked once
//
// TODO: make sure directly recursive functions still work to inline
// (I think they can since it can re-jump to entry)
fn should_inline(lir: &LIR, func: MonoFunc) -> bool {
    let func = &lir.functions[func];
    func.invocations == 1
        || (func.ssa.ventries.len() - func.ssa.block_params(Block::entry()).count()) < 3
}

impl Entry {
    pub fn is_terminator(&self) -> bool {
        match self {
            Entry::JmpFunc(..)
            | Entry::JmpBlock(..)
            | Entry::Return(..)
            | Entry::Select { .. }
            | Entry::JmpTable(..)
            | Entry::Trap(..) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debuginfo::Item;
    use crate::lir::{ty_fmt, MonoType};
    use lumina_key as key;
    use lumina_key::{Map, M};
    use tracing::info;

    fn test_block_opts(hi: bool, name: &str, ssa: SSA) -> (String, String) {
        let mut fuel = 5;

        let item = Item::Defined(M(key::Module::from(0), key::Func::from(0)));
        let mut func = Function::new(name.into(), item, ssa, MonoType::u(8), 1);

        let mut changed = true;

        let types = Map::new();
        lumina_util::enable_highlighting(hi);
        let before = format!("BEFORE:\n{}", ty_fmt(&types, &func.ssa));

        while fuel != 0 && changed {
            changed = func
                .ssa
                .blocks
                .keys()
                .any(|block| block_opt_iter(&mut func, MonoFunc(0), block));

            if changed {
                info!("MIDDLE OPTIMIZATION:\n{}", ty_fmt(&types, &func));
            }

            fuel -= 1;
        }

        let after = format!("AFTER:\n{}", ty_fmt(&types, &func.ssa));

        (before, after)
    }

    #[test]
    fn inline_block() {
        lumina_util::test_logger();

        let mut ssa = SSA::new();

        let block = [Block::entry(), ssa.new_block()];

        let v0 = ssa.add_block_param(block[0], MonoType::u(0));
        let v1 = ssa.add(v0.value(), Value::u(1, 1), MonoType::u(1));
        let _v2 = ssa.jump(block[1], vec![v1]);

        ssa.switch_to_block(block[1]);
        let v3 = ssa.add_block_param(block[1], MonoType::u(3));
        let v4 = ssa.construct(vec![v0.value(), v1, v1, v3.value()], MonoType::u(4));
        let _v5 = ssa.return_(v4);

        let (before, after) = test_block_opts(false, "inline_block", ssa);
        insta::assert_snapshot!(format!("{before}\n{after}"));
    }

    #[test]
    fn tricky() {
        lumina_util::test_logger();

        let mut ssa = SSA::new();

        let block = [Block::entry(), ssa.new_block(), ssa.new_block()];
        ssa.select(
            Value::bool(true),
            [
                (block[1], vec![Value::u(1, 8)]),
                (block[1], vec![Value::u(2, 8)]),
            ],
        );

        ssa.switch_to_block(block[1]);
        let p = ssa.add_block_param(block[1], MonoType::u(8));
        ssa.jump(block[2], vec![]);

        ssa.switch_to_block(block[2]);
        ssa.return_(p.value());

        let (before, after) = test_block_opts(false, "inline_block", ssa);
        insta::assert_snapshot!(format!("{before}\n{after}"));
    }

    #[test]
    fn practical() {
        lumina_util::test_logger();
        use lumina_typesystem::IntSize;

        let mut ssa = SSA::new();

        let block = [Block::entry(), ssa.new_block()];

        let v0 = ssa.add_block_param(block[0], MonoType::u(0));
        let _v1 = ssa.cast_payload(v0.value(), MonoType::u(1));
        let _v2 = ssa.abs(v0.value(), MonoType::u(2));
        ssa.jump(block[1], vec![]);

        ssa.switch_to_block(block[1]);
        let v3 = ssa.tag_of(v0.value(), IntSize::new(false, 16));
        ssa.return_(v3);
        // ssa.select(v3, [(block[2], vec![]), (block[3], vec![])]);

        let (before, after) = test_block_opts(false, "inline_block", ssa);
        insta::assert_snapshot!(format!("{before}\n{after}"));
    }

    #[test]
    fn functions() {
        lumina_util::test_logger();

        let ofunc = MonoFunc(1);

        let (mut fssa, v0, v1) = {
            let mut ssa = SSA::new();
            let v0 = ssa.abs(Value::u(0, 0), MonoType::u(0));
            let v1 = ssa.call(ofunc, vec![v0], MonoType::u(1));
            ssa.return_(v1);
            (ssa, v0, v1)
        };

        let ossa = {
            let mut ssa = SSA::new();
            ssa.new_block();

            let v0 = ssa.add_block_param(Block(0), MonoType::u(2));
            ssa.jump(Block(1), vec![v0.value()]);

            ssa.switch_to_block(Block(1));
            let p = ssa.add_block_param(Block(1), MonoType::u(2));
            ssa.jump(MonoFunc(2), vec![p.value()]);

            ssa
        };

        let types = Map::new();
        lumina_util::enable_highlighting(false);
        let before = format!(
            "BEFORE:\n{}\n{}",
            ty_fmt(&types, &fssa),
            ty_fmt(&types, &ossa)
        );

        let Value::V(v1) = v1 else { unreachable!() };
        full_func_inline(&mut fssa, &ossa, v1, vec![v0]);

        let after = format!("AFTER:\n{}", ty_fmt(&types, &fssa));

        insta::assert_snapshot!(format!("{before}\n{after}"));
    }
}
