use super::*;
use lumina_collections::MapKey;
use tracing::error;

pub fn insert_buf<K: MapKey, T: fmt::Debug>(
    at: V,
    map: &mut Map<K, T>,
    added: impl IntoIterator<Item = T>,
    replace: bool,
) {
    let buf = map.as_mut_vec();
    let rhs = buf.split_off(at.0 as usize);
    buf.extend(added);
    if replace {
        buf.extend(rhs.into_iter().skip(1));
    } else {
        buf.extend(rhs);
    }
}

#[derive(Clone)]
pub struct Rewrite<'p> {
    // Only apply to anything above this point
    pub atv: V,
    pub atb: Block,
    // Offsets in either direction
    pub voff: i32,
    pub boff: i32,
    // Start of the block and parameters to substitute bparams
    pub new_block_params: Option<(V, V, &'p [Value])>,
}

impl<'p> Rewrite<'p> {
    pub fn new(atv: V, atb: Block) -> Self {
        Self { atv, atb, voff: 0, boff: 0, new_block_params: None }
    }

    pub fn v(&self, in_rblock: bool, v: V) -> Value {
        if let Some((start, _, bparams)) = self.new_block_params {
            let pend = start.0 + bparams.len() as u32;
            if v.0 >= start.0 && v.0 < pend && !in_rblock {
                let i = v.0 - start.0;
                trace!("{v} detected as parameter {i} ({})", &bparams[i as usize]);
                return bparams[i as usize];
            }
        }
        Value::V(offset_if_after(self.atv, self.voff, v))
    }
    pub fn b(&self, block: Block) -> Block {
        offset_if_after(self.atb, self.boff, block)
    }

    pub fn entry(&self, in_rblock: bool, entry: &mut Entry) {
        for_entry_mut(entry, &mut |v| self.v(in_rblock, v), &mut |b| self.b(b));
    }
}

fn offset_if_after<K: MapKey>(at: K, off: i32, k: K) -> K {
    if k.into() >= at.into() {
        let new = (k.into() as u32).wrapping_add_signed(off) as usize;
        if new > usize::MAX - 500 {
            panic!("optimization caused underflow");
        }
        trace!("offsetting {k} by {off} into {}", K::from(new));
        new.into()
    } else {
        trace!("leaving {k}");
        k
    }
}

impl SSA {
    pub fn apply<'p>(&mut self, start: V, r: &Rewrite<'p>) {
        let mut in_rblock = false;
        for v in self.ventries.range_to_end(start) {
            if let Some((start_, end, _)) = r.new_block_params {
                if v == start_ {
                    in_rblock = true;
                }
                r.entry(in_rblock, &mut self.ventries[v]);
                if v == end {
                    in_rblock = false;
                }
            } else {
                r.entry(in_rblock, &mut self.ventries[v]);
            }
        }

        for block in self.blocks() {
            if self.blocks[block].start.0 >= r.atv.0 {
                info!("offsetting the start of {block} by {}", r.voff);

                if r.voff < 0 {
                    self.blocks[block].start.0 -= r.voff.abs() as u32;
                } else {
                    self.blocks[block].start.0 += r.voff as u32;
                }
            }
        }
    }

    pub fn delete_range_no_offset(&mut self, start: V, count: usize) {
        let removed = self
            .ventries
            .as_mut_vec()
            .drain(start.0 as usize..start.0 as usize + count)
            .inspect(|entry| trace!("removing {entry}"))
            .count();

        self.vtypes
            .as_mut_vec()
            .drain(start.0 as usize..start.0 as usize + count)
            .count();

        assert_eq!(count, removed);
    }

    pub fn purge_block(&mut self, block: Block) {
        let BlockInfo { start, end, .. } = self.block_info(block);

        offset_predecessors(self, end, -1);
        let bblock = self.blocks.as_mut_vec().remove(block.0 as usize);

        if bblock.predecessors != 0 {
            error!("purging block with active predecessors");
        }

        let removed = end.0 as usize - start.0 as usize + 1;
        self.delete_range_no_offset(start, removed);

        let mut r = Rewrite::new(start, block);
        r.voff = -(removed as i32);
        r.boff = -1;
        self.apply(V(0), &r);
    }
}

// Offset predecessor for any blocks referenced by the entry
pub(super) fn offset_predecessors(ssa: &mut SSA, end: V, by: i16) {
    for_entry_mut(&mut ssa.ventries[end], &mut |v| v.value(), &mut |b| {
        if by < 0 {
            ssa.blocks[b].predecessors -= by.abs() as u16;
        } else {
            ssa.blocks[b].predecessors += by as u16;
        }
        b
    });
}

pub(super) fn for_value_mut<F: Fn(V) -> Value>(v: &mut Value, f: &mut F) {
    match v {
        Value::V(i) => *v = f(*i),
        _ => {}
    }
}

pub(super) fn for_values_mut<F: Fn(V) -> Value>(values: &mut Vec<Value>, f: &mut F) {
    values.iter_mut().for_each(|v| for_value_mut(v, f))
}

pub(super) fn for_entry_mut<FV, FB>(entry: &mut Entry, on_v: &mut FV, on_b: &mut FB)
where
    FV: Fn(V) -> Value,
    FB: FnMut(Block) -> Block,
{
    match entry {
        Entry::CallStatic(_, params)
        | Entry::Variant(_, params)
        | Entry::Construct(params)
        | Entry::CallExtern(_, params)
        | Entry::JmpFunc(_, params) => for_values_mut(params, on_v),
        Entry::JmpBlock(BlockJump { params, id }) => {
            *id = on_b(*id);
            for_values_mut(params, on_v);
        }
        Entry::Select { value, on_true, on_false } => {
            on_true.id = on_b(on_true.id);
            on_false.id = on_b(on_false.id);
            for_value_mut(value, on_v);
            for_values_mut(&mut on_true.params, on_v);
            for_values_mut(&mut on_false.params, on_v);
        }
        Entry::JmpTable(v, blocks) => {
            for_value_mut(v, on_v);
            blocks.iter_mut().for_each(|b| *b = on_b(*b));
        }
        Entry::CallValue(v, params) => {
            for_value_mut(v, on_v);
            for_values_mut(params, on_v);
        }
        Entry::BinOp(_, [lhs, rhs])
        | Entry::WritePtr { ptr: lhs, value: rhs }
        | Entry::IntCmpInclusive([lhs, rhs], _, _) => {
            for_value_mut(lhs, on_v);
            for_value_mut(rhs, on_v);
        }
        Entry::MemCpy { dst, src, count } => {
            for_value_mut(dst, on_v);
            for_value_mut(src, on_v);
            for_value_mut(count, on_v);
        }
        Entry::SizeOf(_) => {}
        Entry::AlignOf(_) => {}
        Entry::Transmute(v)
        | Entry::IntAbs(v)
        | Entry::Field { of: v, .. }
        | Entry::Replicate(v, _)
        | Entry::BitNot(v)
        | Entry::CastFromSum { of: v }
        | Entry::TagFromSum { of: v }
        | Entry::Indice { of: v, .. }
        | Entry::Return(v)
        | Entry::Reduce(v)
        | Entry::Deref(v)
        | Entry::Dealloc { ptr: v }
        | Entry::ExtendSigned(v)
        | Entry::ExtendUnsigned(v)
        | Entry::IntToFloat(v, _)
        | Entry::FloatToInt(v, _) => for_value_mut(v, on_v),
        Entry::Alloc | Entry::Alloca | Entry::Trap(_) | Entry::RefStaticVal(_) => {}
        Entry::BlockParam(block, _) => *block = on_b(*block),
    }
}

pub(super) fn for_value(v: &Value, f: &mut dyn FnMut(V)) {
    match v {
        Value::V(i) => f(*i),
        _ => {}
    }
}

pub(super) fn for_values(values: &Vec<Value>, f: &mut dyn FnMut(V)) {
    values.iter().for_each(|v| for_value(v, f))
}

pub(super) fn for_entry(entry: &Entry, f: &mut dyn FnMut(V)) {
    match entry {
        Entry::CallStatic(_, params)
        | Entry::Variant(_, params)
        | Entry::Construct(params)
        | Entry::CallExtern(_, params)
        | Entry::JmpFunc(_, params)
        | Entry::JmpBlock(BlockJump { params, .. }) => for_values(params, f),
        Entry::CallValue(value, params) => {
            for_value(value, f);
            for_values(&params, f);
        }
        Entry::Select { value, on_true, on_false } => {
            for_value(value, f);
            for_values(&on_true.params, f);
            for_values(&on_false.params, f);
        }
        Entry::BinOp(_, [lhs, rhs])
        | Entry::WritePtr { ptr: lhs, value: rhs }
        | Entry::IntCmpInclusive([lhs, rhs], _, _) => {
            for_value(lhs, f);
            for_value(rhs, f);
        }
        Entry::MemCpy { dst, src, count } => {
            for_value(dst, f);
            for_value(src, f);
            for_value(count, f);
        }
        Entry::SizeOf(_) => {}
        Entry::AlignOf(_) => {}
        Entry::Transmute(v)
        | Entry::IntAbs(v)
        | Entry::Field { of: v, .. }
        | Entry::JmpTable(v, _)
        | Entry::Replicate(v, _)
        | Entry::BitNot(v)
        | Entry::CastFromSum { of: v }
        | Entry::TagFromSum { of: v }
        | Entry::Indice { of: v, .. }
        | Entry::Return(v)
        | Entry::Reduce(v)
        | Entry::Deref(v)
        | Entry::Dealloc { ptr: v }
        | Entry::ExtendSigned(v)
        | Entry::ExtendUnsigned(v)
        | Entry::IntToFloat(v, _)
        | Entry::FloatToInt(v, _) => for_value(v, f),
        Entry::Alloc
        | Entry::Alloca
        | Entry::Trap(_)
        | Entry::RefStaticVal(_)
        | Entry::BlockParam(_, _) => {}
    }
}
