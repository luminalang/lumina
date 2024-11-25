use super::*;
use crate::lir::MonoType;
use lumina_collections::MapKey;
use tracing::error;

fn insert_buf<K: MapKey, T: fmt::Debug>(
    at: V,
    map: &mut Map<K, T>,
    added: &mut Vec<T>,
    replace: bool,
) {
    let buf = map.as_mut_vec();
    let rhs = buf.split_off(at.0 as usize);
    buf.extend(added.drain(..));
    if replace {
        buf.extend(rhs.into_iter().skip(1));
    } else {
        buf.extend(rhs);
    }
}

impl SSA {
    pub fn delete_range(&mut self, start: V, count: usize) {
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

        for bb in self.blocks.values_mut() {
            if bb.start.0 > start.0 {
                bb.start.0 -= removed as u32;
            }
        }

        assert_eq!(count, removed);

        // TODO: possible of-by-one error?
        self.offset_values(start, V(start.0 + removed as u32), -(removed as i32));
    }

    pub fn purge_block(&mut self, block: Block) {
        let BlockInfo { start, end, .. } = self.block_info(block);

        let bblock = self.blocks.as_mut_vec().remove(block.0 as usize);

        if bblock.predecessors != 0 {
            error!("purging block with active predecessors");
        }

        self.delete_range(start, end.0 as usize - start.0 as usize + 1);

        self.for_each_block(V(0), |b| {
            if b.0 >= block.0 {
                b.0 -= 1;
            }
        });
    }

    pub fn for_each_block(&mut self, at: V, mut f: impl FnMut(&mut Block)) {
        for v in KeysIter::range(at, usize::MAX) {
            if !self.ventries.has(v) {
                return;
            }

            match &mut self.ventries[v] {
                Entry::JmpBlock(block) => f(&mut block.id),
                Entry::Select { on_true, on_false, .. } => {
                    f(&mut on_true.id);
                    f(&mut on_false.id);
                }
                Entry::JmpTable(_, blocks) => blocks.iter_mut().for_each(|block| f(block)),
                Entry::BlockParam(block, _) => f(block),
                _ => {}
            }
        }
    }

    pub fn offset_values(&mut self, start: V, when: V, offset: i32) {
        for v in KeysIter::range(start, usize::MAX) {
            if self.ventries.has(v) {
                for_entry_mut(&mut self.ventries[v], |v| {
                    if v.0 >= when.0 {
                        info!("{v} -> {}", V(v.0.checked_add_signed(offset).unwrap()));
                        V(v.0.checked_add_signed(offset).unwrap()).value()
                    } else {
                        info!("keeping {v}");
                        v.value()
                    }
                });
            } else {
                break;
            }
        }
    }

    pub fn insert(
        &mut self,
        at: V,
        entries: &mut Vec<Entry>,
        types: &mut Vec<MonoType>,
        replace: bool,
    ) {
        assert_eq!(entries.len(), types.len());

        let mut offset = entries.len() as u32;
        if replace {
            offset -= 1;
        }

        self.offset_values(V(at.0 + 1), V(at.0 + 1), offset as i32);

        // Do we need to use `replace` in the edge-case?
        for bblock in self.blocks.values_mut() {
            if bblock.start.0 > at.0 && bblock.start != V(u32::MAX) {
                info!("r block start {} += {offset}", bblock.start);
                bblock.start.0 += offset;
            }
        }

        insert_buf(at, &mut self.ventries, entries, replace);
        insert_buf(at, &mut self.vtypes, types, replace);
    }
}

pub(super) fn for_value_mut(v: &mut Value, f: impl Fn(V) -> Value) {
    match v {
        Value::V(i) => *v = f(*i),
        _ => {}
    }
}

pub(super) fn for_values_mut(values: &mut Vec<Value>, f: impl Fn(V) -> Value + Clone) {
    values.iter_mut().for_each(|v| for_value_mut(v, f.clone()))
}

// TODO: at this point; we should probably just do kind+params pattern instead of this.

pub(super) fn for_entry_mut(entry: &mut Entry, f: impl Fn(V) -> Value + Clone) {
    match entry {
        Entry::CallStatic(_, params)
        | Entry::Variant(_, params)
        | Entry::Construct(params)
        | Entry::CallExtern(_, params)
        | Entry::JmpFunc(_, params)
        | Entry::JmpBlock(BlockJump { params, .. }) => for_values_mut(params, f),
        Entry::Select { value, on_true, on_false } => {
            for_value_mut(value, f.clone());
            for_values_mut(&mut on_true.params, f.clone());
            for_values_mut(&mut on_false.params, f);
        }
        Entry::CallValue(v, params) => {
            for_value_mut(v, f.clone());
            for_values_mut(params, f);
        }
        Entry::BinOp(_, [lhs, rhs])
        | Entry::WritePtr { ptr: lhs, value: rhs }
        | Entry::IntCmpInclusive([lhs, rhs], _, _) => {
            for_value_mut(lhs, f.clone());
            for_value_mut(rhs, f);
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
        | Entry::FloatToInt(v, _) => for_value_mut(v, f),
        Entry::Alloc
        | Entry::Alloca
        | Entry::Trap(_)
        | Entry::RefStaticVal(_)
        | Entry::BlockParam(_, _) => {}
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
        Entry::SizeOf(_) => todo!(),
        Entry::AlignOf(_) => todo!(),
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
