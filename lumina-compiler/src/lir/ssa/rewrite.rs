use super::*;
use crate::lir::MonoType;
use lumina_collections::MapKey;

// pub struct Rewrite {
//     offv: V,
//
// }

fn insert_buf<K: MapKey, T: fmt::Debug>(
    after: V,
    map: &mut Map<K, T>,
    added: Vec<T>,
    replace: bool,
) {
    let buf = map.as_mut_vec();
    let rhs = buf.split_off(after.0 as usize);
    buf.extend(added);
    if replace {
        buf.extend(rhs.into_iter().skip(1));
    } else {
        buf.extend(rhs);
    }
}

impl SSA {
    pub fn copy_for_inline(
        &self,
        from: V,
        into: V,
        params: &[Value],
        buf: &mut Vec<Entry>,
        tbuf: &mut Vec<MonoType>,
    ) {
        let offset = into.0 as i32 - from.0 as i32;

        for v in KeysIter::range(from, usize::MAX) {
            match self.entry_of(v).clone() {
                Entry::BlockParam(_, _) => {}
                mut e => {
                    for_entry_mut(&mut e, |v| {
                        if v.0 < from.0 {
                            dbg!(v, from);
                            return v.value();
                        }

                        match params.get((v.0 - from.0) as usize) {
                            Some(v) => *v,
                            None => V(v.0.wrapping_add_signed(offset)).value(),
                        }
                    });
                    let is_term = e.is_terminator();
                    buf.push(e);
                    tbuf.push(self.vtypes[v].clone());

                    if is_term {
                        break;
                    }
                }
            }
        }
    }

    pub fn insert(&mut self, at: V, entries: Vec<Entry>, types: Vec<MonoType>, replace: bool) {
        assert_eq!(entries.len(), types.len());

        let mut offset = entries.len() as u32;
        if replace {
            offset -= 1;
        }

        for v in KeysIter::range(V(at.0 + 1), usize::MAX) {
            if self.ventries.has(v) {
                for_entry_mut(&mut self.ventries[v], |v| {
                    if v.0 > at.0 {
                        V(v.0 + offset).value()
                    } else {
                        v.value()
                    }
                });
            } else {
                break;
            }
        }

        if replace {
            // TODO: not sure about these comparisons
            for bblock in self.blocks.values_mut() {
                if bblock.start.0 >= at.0 && bblock.start != V(u32::MAX) {
                    bblock.start.0 += offset;
                }
            }
        } else {
            for bblock in self.blocks.values_mut() {
                if bblock.start.0 > at.0 && bblock.start != V(u32::MAX) {
                    bblock.start.0 += offset;
                }
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
        | Entry::CallValue(_, params)
        | Entry::JmpFunc(_, params)
        | Entry::JmpBlock(BlockJump { params, .. }) => for_values_mut(params, f),
        Entry::Select { value, on_true, on_false } => {
            for_value_mut(value, f.clone());
            for_values_mut(&mut on_true.params, f.clone());
            for_values_mut(&mut on_false.params, f);
        }
        Entry::BinOp(_, [lhs, rhs])
        | Entry::WritePtr { ptr: lhs, value: rhs }
        | Entry::IntCmpInclusive([lhs, rhs], _, _) => {
            for_value_mut(lhs, f.clone());
            for_value_mut(rhs, f);
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
        | Entry::CallValue(_, params)
        | Entry::JmpFunc(_, params)
        | Entry::JmpBlock(BlockJump { params, .. }) => for_values(params, f),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir::ty_fmt;

    #[test]
    fn inline_block() {
        lumina_util::enable_highlighting(false);

        let mut ssa = SSA::new();
        let types = Map::new();

        let block = [Block::entry(), ssa.new_block()];

        let v0 = ssa.add_block_param(block[0], MonoType::u(0));
        let v1 = ssa.add(v0.value(), Value::u(1, 1), MonoType::u(1));
        let v2 = ssa.jump(block[1], vec![v1]);

        ssa.switch_to_block(block[1]);
        let v3 = ssa.add_block_param(block[1], MonoType::u(3));
        let v4 = ssa.construct(vec![v0.value(), v1, v2, v3.value()], MonoType::u(4));
        let _v5 = ssa.return_(v4);

        let before = format!("BEFORE:\n{}", ty_fmt(&types, &ssa));

        let at = V(2);
        let istart = V(3);
        assert_eq!(istart, ssa.blocks[block[1]].start);

        let mut entrybuf = Vec::new();
        let mut typebuf = Vec::new();
        ssa.copy_for_inline(istart, at, &[v1], &mut entrybuf, &mut typebuf);
        ssa.insert(
            at,
            std::mem::take(&mut entrybuf),
            std::mem::take(&mut typebuf),
            true,
        );

        let after = format!("AFTER:\n{}", ty_fmt(&types, &ssa));

        insta::assert_snapshot!(format!("{before}\n{after}"));
    }
}
