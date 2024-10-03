use super::*;
use std::cmp::Ordering;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn float_to_int(&mut self, n: lir::Value, intsize: IntSize) -> VEntry {
        let v = self.value_to_entry(n).as_direct();
        let int = Type::int(intsize.bits() as u16).unwrap();
        let v = if intsize.signed {
            self.ins().fcvt_to_sint_sat(int, v)
        } else {
            self.ins().fcvt_to_uint_sat(int, v)
        };
        VEntry::direct(v)
    }

    pub(super) fn int_to_float(&mut self, n: lir::Value, intsize: IntSize) -> VEntry {
        let v = self.value_to_entry(n).as_direct();
        let v = if intsize.signed {
            self.ins().fcvt_from_sint(types::F64, v)
        } else {
            self.ins().fcvt_from_uint(types::F64, v)
        };
        VEntry::direct(v)
    }

    pub(super) fn iunary<'b, F>(&'b mut self, v: lir::Value, is: IntSize, f: F) -> VEntry
    where
        F: FnOnce(FuncInstBuilder<'b, 'f>, Type, Value) -> Value + 'b,
    {
        let v = self.value_to_entry(v).as_direct();
        let ty = Type::int(is.bits() as u16).unwrap();
        let v = f(self.ins(), ty, v);
        VEntry::direct(v)
    }

    pub(super) fn bit_and(&mut self, [left, right]: [lir::Value; 2]) -> VEntry {
        let [left, right] = [left, right].map(|v| self.value_to_entry(v).as_intable().0);
        let v = self.ins().band(left, right);
        VEntry::direct(v)
    }

    pub(super) fn bit_not(&mut self, v: lir::Value) -> VEntry {
        let v = self.value_to_entry(v).as_direct();
        let v = self.ins().bnot(v);
        VEntry::direct(v)
    }

    pub(super) fn resize_uint(&mut self, n: Value, to: Type) -> Value {
        let has = self.f.type_of_value(n).bytes();

        match has.cmp(&to.bytes()) {
            std::cmp::Ordering::Equal => n,
            std::cmp::Ordering::Less => self.ins().uextend(to, n),
            std::cmp::Ordering::Greater => self.ins().ireduce(to, n),
        }
    }

    pub(super) fn int_cmpi(
        &mut self,
        [left, right]: [lir::Value; 2],
        cmp: Ordering,
        bitsize: IntSize,
    ) -> VEntry {
        let [(left, _), (right, _)] = [left, right].map(|v| self.value_to_entry(v).as_intable());
        let intty = Type::int(bitsize.bits() as u16).unwrap();
        assert_eq!(self.f.type_of_value(left), intty);
        assert_eq!(self.f.type_of_value(right), intty);

        let intcc = match cmp {
            std::cmp::Ordering::Less => IntCC::SignedLessThan,
            std::cmp::Ordering::Equal => IntCC::Equal,
            std::cmp::Ordering::Greater => IntCC::SignedGreaterThan,
        };

        let v = self.ins().icmp(intcc, left, right);

        VEntry::direct(v)
    }

    pub(super) fn int_div(&mut self, [left, right]: [lir::Value; 2], intsize: IntSize) -> VEntry {
        let [(left, lkind), (right, _)] =
            [left, right].map(|v| self.value_to_entry(v).as_intable());
        let v = if intsize.signed {
            self.ins().sdiv(left, right)
        } else {
            self.ins().udiv(left, right)
        };
        VEntry::Scalar(Scalar { point: v, kind: lkind })
    }

    pub(super) fn ibinary<'b>(
        &'b mut self,
        ty: &MonoType,
        [left, right]: [lir::Value; 2],
        signed: fn(FuncInstBuilder<'b, 'f>, Value, Value) -> (Value, Value),
        unsigned: fn(FuncInstBuilder<'b, 'f>, Value, Value) -> (Value, Value),
        simple: fn(FuncInstBuilder<'b, 'f>, Value, Value) -> Value,
    ) -> VEntry {
        let [left, right] = [left, right].map(|v| self.value_to_entry(v).as_intable().0);

        let with_check = match ty {
            MonoType::Monomorphised(mk) => {
                let [int, bool_] = [0, 1]
                    .map(key::Field)
                    .map(|field| self.types()[*mk].as_record()[field].clone());

                assert_eq!(bool_, MonoType::bool());
                Either::Left((*mk, as_int(&int).signed))
            }
            MonoType::Int(_) => Either::Right(ScalarKind::Direct),
            MonoType::Pointer(key) => Either::Right(ScalarKind::Pointer((**key).clone())),
            _ => panic!("invalid return signature for num binop: {ty:?}"),
        };

        match with_check {
            Either::Left((mk, true)) => {
                let (n, c) = signed(self.ins(), left, right);
                let fields = [n, c].into_iter().map(VEntry::direct).collect();
                VEntry::StructFlat(mk, fields)
            }
            Either::Left((mk, false)) => {
                let (n, c) = unsigned(self.ins(), left, right);
                let fields = [n, c].into_iter().map(VEntry::direct).collect();
                VEntry::StructFlat(mk, fields)
            }
            Either::Right(kind) => {
                VEntry::Scalar(Scalar { point: simple(self.ins(), left, right), kind })
            }
        }
    }
}
