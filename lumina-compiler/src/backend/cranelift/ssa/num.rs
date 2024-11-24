use super::*;
use std::cmp::Ordering;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn float_to_int(&mut self, n: lir::Value, intsize: IntSize) -> VLayout {
        let v = self.value_to_vlayout(n).as_direct();
        let int = Type::int(intsize.bits() as u16).unwrap();
        let v = if intsize.signed {
            self.cins().fcvt_to_sint_sat(int, v)
        } else {
            self.cins().fcvt_to_uint_sat(int, v)
        };
        Layout::direct(v)
    }

    pub(super) fn int_to_float(&mut self, n: lir::Value, intsize: IntSize) -> VLayout {
        let v = self.value_to_vlayout(n).as_direct();
        let v = if intsize.signed {
            self.cins().fcvt_from_sint(types::F64, v)
        } else {
            self.cins().fcvt_from_uint(types::F64, v)
        };
        Layout::direct(v)
    }

    pub(super) fn iunary<'b, F>(&'b mut self, v: lir::Value, is: IntSize, f: F) -> VLayout
    where
        F: FnOnce(FuncInstBuilder<'b, 'f>, Type, Value) -> Value + 'b,
    {
        let v = self.value_to_vlayout(v).as_direct();
        let ty = Type::int(is.bits() as u16).unwrap();
        let v = f(self.cins(), ty, v);
        Layout::direct(v)
    }

    pub(super) fn bit_and(&mut self, [left, right]: [lir::Value; 2]) -> VLayout {
        let [left, right] = [left, right].map(|v| self.value_to_vlayout(v).as_scalar());
        let v = self.cins().band(left, right);
        Layout::direct(v)
    }

    pub(super) fn bit_not(&mut self, v: lir::Value) -> VLayout {
        let v = self.value_to_vlayout(v).as_direct();
        let v = self.cins().bnot(v);
        Layout::direct(v)
    }

    pub(super) fn resize_uint(&mut self, n: Value, to: Type) -> Value {
        let has = self.f.type_of_value(n).bytes();

        match has.cmp(&to.bytes()) {
            std::cmp::Ordering::Equal => n,
            std::cmp::Ordering::Less => self.cins().uextend(to, n),
            std::cmp::Ordering::Greater => self.cins().ireduce(to, n),
        }
    }

    pub(super) fn int_cmpi(
        &mut self,
        [left, right]: [lir::Value; 2],
        cmp: Ordering,
        bitsize: IntSize,
    ) -> VLayout {
        let [left, right] = [left, right].map(|v| self.value_to_vlayout(v).as_scalar());
        let intty = Type::int(bitsize.bits() as u16).unwrap();
        assert_eq!(self.f.type_of_value(left), intty);
        assert_eq!(self.f.type_of_value(right), intty);

        let intcc = match cmp {
            std::cmp::Ordering::Less => IntCC::SignedLessThan,
            std::cmp::Ordering::Equal => IntCC::Equal,
            std::cmp::Ordering::Greater => IntCC::SignedGreaterThan,
        };

        let v = self.cins().icmp(intcc, left, right);

        Layout::direct(v)
    }

    pub(super) fn int_div(&mut self, [left, right]: [lir::Value; 2], intsize: IntSize) -> VLayout {
        let [left, right] = [left, right].map(|v| self.value_to_vlayout(v).as_scalar());
        let v = if intsize.signed {
            self.cins().sdiv(left, right)
        } else {
            self.cins().udiv(left, right)
        };
        Layout::direct(v)
    }

    pub(super) fn ibinary<'b>(
        &'b mut self,
        ty: &MonoType,
        [left, right]: [lir::Value; 2],
        BinOpFuncs { signed, unsigned, simple }: BinOpFuncs<'b, 'f>,
    ) -> VLayout {
        let [left, right] = [left, right].map(|v| self.value_to_vlayout(v).as_scalar());

        match ty {
            MonoType::Int(_) => Layout::direct(simple(self.cins(), left, right)),
            MonoType::Pointer(inner) => {
                Layout::pointer((**inner).clone(), simple(self.cins(), left, right))
            }
            MonoType::Monomorphised(mk) => {
                let [int, bool_] = [0, 1]
                    .map(key::Field)
                    .map(|field| self.types()[*mk].as_record()[field].clone());

                assert_eq!(bool_, MonoType::bool());

                let (n, c) = if as_int(&int).signed {
                    signed(self.cins(), left, right)
                } else {
                    unsigned(self.cins(), left, right)
                };
                let fields = [n, c].into_iter().map(Layout::direct).collect();
                Layout::StructFlat(*mk, fields)
            }
            _ => panic!("invalid return signature for num binop: {ty:?}"),
        }
    }
}

pub fn binops_from_kind<'b, 'f>(op: lir::BinOp) -> BinOpFuncs<'b, 'f> {
    use cranelift_codegen::ir::InstBuilder as F;

    match op {
        lir::BinOp::Add => BinOpFuncs::new(F::sadd_overflow, F::uadd_overflow, F::iadd),
        lir::BinOp::Sub => BinOpFuncs::new(F::ssub_overflow, F::usub_overflow, F::isub),
        lir::BinOp::Mul => BinOpFuncs::new(F::smul_overflow, F::umul_overflow, F::imul),
        lir::BinOp::Div => unreachable!(),
        lir::BinOp::And => unreachable!(),
    }
}

#[derive(new)]
pub(super) struct BinOpFuncs<'b, 'f> {
    signed: fn(FuncInstBuilder<'b, 'f>, Value, Value) -> (Value, Value),
    unsigned: fn(FuncInstBuilder<'b, 'f>, Value, Value) -> (Value, Value),
    simple: fn(FuncInstBuilder<'b, 'f>, Value, Value) -> Value,
}
