use super::*;
use derive_new::new;
use ssa::Entry;

// Scans the LIR or obvious errors
#[derive(new)]
pub struct Debugger<'a> {
    lir: &'a LIR,
    mir: &'a mir::MIR,
    #[new(value = "MonoFunc::from(0)")]
    mfunc: MonoFunc,
}

impl<'a> Debugger<'a> {
    pub fn run(mut self) {
        for (mfunc, func) in self.lir.functions.iter() {
            self.mfunc = mfunc;

            let _span = info_span!("running LIR debugger", entity = func.symbol);
            let _handle = _span.enter();

            for v in func.ssa.iterv() {
                let entry = func.ssa.entry_of(v);
                let exp = func.ssa.type_of(v);
                trace!("{v} = {entry} : {}", self.lir.mono.fmt(exp));
                self.entry(v, exp, entry)
            }
        }
    }

    fn tfmt<T>(&self, v: T) -> MonoFormatter<'_, T> {
        MonoFormatter {
            types: &self.lir.mono.types,
            ro: Some(&self.mir.read_only_table[self.lir.functions[self.mfunc].kind.module()]),
            v,
            funcs: Some(&self.lir.functions),
        }
    }

    #[track_caller]
    fn tcheck(
        &self,
        mut given: impl Iterator<Item = MonoType>,
        mut expected: impl Iterator<Item = &'a MonoType>,
    ) {
        for (g, e) in (&mut given).zip(&mut expected) {
            info!(" · {} {} {}", self.tfmt(&g), "=?".symbol(), self.tfmt(e));
            self.check(&g, e);
        }

        assert_eq!(given.next(), None, "excess parameter");
        assert_eq!(expected.next(), None, "missing parameter");
    }

    #[track_caller]
    fn check(&self, got: &MonoType, exp: &MonoType) {
        if got != exp {
            panic!(
                "LIR type mismatch post-monomorphisation: {} != {}",
                self.tfmt(got),
                self.tfmt(exp)
            );
        }
    }

    #[track_caller]
    fn params(&self, given: &[Value], expected: impl Iterator<Item = &'a MonoType>) {
        let given = given.iter().map(|v| self.lir.type_of_value(self.mfunc, *v));
        self.tcheck(given, expected);
    }

    fn check_declared(&mut self, at: V, value: Value) {
        match value {
            Value::V(v) if v.0 >= at.0 => panic!("{v} used before declared"),
            _ => {}
        }
    }

    fn check_declaredn(&mut self, at: V, values: &[Value]) {
        values.iter().for_each(|v| self.check_declared(at, *v))
    }

    fn entry(&mut self, at: V, exp: &MonoType, entry: &Entry) {
        match entry {
            Entry::CallStatic(mfunc, params) => {
                self.check_declaredn(at, params);

                let expected = self.lir.functions[*mfunc]
                    .ssa
                    .param_types(ssa::Block::entry());

                self.params(params, expected);

                assert_eq!(exp, &self.lir.functions[*mfunc].returns)
            }
            Entry::Transmute(v) => {
                self.check_declared(at, *v);
                let _ty = self.lir.type_of_value(self.mfunc, *v);
            }
            Entry::SizeOf(_) => {
                self.as_int(exp, "size-of");
            }
            Entry::AlignOf(_) => {
                self.as_int(exp, "align-of");
            }
            Entry::CallExtern(fkey, params) => {
                self.check_declaredn(at, params);
                let extern_ = &self.lir.extern_funcs[fkey];
                self.params(params, extern_.params.iter());
                assert_eq!(exp, &extern_.returns);
            }
            Entry::CallValue(to_call, params) => {
                self.check_declaredn(at, params);
                let called = self.lir.type_of_value(self.mfunc, *to_call);
                match called {
                    MonoType::FnPointer(ptypes, ret) => {
                        self.params(params, ptypes.iter());
                        assert_eq!(exp, &*ret);
                    }
                    MonoType::Monomorphised(mkey) => match &self.lir.mono.types[mkey] {
                        MonoTypeData::DynTraitObject { trait_, vtable }
                            if self.mir.name_of_type(*trait_) == "Closure" =>
                        {
                            let (ptypes, ret) = vtable.as_fnptr();
                            self.params(params, ptypes.iter());
                            assert_eq!(exp, ret);
                        }
                        _ => {
                            panic!("CallValue for non-closure: {}", self.tfmt(&called))
                        }
                    },
                    ty => panic!(
                        "CallValue for non-closure or non-fnpointer: {}",
                        self.tfmt(&ty)
                    ),
                }
            }

            Entry::Construct(values) => {
                self.check_declaredn(at, values);

                match exp {
                    MonoType::Array(len, inner) => {
                        assert_eq!(*len, values.len() as u64);
                        for p in values {
                            assert_eq!(self.lir.type_of_value(self.mfunc, *p), **inner);
                        }
                    }
                    MonoType::Monomorphised(mk) => match &self.lir.mono.types[*mk] {
                        MonoTypeData::Record { fields, .. } => self.params(values, fields.values()),
                        MonoTypeData::DynTraitObject { vtable, .. } => {
                            self.params(values, [&MonoType::u8_pointer(), vtable].into_iter());
                        }
                        _ => {
                            panic!("invalid type for construct: {}", self.tfmt(exp))
                        }
                    },
                    _ => panic!("cannot construct non-record: {}", self.tfmt(exp)),
                }
            }
            Entry::Replicate(value, times) => {
                self.check_declared(at, *value);
                match exp {
                    MonoType::Array(len, inner) => {
                        assert_eq!(times, len);
                        assert_eq!(self.lir.type_of_value(self.mfunc, *value), **inner);
                    }
                    MonoType::Monomorphised(_) => todo!(),
                    _ => panic!("cannot replicate non-aggregate: {}", self.tfmt(exp)),
                }
            }
            Entry::TagFromSum { of } => {
                self.check_declared(at, *of);
                let ty = self.lir.type_of_value(self.mfunc, *of);
                let _ = self.lir.mono.types[ty.as_key()].as_sum();
                self.as_int(exp, "tag");
            }
            Entry::Variant(var, elems) => {
                self.check_declaredn(at, elems);
                let (_, _, variants) = self.lir.mono.types[exp.as_key()].as_sum();
                self.params(
                    elems,
                    self.lir.mono.types[variants[*var]].as_record().values(),
                );
            }
            Entry::RefStaticVal(val) => {
                let ty = &self.lir.vals[*val];
                assert_eq!(exp, &MonoType::pointer(ty.clone()));
            }
            Entry::Field { of, key, field } => {
                self.check_declared(at, *of);
                assert_eq!(self.lir.type_of_value(self.mfunc, *of).as_key(), *key);
                match &self.lir.mono.types[*key] {
                    MonoTypeData::Record { fields, .. } => assert_eq!(exp, &fields[*field]),
                    MonoTypeData::DynTraitObject { vtable, .. } => match field {
                        key::Field(0) => assert_eq!(exp, &MonoType::u8_pointer()),
                        key::Field(1) => assert_eq!(exp, vtable),
                        field => panic!("{field}: dyn objects only have two fields"),
                    },
                    other => panic!("field of non-record: {other:?}"),
                }
            }
            Entry::Indice { of, indice } => {
                self.check_declared(at, *of);

                let of_ty = self.lir.type_of_value(self.mfunc, *of);
                let indice_ty = self.lir.type_of_value(self.mfunc, *indice);

                self.as_int(&indice_ty, "array/tuple indice");

                match of_ty {
                    MonoType::Array(_, inner) => {
                        assert_eq!(&*inner, exp);
                    }
                    MonoType::Monomorphised(_) => {}
                    _ => panic!("indice of non- array or tuple"),
                }
            }
            Entry::CastFromSum { of } => {
                self.check_declared(at, *of);

                let of = self.lir.type_of_value(self.mfunc, *of);
                match of {
                    MonoType::Monomorphised(mkey) => {
                        let (_, _, _) = self.lir.mono.types[mkey].as_sum();
                    }
                    _ => panic!("SumField of non-opaque sum data: {}", self.tfmt(&of)),
                }
            }
            Entry::IntCmpInclusive([lhs, rhs], _, _) | Entry::BinOp(_, [lhs, rhs]) => {
                self.check_declared(at, *lhs);
                self.check_declared(at, *rhs);

                let [lhs, rhs] = [lhs, rhs].map(|v| self.lir.type_of_value(self.mfunc, *v));
                match lhs {
                    MonoType::Pointer(inner) => {
                        self.as_int(&rhs, "numeric operator");
                        assert_eq!(&*inner, self.as_ptr(exp));
                    }
                    MonoType::Int(_) => {
                        assert_eq!(lhs, rhs, "{} != {}", self.tfmt(&lhs), self.tfmt(&rhs));
                        self.as_int(&lhs, "numeric operator");
                    }
                    _ => panic!("invalid operand for builtin numeric operation: {lhs:?}"),
                }
            }
            Entry::IntAbs(v) => {
                self.check_declared(at, *v);
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_int(&ty, "iabs");
            }
            Entry::Reduce(v) => {
                self.check_declared(at, *v);
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_int(&ty, "reduce");
            }
            Entry::ExtendSigned(v) => {
                self.check_declared(at, *v);
                let ty = self.lir.type_of_value(self.mfunc, *v);
                assert!(self.as_int(&ty, "extend").signed);
            }
            Entry::ExtendUnsigned(v) => {
                self.check_declared(at, *v);
                let ty = self.lir.type_of_value(self.mfunc, *v);
                assert!(!self.as_int(&ty, "extend").signed);
            }
            Entry::IntToFloat(v, size) => {
                self.check_declared(at, *v);
                let ty = self.lir.type_of_value(self.mfunc, *v);
                assert_eq!(*size, self.as_int(&ty, "cast"));
                self.as_float(exp, "cast");
            }
            Entry::FloatToInt(v, size) => {
                self.check_declared(at, *v);
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_float(&ty, "cast");
                assert_eq!(*size, self.as_int(exp, "cast"));
            }

            Entry::BitNot(v) => {
                self.check_declared(at, *v);
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_int(&ty, "bitnot");
            }
            Entry::BlockParam(block, i) => {
                let ssa = &self.lir.functions[self.mfunc].ssa;
                let v = ssa.get_block_param(*block, *i);
                let ty = ssa.type_of(v);
                assert_eq!(exp, ty);
            }
            Entry::Alloc { .. } => {}
            Entry::Alloca => {}
            Entry::Dealloc { ptr } => {
                self.check_declared(at, *ptr);
                let ty = self.lir.type_of_value(self.mfunc, *ptr);
                self.as_ptr(&ty);
                self.as_unit(exp);
            }
            Entry::WritePtr { ptr, value } => {
                self.check_declared(at, *ptr);
                self.check_declared(at, *value);
                let ty = self.lir.type_of_value(self.mfunc, *ptr);
                let inner = self.as_ptr(&ty);
                let ty = self.lir.type_of_value(self.mfunc, *value);
                assert_eq!(ty, *inner);
                self.as_unit(exp);
            }
            Entry::MemCpy { dst, src, count } => {
                self.check_declared(at, *dst);
                self.check_declared(at, *src);
                self.check_declared(at, *count);
                let dstt = self.lir.type_of_value(self.mfunc, *dst);
                let srct = self.lir.type_of_value(self.mfunc, *src);
                assert_eq!(dstt, srct);
                assert!(matches!(dstt, MonoType::Pointer(..)));
                self.as_unit(exp);
            }
            Entry::Deref(ptr) => {
                self.check_declared(at, *ptr);
                // TODO: I think our casts are currently implicit for pointers. Wwe should probably change that?
                //
                // or no, they probably occur in the intcast stuff?
                let ty = self.lir.type_of_value(self.mfunc, *ptr);
                let inner = self.as_ptr(&ty);
                assert_eq!(exp, inner);
            }
            Entry::JmpFunc(mfunc, params) => {
                let expected = self.lir.functions[*mfunc]
                    .ssa
                    .param_types(ssa::Block::entry());

                self.params(params, expected);
            }
            Entry::JmpBlock(jump) => {
                let expected = self.lir.functions[self.mfunc].ssa.param_types(jump.id);
                self.params(&jump.params, expected)
            }
            Entry::Trap(_) => {}
            Entry::Return(v) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                let exp = &self.lir.functions[self.mfunc].returns;
                self.check(&ty, exp);
            }
            Entry::Select { value, .. } => {
                let ty = self.lir.type_of_value(self.mfunc, *value);
                self.check(&ty, &MonoType::bool());
            }
            Entry::JmpTable(v, _) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_int(&ty, "jump table");
            }
        }
    }

    #[track_caller]
    fn as_unit(&self, ty: &MonoType) {
        assert_eq!(*ty, MonoType::from(UNIT), "non-unit");
    }

    #[track_caller]
    fn as_int(&self, ty: &MonoType, ctx: &str) -> IntSize {
        match ty {
            MonoType::Int(size) => *size,
            _ => panic!("non-int {ctx}: {}", self.tfmt(ty)),
        }
    }

    #[track_caller]
    fn as_float(&self, ty: &MonoType, ctx: &str) {
        match ty {
            MonoType::Float => {}
            _ => panic!("non-float {ctx}: {}", self.tfmt(ty)),
        }
    }

    #[track_caller]
    fn as_ptr<'t>(&self, ty: &'t MonoType) -> &'t MonoType {
        match ty {
            MonoType::Pointer(inner) => inner,
            _ => panic!("as_ptr called on non-pointer: {}", self.tfmt(ty)),
        }
    }
}
