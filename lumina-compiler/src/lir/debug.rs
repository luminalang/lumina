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

            for block in func.blocks.blocks() {
                for v in func.blocks.entries(block) {
                    let entry = func.blocks.entry_of(v);
                    info!("{v} = {entry}");
                    let exp = func.blocks.type_of(v);
                    self.entry(exp, entry)
                }

                let flow = func.blocks.flow_of(block);
                info!("{flow}");
                self.flow(flow);
            }
        }
    }

    fn tfmt<T>(&self, v: T) -> MonoFormatter<'_, T> {
        MonoFormatter { types: &self.lir.mono.types, v }
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

        assert_eq!(given.next(), None);
        assert_eq!(expected.next(), None);
    }

    #[track_caller]
    fn check(&self, got: &MonoType, exp: &MonoType) {
        if got != exp {
            dbg!(&got, &exp);
            panic!(
                "LIR type mismatch post-monomorphisation: {} != {}",
                self.tfmt(got),
                self.tfmt(exp)
            );
        }
    }

    fn flow(&mut self, flow: &ControlFlow) {
        match flow {
            ControlFlow::JmpFunc(mfunc, params) => {
                let expected = self.lir.functions[*mfunc]
                    .blocks
                    .param_types(ssa::Block::entry());

                self.params(params, expected);
            }
            ControlFlow::JmpBlock(block, params) => {
                let expected = self.lir.functions[self.mfunc].blocks.param_types(*block);
                self.params(params, expected)
            }
            ControlFlow::Unreachable => {}
            ControlFlow::Empty => {}
            ControlFlow::Return(v) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                let exp = &self.lir.functions[self.mfunc].returns;
                self.check(&ty, exp);
            }
            ControlFlow::Select { value, .. } => {
                let ty = self.lir.type_of_value(self.mfunc, *value);
                self.check(&ty, &MonoType::bool());
            }
            ControlFlow::JmpTable(v, _, _) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_int(&ty, "jump table");
            }
        }
    }

    #[track_caller]
    fn params(&self, given: &[Value], expected: impl Iterator<Item = &'a MonoType>) {
        let given = given.iter().map(|v| self.lir.type_of_value(self.mfunc, *v));
        self.tcheck(given, expected);
    }

    fn entry(&mut self, exp: &MonoType, entry: &Entry) {
        match entry {
            Entry::CallStatic(mfunc, params) => {
                let expected = self.lir.functions[*mfunc]
                    .blocks
                    .param_types(ssa::Block::entry());

                self.params(params, expected);

                assert_eq!(exp, &self.lir.functions[*mfunc].returns)
            }
            Entry::Transmute(v) => {
                let _ty = self.lir.type_of_value(self.mfunc, *v);
            }
            Entry::SizeOf(_) => {
                self.as_int(exp, "size-of");
            }
            Entry::CallExtern(fkey, params) => {
                let extern_ = &self.lir.extern_funcs[fkey];
                self.params(params, extern_.params.iter());
                assert_eq!(exp, &extern_.returns);
            }
            Entry::CallValue(to_call, params) => {
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
            Entry::Copy(v) => assert_eq!(exp, &self.lir.type_of_value(self.mfunc, *v)),
            Entry::Construct(values) => match exp {
                MonoType::Monomorphised(mk) => match &self.lir.mono.types[*mk] {
                    MonoTypeData::Record { fields, .. } => self.params(values, fields.values()),
                    MonoTypeData::DynTraitObject { vtable, .. } => {
                        self.params(values, [&MonoType::u8_pointer(), vtable].into_iter());
                    }
                    _ => {
                        panic!("invalid type for construct: {exp:?}")
                    }
                },
                ty => panic!("cannot construct non-record: {}", self.tfmt(ty)),
            },
            Entry::TagFromSum { of } => {
                let ty = self.lir.type_of_value(self.mfunc, *of);
                let _ = self.lir.mono.types[ty.as_key()].as_sum();
                self.as_int(exp, "tag");
            }
            Entry::Variant(var, elems) => {
                let (_, variants) = self.lir.mono.types[exp.as_key()].as_sum();
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
            Entry::CastFromSum { of } => {
                let of = self.lir.type_of_value(self.mfunc, *of);
                match of {
                    MonoType::Monomorphised(mkey) => {
                        let (_, _) = self.lir.mono.types[mkey].as_sum();
                    }
                    _ => panic!("SumField of non-opaque sum data: {}", self.tfmt(&of)),
                }
            }
            Entry::IntCmpInclusive(lhs, _, rhs, _)
            | Entry::IntAdd(lhs, rhs)
            | Entry::IntSub(lhs, rhs)
            | Entry::IntMul(lhs, rhs)
            | Entry::IntDiv(lhs, rhs) => {
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
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_int(&ty, "iabs");
            }
            Entry::Reduce(v) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_int(&ty, "reduce");
            }
            Entry::ExtendSigned(v) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                assert!(self.as_int(&ty, "extend").signed);
            }
            Entry::ExtendUnsigned(v) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                assert!(!self.as_int(&ty, "extend").signed);
            }
            Entry::IntToFloat(v, size) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                assert_eq!(*size, self.as_int(&ty, "cast"));
                self.as_float(exp, "cast");
            }
            Entry::FloatToInt(v, size) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_float(&ty, "cast");
                assert_eq!(*size, self.as_int(exp, "cast"));
            }
            Entry::BitAnd(values) => values
                .map(|v| self.lir.type_of_value(self.mfunc, v))
                .iter()
                .chain(std::iter::once(exp))
                .for_each(|v| assert_eq!(self.as_int(v, "bitand").bits(), 8)),
            Entry::BitNot(v) => {
                let ty = self.lir.type_of_value(self.mfunc, *v);
                self.as_int(&ty, "bitnot");
            }
            Entry::BlockParam(v) => {
                let ty = self.lir.functions[self.mfunc].blocks.type_of(*v);
                assert_eq!(exp, ty);
            }
            Entry::Alloc { .. } => {}
            Entry::Dealloc { ptr } => {
                let ty = self.lir.type_of_value(self.mfunc, *ptr);
                self.as_ptr(&ty);
                self.as_unit(exp);
            }
            Entry::WritePtr { ptr, value } => {
                let ty = self.lir.type_of_value(self.mfunc, *ptr);
                let inner = self.as_ptr(&ty);
                let ty = self.lir.type_of_value(self.mfunc, *value);
                assert_eq!(ty, *inner);
                self.as_unit(exp);
            }
            Entry::Deref(ptr) => {
                // TODO: I think our casts are currently implicit for pointers. Wwe should probably change that?
                //
                // or no, they probably occur in the intcast stuff?
                let ty = self.lir.type_of_value(self.mfunc, *ptr);
                let inner = self.as_ptr(&ty);
                assert_eq!(exp, inner);
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
            _ => panic!("dealloc of non-pointer: {}", self.tfmt(ty)),
        }
    }
}
