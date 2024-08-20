use super::*;
use derive_new::new;
use ssa::Entry;

// Scans the LIR or obvious errors
#[derive(new)]
pub struct Debugger<'a> {
    lir: &'a LIR,
    mir: &'a mir::MIR,
    #[new(value = "MonoFunc::from_u32(0)")]
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
                self.flow(flow);
            }
        }
    }

    fn tfmt<T>(&self, v: T) -> MonoFormatter<'_, T> {
        MonoFormatter { types: &self.lir.mono.types, v }
    }

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
                let ty = self.lir.type_of_value(self.mfunc, *v);
                let size = self.lir.mono.types.size_of(&ty);
                let esize = self.lir.mono.types.size_of(exp);
                assert_eq!(
                    size, esize,
                    "both sides of transmute need to be of equal size: {size} != {esize}"
                );
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
                    MonoType::Monomorphised(mkey)
                        if self
                            .lir
                            .mono
                            .types
                            .as_trait_object(mkey)
                            .map(|key| self.mir.name_of_type(key))
                            == Some("Closure") =>
                    {
                        let ty = self.lir.mono.types.as_closure_get_fnptr(mkey);
                        let (ptypes, ret) = ty.as_fnptr();
                        self.params(params, ptypes.iter());
                        assert_eq!(exp, ret);
                    }
                    ty => panic!(
                        "CallValue for non-closure or non-fnpointer: {}",
                        self.tfmt(&ty)
                    ),
                }
            }
            Entry::Copy(v) => assert_eq!(exp, &self.lir.type_of_value(self.mfunc, *v)),
            Entry::Construct(values) => match exp {
                MonoType::SumDataCast { largest } => {
                    let size = values
                        .iter()
                        .map(|v| {
                            self.lir
                                .mono
                                .types
                                .size_of(&self.lir.type_of_value(self.mfunc, *v))
                        })
                        .sum::<u32>();
                    assert!(size <= *largest);
                    todo!();
                }
                MonoType::Monomorphised(mk) => {
                    let fields = self.lir.mono.fields(*mk);
                    self.params(
                        values,
                        fields.map(|field| &self.lir.mono.types[*mk].fields[field]),
                    );
                }
                ty => panic!("cannot construct non-record: {}", self.tfmt(ty)),
            },
            Entry::RefStaticVal(val) => {
                let ty = &self.lir.vals[*val];
                assert_eq!(exp, ty);
            }
            Entry::Field { of, key, field } => {
                assert_eq!(self.lir.type_of_value(self.mfunc, *of).as_key(), *key);
                let ty = &self.lir.mono.types[*key].fields[*field];
                assert_eq!(exp, ty);
            }
            Entry::SumField { of, offset } => {
                let of = self.lir.type_of_value(self.mfunc, *of);
                match of {
                    MonoType::SumDataCast { largest } => assert!(offset.0 < largest),
                    _ => panic!("SumField of non-opaque sum data: {}", self.tfmt(&of)),
                }
            }
            Entry::IntCmpInclusive(lhs, _, rhs, _)
            | Entry::IntAdd(lhs, rhs)
            | Entry::IntSub(lhs, rhs)
            | Entry::IntMul(lhs, rhs)
            | Entry::IntDiv(lhs, rhs) => {
                let [lhs, rhs] = [lhs, rhs].map(|v| self.lir.type_of_value(self.mfunc, *v));
                assert_eq!(lhs, rhs, "{} != {}", self.tfmt(&lhs), self.tfmt(&rhs));
                self.as_int(&lhs, "div");
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
        assert_eq!(self.lir.mono.types.size_of(ty), 0, "non-unit");
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
