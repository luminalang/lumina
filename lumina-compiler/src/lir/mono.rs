use super::{Function, Item, MonoFunc, MonoTyping, UNIT};
use crate::prelude::*;
use ast::attr::Repr;
use derive_more::{Deref, DerefMut};
use lumina_collections::{map_key_impl, ReadOnlyTable};
use lumina_key as key;
use lumina_typesystem::{
    ConstValue, Container, Forall, Generic, GenericKind, GenericMapper, IntSize, Static,
    Transformer, Ty, Type,
};
use lumina_util::Highlighting;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct MonoTypeKey(pub u32);
map_key_impl!(MonoTypeKey(u32), "mr");

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum MonoType {
    Int(IntSize),
    Pointer(Box<Self>),
    FnPointer(Vec<Self>, Box<Self>),
    Float,
    Unreachable,
    Const(ConstValue),
    Array(u64, Box<Self>),
    Monomorphised(MonoTypeKey),
}

impl From<MonoTypeKey> for MonoType {
    fn from(value: MonoTypeKey) -> Self {
        MonoType::Monomorphised(value)
    }
}

#[derive(Deref, DerefMut)]
pub struct Types {
    #[deref]
    #[deref_mut]
    pub(super) records: Map<MonoTypeKey, MonoTypeData>,

    pub pointer_bits: u32,
}

pub struct MonomorphisedTypes {
    resolve: HashMap<(M<key::TypeKind>, Vec<MonoType>), MonoTypeKey>,
    tuples: HashMap<Vec<MonoType>, MonoTypeKey>,

    pub types: Types,

    closure: M<key::Trait>,
    default_repr: Repr,
}

#[derive(Debug)]
pub enum MonoTypeData {
    Record {
        repr: Repr,
        key: Option<M<key::Record>>,
        fields: Map<key::Field, MonoType>,
    },
    Sum {
        tag: IntSize,
        key: M<key::Sum>,
        variants: Map<key::Variant, MonoTypeKey>,
    },
    DynTraitObject {
        trait_: M<key::Trait>,
        vtable: MonoType,
    },

    // Used for stoppping infinite recursion on depth-first monomorphisation of recursive types.
    Placeholder,
}

impl MonoTypeData {
    pub fn original(&self) -> Option<M<key::TypeKind>> {
        match self {
            MonoTypeData::Record { key, .. } => key.map(|m| m.map(Into::into)),
            MonoTypeData::Sum { key, .. } => Some(key.map(Into::into)),
            MonoTypeData::DynTraitObject { trait_, .. } => Some(trait_.map(Into::into)),
            MonoTypeData::Placeholder => None,
        }
    }

    #[track_caller]
    pub fn as_record(&self) -> &Map<key::Field, MonoType> {
        match self {
            MonoTypeData::Record { fields, .. } => fields,
            other => panic!("not a record: {other:?}"),
        }
    }

    #[track_caller]
    pub fn as_sum(&self) -> (IntSize, M<key::Sum>, &Map<key::Variant, MonoTypeKey>) {
        match self {
            MonoTypeData::Sum { tag, variants, key } => (*tag, *key, variants),
            other => panic!("not a sum: {other:?}"),
        }
    }

    #[track_caller]
    pub fn as_dyn_trait(&self) -> (M<key::Trait>, &MonoType) {
        match self {
            MonoTypeData::DynTraitObject { trait_, vtable } => (*trait_, vtable),
            other => panic!("not a dyn trait: {other:?}"),
        }
    }
}

pub struct MonoFormatter<'a, T> {
    pub types: &'a Map<MonoTypeKey, MonoTypeData>,
    pub funcs: Option<&'a Map<MonoFunc, Function>>,
    pub ro: Option<&'a ReadOnlyTable<key::ReadOnly>>,
    pub v: T,
}

impl<'a, T> MonoFormatter<'a, T> {
    pub fn fns(mut self, funcs: &'a Map<MonoFunc, Function>) -> Self {
        self.funcs = Some(funcs);
        self
    }

    pub fn ros(mut self, ro: &'a ReadOnlyTable<key::ReadOnly>) -> Self {
        self.ro = Some(ro);
        self
    }

    pub fn fork<U>(&self, other: U) -> MonoFormatter<'_, U> {
        MonoFormatter { types: self.types, funcs: self.funcs, ro: self.ro, v: other }
    }
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, &lir::Function> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {} {} =\n{}",
            "fn".keyword(),
            self.v.symbol,
            "returning".keyword(),
            self.fork(&self.v.returns),
            self.fork(&self.v.ssa),
        )
    }
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, &'t MonoType> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.v {
            MonoType::Int(intsize) => write!(f, "{}", intsize),
            MonoType::Pointer(inner) => write!(f, "*{}", self.fork(&**inner)),
            MonoType::FnPointer(params, ret) if params.is_empty() => {
                write!(f, "fnptr({})", self.fork(&**ret))
            }
            MonoType::FnPointer(params, ret) => {
                write!(
                    f,
                    "fnptr({} -> {})",
                    params.iter().map(|t| self.fork(t)).format(", "),
                    self.fork(&**ret),
                )
            }
            MonoType::Const(const_) => const_.fmt(f),
            MonoType::Float => "float".fmt(f),
            MonoType::Unreachable => "!".fmt(f),
            MonoType::Array(len, inner) => write!(f, "[{}; {len}]", self.fork(&**inner)),
            MonoType::Monomorphised(key) => self.fork(*key).fmt(f),
        }
    }
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, MonoTypeKey> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.types.has(self.v) {
            return write!(f, "{}:???", self.v);
        }
        let data = &self.types[self.v];
        match data {
            MonoTypeData::Record { fields, .. } => write!(
                f,
                "({})",
                fields.values().map(|ty| format!("{ty:?}")).format(" * ")
            ),
            MonoTypeData::Sum { variants, tag, .. } => write!(
                f,
                "({tag} * {})",
                variants
                    .values()
                    .map(|params| format!("({})", params))
                    .format(" | ")
            ),
            MonoTypeData::DynTraitObject { vtable, trait_ } => {
                write!(f, "(dyn {trait_} {})", self.fork(vtable))
            }
            MonoTypeData::Placeholder => write!(f, "???"),
        }
    }
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, &'t MonoTyping> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fn {} as {} -> {}",
            self.v.origin,
            self.v.params.values().map(|t| self.fork(t)).format(", "),
            self.fork(&self.v.returns),
        )
    }
}

impl<'a, 't, T: fmt::Display> fmt::Display for MonoFormatter<'a, (T, &'t [MonoType])> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} {})",
            &self.v.0,
            self.v.1.iter().map(|t| self.fork(t)).format(" ")
        )
    }
}

pub fn fmt<'a, T>(types: &'a Map<MonoTypeKey, MonoTypeData>, v: T) -> MonoFormatter<'_, T> {
    MonoFormatter { v, types, ro: None, funcs: None }
}

impl MonomorphisedTypes {
    pub fn new(closure: M<key::Trait>, pointer_size: u32, default: Repr) -> Self {
        let mut types = Self {
            closure,
            resolve: HashMap::new(),
            tuples: HashMap::new(),
            types: Types { records: Map::new(), pointer_bits: pointer_size },
            default_repr: default,
        };
        assert_eq!(UNIT, types.get_or_make_tuple(vec![]));
        types
    }

    pub fn into_records(self) -> Types {
        self.types
    }

    pub fn fmt<T>(&self, v: T) -> MonoFormatter<'_, T> {
        MonoFormatter { v, types: &self.types.records, ro: None, funcs: None }
    }

    pub fn get_or_make_tuple(&mut self, elems: Vec<MonoType>) -> MonoTypeKey {
        if let Some(key) = self.tuples.get(&elems).copied() {
            return key;
        }

        let record = MonoTypeData::Record {
            repr: self.default_repr,
            fields: elems.iter().cloned().collect(),
            key: None,
        };

        let key = self.types.records.push(record);
        self.tuples.insert(elems, key);

        key
    }

    #[cfg(test)]
    pub fn get_or_make_record(
        &mut self,
        rkey: M<key::Record>,
        params: Vec<MonoType>,
        fields: Map<key::Field, MonoType>,
    ) -> MonoTypeKey {
        let key = (rkey.map(key::Record::into), params);
        if let Some(mk) = self.resolve.get(&key).copied() {
            return mk;
        }

        let record = MonoTypeData::Record { repr: self.default_repr, fields, key: Some(rkey) };

        let mk = self.types.records.push(record);
        self.resolve.insert(key, mk);

        mk
    }

    // pub fn fields(&self, ty: MonoRecord) -> impl Iterator<Item = key::Field> + 'static {
    //     self.types[ty].fields.keys()
    // }
}

impl MonoType {
    pub fn unit() -> Self {
        Self::Monomorphised(UNIT)
    }

    pub fn bool() -> Self {
        Self::Int(IntSize::new(false, 8))
    }

    pub fn pointer(to: MonoType) -> MonoType {
        MonoType::Pointer(Box::new(to))
    }

    pub fn u8_pointer() -> MonoType {
        MonoType::pointer(Self::byte())
    }

    pub fn u(bits: u8) -> Self {
        MonoType::Int(IntSize::new(false, bits))
    }
    pub fn byte() -> MonoType {
        MonoType::Int(IntSize::new(false, 8))
    }

    pub fn fn_pointer(params: impl Into<Vec<MonoType>>, ret: MonoType) -> MonoType {
        MonoType::FnPointer(params.into(), Box::new(ret))
    }

    #[track_caller]
    pub fn deref(self) -> MonoType {
        match self {
            Self::Pointer(inner) => *inner,
            ty => panic!("cannot deref non-pointer: {ty:#?}"),
        }
    }

    #[track_caller]
    pub fn as_key(&self) -> MonoTypeKey {
        match self {
            Self::Monomorphised(key) => *key,
            ty => panic!("not a monomorphised type: {ty:#?}"),
        }
    }

    #[track_caller]
    pub fn as_array(&self) -> (u64, MonoType) {
        match self {
            MonoType::Array(len, inner) => (*len, (**inner).clone()),
            ty => panic!("not an array: {ty:?}"),
        }
    }

    pub fn as_fnptr(&self) -> (&[MonoType], &MonoType) {
        match self {
            MonoType::FnPointer(ptypes, ret) => (ptypes.as_slice(), &**ret),
            ty => panic!("not a function pointer: {ty:#?}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeMap {
    pub generics: Vec<(Generic, MonoType)>,
    pub self_: Option<MonoType>,
    pub weak: GenericMapper<Static>,
}

#[derive(new)]
pub struct Monomorphization<'a> {
    pub mono: &'a mut MonomorphisedTypes,

    type_repr: &'a hir::TypeRepr,

    field_types: &'a MMap<key::Record, Map<key::Field, Tr<Type>>>,
    variant_types: &'a MMap<key::Sum, Map<key::Variant, Vec<Tr<Type>>>>,

    // We need this data to correctly monomorphise trait objects.
    //
    // VTables for dynamic dispatch is generated lazily
    methods: &'a MMap<key::Trait, Map<key::Method, key::Func>>,
    funcs: &'a MMap<key::Func, mir::FunctionStatus>,
    trait_objects: &'a MMap<key::Trait, Option<Map<key::Method, key::Param>>>,

    pub tmap: &'a mut TypeMap,
}

macro_rules! fork {
    ($this:ident, $tmap:expr) => {
        Monomorphization::new(
            $this.mono,
            $this.type_repr,
            $this.field_types,
            $this.variant_types,
            $this.methods,
            $this.funcs,
            $this.trait_objects,
            $tmap,
        )
    };
}

impl<'a> Monomorphization<'a> {
    pub fn substitute_generics_for_unit_type<'s>(&mut self, forall: &Forall<'s, Static>) {
        let unit = self.mono.get_or_make_tuple(vec![]);

        forall.generics.keys().for_each(|key| {
            self.tmap.push(
                Generic::new(key, GenericKind::Entity),
                Type::tuple(vec![]),
                MonoType::Monomorphised(unit),
            );
        });
    }

    fn get_or_monomorphise(
        &mut self,
        kind: M<impl Into<key::TypeKind>>,
        params: &[Type],
        gkind: GenericKind,
        or: impl FnOnce(&mut Self, Repr, TypeMap) -> MonoTypeData,
    ) -> MonoTypeKey {
        let kind = kind.map(Into::into);
        let repr = self.type_repr.get(kind);

        let (mut tmap, mparams) = self.new_type_map_by(params, gkind);

        let key = (kind, mparams);

        match self.mono.resolve.get(&key) {
            Some(key) => *key,
            None => {
                let mk = self.mono.types.records.push(MonoTypeData::Placeholder);
                self.mono.resolve.insert(key, mk);
                tmap.set_self(
                    Type::defined(kind, params.to_vec()),
                    MonoType::Monomorphised(mk),
                );
                let record = or(self, repr, tmap);
                self.mono.types[mk] = record;
                mk
            }
        }
    }

    pub fn defined(&mut self, M(module, kind): M<key::TypeKind>, params: &[Type]) -> MonoTypeKey {
        match kind {
            key::TypeKind::Record(k) => self.record(k.inside(module), params),
            key::TypeKind::Sum(k) => self.sum(k.inside(module), params),
            key::TypeKind::Trait(k) => {
                let mparams = self.applys(params);
                self.trait_object(k.inside(module), mparams)
            }
        }
    }

    pub fn record(&mut self, key: M<key::Record>, params: &[Type]) -> MonoTypeKey {
        self.get_or_monomorphise(key, params, GenericKind::Entity, |this, repr, mut tmap| {
            let fields = &this.field_types[key];
            let fields = fork!(this, &mut tmap).applys(fields.values().map(|t| &t.value));
            MonoTypeData::Record { key: Some(key), fields, repr }
        })
    }

    pub fn sum(&mut self, key: M<key::Sum>, params: &[Type]) -> MonoTypeKey {
        self.get_or_monomorphise(key, params, GenericKind::Entity, |this, repr, mut tmap| {
            let tag = match repr {
                Repr::Enum(size) => size,
                Repr::Align(bytes) => IntSize::new(false, bytes * 8),
                _ => IntSize::new(false, 16),
            };

            let variants = &this.variant_types[key];
            let variants = variants
                .values()
                .map(|params| {
                    let elems = fork!(this, &mut tmap).applys(params.iter().map(|t| &t.value));
                    this.mono.get_or_make_tuple(elems)
                })
                .collect();

            MonoTypeData::Sum { tag, variants, key }
        })
    }

    // For closures, the type parameter `p` actually expands from {a,b} to `a,b`
    //
    // This greatly simplifies partial application, but means we need to edge-case them
    // instead of relying on the generalised `trait_object` monomorphisation.
    pub fn closure_object(
        &mut self,
        trait_: M<key::Trait>,
        mut ptypes: Vec<MonoType>,
        ret: MonoType,
    ) -> MonoTypeKey {
        let mut params = ptypes.clone();
        params.push(ret.clone());
        let key = (trait_.map(key::TypeKind::Trait), params);

        if let Some(&key) = self.mono.resolve.get(&key) {
            return key;
        }

        // Reserve in case one of the methods contain the same trait object
        let reserved = self.mono.types.records.push(MonoTypeData::Placeholder);
        self.mono.resolve.insert(key.clone(), reserved);

        ptypes.insert(0, MonoType::u8_pointer());
        let vtable = MonoType::fn_pointer(ptypes, ret);

        self.mono.types[reserved] = MonoTypeData::DynTraitObject { vtable, trait_ };

        reserved
    }

    pub fn trait_object(&mut self, trait_: M<key::Trait>, params: Vec<MonoType>) -> MonoTypeKey {
        let key = (trait_.map(key::TypeKind::Trait), params);

        if let Some(&key) = self.mono.resolve.get(&key) {
            return key;
        }

        // Reserve in case one of the methods contain the same trait object
        let reserved = self.mono.types.records.push(MonoTypeData::Placeholder);
        self.mono.resolve.insert(key.clone(), reserved);

        // For closures we convert `call {a} {b, c}` into `call {a} b c` because it makes partial
        // application a lot easier.
        let vtable = if trait_ == self.mono.closure {
            assert_eq!(key.1.len(), 2);

            let mut ptypes = vec![MonoType::u8_pointer()];

            let param_tuple = key.1[0].as_key();
            for (_, ty) in self.mono.types.records[param_tuple].as_record() {
                ptypes.push(ty.clone());
            }

            let ret = key.1[1].clone();
            MonoType::fn_pointer(ptypes, ret)
        } else {
            let methods = &self.methods[trait_];

            // Create a tmap to monomorphise the generics from the `trait` decl when creating fnpointers
            let mut tmap = TypeMap::new();
            tmap.set_self(Type::u8_pointer(), MonoType::u8_pointer());
            tmap.extend_no_weak(GenericKind::Parent, key.1);

            let mut method_to_fnptr = |func| {
                let typing = self.funcs[M(trait_.0, func)].as_done();

                let mut tmap = tmap.clone();
                let mut morph = fork!(self, &mut tmap);

                let ptypes = morph.applys::<Vec<_>>(&typing.typing.params);
                let ret = morph.apply(&typing.typing.returns);

                MonoType::fn_pointer(ptypes, ret)
            };

            if methods.len() == 1 {
                method_to_fnptr(methods[key::Method(0)])
            } else {
                let fields = methods
                    .values()
                    .map(|func| method_to_fnptr(*func))
                    .collect::<Vec<_>>();

                let vtable = self.mono.get_or_make_tuple(fields);

                MonoType::pointer(vtable.into())
            }
        };

        self.mono.types[reserved] = MonoTypeData::DynTraitObject { vtable, trait_ };

        reserved
    }

    pub fn apply(&mut self, ty: &Type) -> MonoType {
        trace!("monomorphising {ty}");

        match ty {
            Ty::Container(con, params) => match con {
                Container::FnPointer => {
                    let mut params: Vec<_> = self.applys(params);
                    let returns = params.pop().unwrap();
                    MonoType::fn_pointer(params, returns)
                }
                Container::Closure => {
                    let mut params = params.clone();
                    let returns = params.pop().unwrap();

                    let mparams = self.applys(&params);
                    let ret = self.apply(&returns);

                    let object = self.closure_object(self.mono.closure, mparams, ret);

                    MonoType::Monomorphised(object)
                }
                Container::Tuple => {
                    let elems = self.applys(params);
                    MonoType::Monomorphised(self.mono.get_or_make_tuple(elems))
                }
                Container::Pointer => {
                    let inner = self.apply(&params[0]);
                    MonoType::pointer(inner)
                }
                Container::Array => {
                    assert_eq!(params.len(), 2);
                    let inner = self.apply(&params[0]);
                    let MonoType::Const(ConstValue::Usize(len)) = self.apply(&params[1]) else {
                        panic!("non-const-usize for array length post- type-checking");
                    };
                    MonoType::Array(len, Box::new(inner))
                }
                &Container::Defined(M(module, key), _) => match key {
                    key::TypeKind::Record(rkey) => {
                        let mk = self.record(rkey.inside(module), params);
                        MonoType::Monomorphised(mk)
                    }

                    key::TypeKind::Sum(sum) => {
                        let mk = self.sum(sum.inside(module), params);
                        MonoType::Monomorphised(mk)
                    }

                    key::TypeKind::Trait(trait_) => {
                        let params = self.applys(params);
                        let mk = self.trait_object(trait_.inside(module), params);
                        MonoType::Monomorphised(mk)
                    }
                },
            },
            Ty::Const(const_) => MonoType::Const(const_.clone()),
            Ty::Generic(generic) => self.generic(*generic).clone(),
            Ty::Int(intsize) => MonoType::Int(*intsize),
            Ty::Simple("f64") => MonoType::Float,
            Ty::Simple("bool") => MonoType::bool(),
            Ty::Simple("self") => self.tmap.self_.clone().unwrap(),
            _ => panic!("invalid type for LIR: {ty}"),
        }
    }

    pub fn apply_weak(&self, ty: &Type) -> Type {
        (&self.tmap.weak).transform(ty)
    }

    fn new_type_map_by(&mut self, params: &[Type], gkind: GenericKind) -> (TypeMap, Vec<MonoType>) {
        let mut map = TypeMap::new();
        let mut elems = Vec::with_capacity(params.len());
        map.extend(
            gkind,
            params.iter().map(|ty| {
                let mono = self.apply(ty);
                let ty = self.apply_weak(ty);
                elems.push(mono.clone());
                (ty, mono)
            }),
        );
        (map, elems)
    }

    pub fn applys<'t, F: FromIterator<MonoType>>(
        &mut self,
        tys: impl IntoIterator<Item = &'t Type>,
    ) -> F {
        tys.into_iter().map(|ty| self.apply(ty)).collect::<F>()
    }

    pub fn applys_weak<'t, F: FromIterator<Type>>(
        &mut self,
        tys: impl IntoIterator<Item = &'t Type>,
    ) -> F {
        tys.into_iter().map(|ty| self.apply_weak(ty)).collect::<F>()
    }

    pub fn apply_typing(&mut self, origin: Item, typing: &mir::ConcreteTyping) -> MonoTyping {
        MonoTyping {
            origin,
            params: self.applys(typing.params.iter()),
            returns: self.apply(&typing.returns),
        }
    }

    pub fn generic(&self, generic: Generic) -> &MonoType {
        self.tmap
            .generics
            .iter()
            .find_map(|(g, ty)| (*g == generic).then_some(ty))
            .unwrap()
    }
}

impl TypeMap {
    pub fn new() -> Self {
        Self {
            generics: Vec::new(),
            self_: None,
            weak: GenericMapper::new(vec![], None),
        }
    }

    pub fn extend(&mut self, kind: GenericKind, tys: impl IntoIterator<Item = (Type, MonoType)>) {
        for (i, ty) in tys.into_iter().enumerate() {
            let generic = Generic::new(key::Generic(i as u32), kind);
            self.push(generic, ty.0, ty.1);
        }
    }

    pub fn extend_no_weak(&mut self, kind: GenericKind, tys: impl IntoIterator<Item = MonoType>) {
        for (i, ty) in tys.into_iter().enumerate() {
            let generic = Generic::new(key::Generic(i as u32), kind);
            self.generics.push((generic, ty));
        }
    }

    pub fn set_self(&mut self, weak: Type, mono: MonoType) {
        self.self_ = Some(mono);
        self.weak.self_ = Some(weak);
    }

    pub fn push(&mut self, generic: Generic, weak: Type, mono: MonoType) {
        self.generics.push((generic, mono));
        self.weak.push(generic, weak);
    }
}

impl fmt::Debug for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MonoType::Int(intsize) => write!(f, "{intsize}"),
            MonoType::Pointer(ty) => write!(f, "*{ty:?}"),
            MonoType::Const(const_) => write!(f, "{const_}"),
            MonoType::FnPointer(params, ret) => {
                write!(
                    f,
                    "fnptr({} -> {ret:?})",
                    params.iter().map(|t| format!("{t:?}")).format(", ")
                )
            }
            MonoType::Float => write!(f, "f64"),
            MonoType::Unreachable => write!(f, "!"),
            MonoType::Array(len, inner) => write!(f, "[{:?}; {len}]", inner),
            MonoType::Monomorphised(key) => write!(f, "{key}"),
        }
    }
}
