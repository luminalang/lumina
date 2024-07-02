use super::{FuncOrigin, MonoFunc, MonoTyping, UNIT};
use crate::prelude::*;
use crate::{TRAIT_OBJECT_DATA_FIELD, VTABLE_FIELD};
use derive_more::{Deref, DerefMut};
use key::{entity_impl, keys};
use lumina_key as key;
use lumina_typesystem::{Bitsize, Container, Forall, FuncKind, Generic, GenericKind, Prim, Type};
use lumina_util::Highlighting;
use std::collections::HashSet;
use std::fmt;

pub const TAG_SIZE: Bitsize = Bitsize(32);
pub const SUM_VARIANT_CHUNK_SIZE: Bitsize = Bitsize(8);

keys! {
    MonoTypeKey . "mtkey",
    BitOffset . "offset"
}

impl From<Bitsize> for BitOffset {
    fn from(value: Bitsize) -> Self {
        BitOffset(value.0 as u32)
    }
}

impl From<u32> for BitOffset {
    fn from(value: u32) -> Self {
        BitOffset(value as u32)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum MonoType {
    Int(Bitsize),
    UInt(Bitsize),
    SumDataCast { largest: u32 },
    Array(Box<Self>, usize),
    Pointer(Box<Self>),
    FnPointer(Vec<Self>, Box<Self>),
    Float,
    Unreachable,
    Monomorphised(MonoTypeKey),
}

impl From<MonoTypeKey> for MonoType {
    fn from(value: MonoTypeKey) -> Self {
        MonoType::Monomorphised(value)
    }
}

#[derive(Deref, DerefMut)]
pub struct Records(Map<MonoTypeKey, MonomorphisedRecord>);

pub struct MonomorphisedTypes {
    resolve: HashMap<(M<key::TypeKind>, Vec<MonoType>), MonoTypeKey>,
    tuples: HashMap<Vec<MonoType>, MonoTypeKey>,

    pub types: Records,

    closure: M<key::Trait>,
    // we can just use `resolve`
    // trait_objects: HashMap<(M<key::Trait>, Vec<MonoType>), MonoTypeKey>,
}

pub struct MonomorphisedRecord {
    pub size: u32,
    pub repr: Repr,
    pub fields: Map<key::RecordField, MonoType>,
    pub autoboxed: HashSet<key::RecordField>,

    // Used to detect circular structure that need indirection
    original: Option<M<key::TypeKind>>,
}

impl MonomorphisedRecord {
    fn placeholder() -> Self {
        Self {
            size: u32::MAX,
            repr: Repr::Lumina,
            fields: Map::new(),
            autoboxed: HashSet::new(),
            original: None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Repr {
    Transparent,
    Lumina,
}

pub struct MonoFormatter<'a, T> {
    pub types: &'a Map<MonoTypeKey, MonomorphisedRecord>,
    pub v: T,
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, &lir::Function> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {} {} =\n{}",
            "fn".keyword(),
            self.v.symbol,
            "as".keyword(),
            fmt(&self.types, &self.v.returns),
            fmt(&self.types, &self.v.blocks)
        )
    }
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, &'t MonoType> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.v {
            MonoType::Int(bit) => write!(f, "i{bit}"),
            MonoType::UInt(bit) => write!(f, "u{bit}"),
            MonoType::Array(inner, len) => write!(f, "[{}; {len}]", fmt(self.types, &**inner)),
            MonoType::Pointer(inner) => write!(f, "*{}", fmt(self.types, &**inner)),
            MonoType::FnPointer(params, ret) if params.is_empty() => {
                write!(f, "fnptr({})", fmt(self.types, &**ret))
            }
            MonoType::FnPointer(params, ret) => {
                write!(
                    f,
                    "fnptr({} -> {})",
                    params.iter().map(|t| fmt(self.types, t)).format(", "),
                    fmt(self.types, &**ret)
                )
            }
            MonoType::Float => "float".fmt(f),
            MonoType::Unreachable => "!".fmt(f),
            MonoType::Monomorphised(key) => fmt(self.types, *key).fmt(f),
            MonoType::SumDataCast { largest, .. } => write!(f, "[sum_data; {largest}]"),
        }
    }
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, MonoTypeKey> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let data = &self.types[self.v];
        write!(
            f,
            "{{{}{}}}",
            match data.original {
                Some(key) => format!("{} ", key),
                None => "".into(),
            }
            .keyword(),
            data.fields.values().map(|v| fmt(self.types, v)).format(" ")
        )
    }
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, &'t MonoTyping> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fn {} as ({} -> {})",
            self.v.origin,
            self.v
                .params
                .values()
                .map(|t| fmt(self.types, t))
                .format(", "),
            fmt(self.types, &self.v.returns)
        )
    }
}

impl<'a, 't, T: fmt::Display> fmt::Display for MonoFormatter<'a, (T, &'t [MonoType])> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} {})",
            &self.v.0,
            self.v.1.iter().map(|t| fmt(self.types, t)).format(" ")
        )
    }
}

pub fn fmt<'a, T>(types: &'a Map<MonoTypeKey, MonomorphisedRecord>, v: T) -> MonoFormatter<'_, T> {
    MonoFormatter { v, types }
}

impl Records {
    pub fn as_sum_type(&self, mk: MonoTypeKey) -> Option<(u32, u32)> {
        if self[mk].fields.len() != 2 {
            return None;
        }

        let tag = &self[mk].fields[key::RecordField(0)];
        let tag_size = match tag {
            MonoType::Int(bitsize) | MonoType::UInt(bitsize) => bitsize.0 as u32,
            _ => return None,
        };

        let data = &self[mk].fields[key::RecordField(1)];
        match data {
            MonoType::SumDataCast { largest } => Some((tag_size, *largest)),
            _ => None,
        }
    }

    pub fn as_trait_object(&self, mk: MonoTypeKey) -> Option<M<key::Trait>> {
        match self[mk].original {
            Some(M { value: key::TypeKind::Trait(tr), module }) => Some(M { value: tr, module }),
            _ => None,
        }
    }

    fn push_record(
        &mut self,
        fields: Map<key::RecordField, MonoType>,
        original: Option<M<key::TypeKind>>,
    ) -> MonoTypeKey {
        let record = MonomorphisedRecord {
            size: fields.values().map(|ty| self.size_of(ty)).sum(),
            repr: Repr::Lumina,
            autoboxed: original
                .map(|key| {
                    fields
                        .iter()
                        .filter_map(|(field, ty)| self.field_is_recursive(key, ty).then_some(field))
                        .collect()
                })
                .unwrap_or_else(HashSet::new),
            fields,
            original,
        };
        self.push(record)
    }

    fn field_is_recursive(&self, key: M<key::TypeKind>, ty: &MonoType) -> bool {
        match ty {
            MonoType::Monomorphised(mk) if self[*mk].original == Some(key) => true,
            MonoType::Monomorphised(mk) => self[*mk]
                .fields
                .values()
                .any(|ty| self.field_is_recursive(key, ty)),
            _ => false,
        }
    }

    pub fn size_of(&self, ty: &MonoType) -> u32 {
        match ty {
            MonoType::SumDataCast { .. } | MonoType::Pointer(_) => 64,
            MonoType::Int(bitsize) => bitsize.0 as u32,
            MonoType::UInt(bitsize) => bitsize.0 as u32,
            MonoType::Array(inner, times) => self.size_of(&inner) * *times as u32,
            MonoType::Float => 64,
            MonoType::FnPointer(_, _) => 64,
            MonoType::Unreachable => 0,
            MonoType::Monomorphised(key) => self.size_of_defined(*key),
        }
    }

    pub fn size_of_defined(&self, key: MonoTypeKey) -> u32 {
        let size = self[key].size;

        // recursive; will be autoboxed. So; we take pointer size
        if size == u32::MAX {
            self.size_of(&MonoType::u8_pointer())
        } else {
            size
        }
    }

    pub fn field_offset(&self, ty: MonoTypeKey, field: key::RecordField) -> BitOffset {
        let ty = &self[ty];

        if !ty.autoboxed.is_empty() {
            panic!("offsets are wrong on recursive types since we don't respect autobox. we should just mark the type itself");
        }

        match ty.repr {
            Repr::Lumina | Repr::Transparent => {
                let mut offset = BitOffset(0);

                for f in 0..field.0 {
                    let f = key::RecordField(f);
                    let ty = &ty.fields[f];
                    offset.0 += self.size_of(ty) as u32;
                }

                offset
            }
        }
    }

    pub fn type_of_field(&self, ty: MonoTypeKey, field: key::RecordField) -> MonoType {
        self[ty].fields[field].clone()
    }

    pub fn vtable_of_object(&self, object: MonoTypeKey) -> MonoTypeKey {
        self.type_of_field(object, VTABLE_FIELD).deref().as_key()
    }

    pub fn get_dyn_method<F: FromIterator<MonoType>>(
        &self,
        table: MonoTypeKey,
        method: key::Method,
    ) -> (F, MonoType) {
        match &self[table].fields[key::RecordField(method.0)] {
            MonoType::FnPointer(ptypes, returns) => {
                (ptypes.iter().cloned().collect(), (**returns).clone())
            }
            _ => unreachable!(),
        }
    }

    pub fn has_field(&self, ty: MonoTypeKey, field: key::RecordField) -> bool {
        self[ty].fields.get(field).is_some()
    }
}

impl MonomorphisedTypes {
    pub fn new(closure: M<key::Trait>) -> Self {
        let mut types = Self {
            closure,
            resolve: HashMap::new(),
            tuples: HashMap::new(),
            types: Records(Map::new()),
        };
        assert_eq!(UNIT, types.get_or_make_tuple(vec![]));
        types
    }

    pub fn into_records(self) -> Records {
        self.types
    }

    pub fn fmt<T>(&self, v: T) -> MonoFormatter<'_, T> {
        MonoFormatter { v, types: &self.types }
    }

    pub fn get_or_make_tuple(&mut self, elems: Vec<MonoType>) -> MonoTypeKey {
        if let Some(key) = self.tuples.get(&elems).copied() {
            return key;
        }

        let record = MonomorphisedRecord {
            size: elems.iter().map(|ty| self.types.size_of(ty)).sum(),
            repr: Repr::Lumina,
            autoboxed: HashSet::new(),
            fields: elems.iter().cloned().collect(),
            original: None,
        };

        let key = self.types.push(record);
        self.tuples.insert(elems, key);

        key
    }

    pub fn get_or_make_record<K: Into<key::TypeKind>>(
        &mut self,
        kind: M<K>,
        elems: Vec<MonoType>,
    ) -> MonoTypeKey {
        let kind = kind.map(|k| k.into());

        let pair = (kind, elems);
        if let Some(key) = self.resolve.get(&pair) {
            return *key;
        }

        let record = MonomorphisedRecord {
            size: pair.1.iter().map(|ty| self.types.size_of(ty)).sum(),
            repr: Repr::Lumina,
            autoboxed: HashSet::new(),
            fields: pair.1.iter().cloned().collect(),
            original: Some(kind),
        };

        let key = self.types.push(record);
        self.resolve.insert(pair, key);
        key
    }

    pub fn fields(&self, ty: MonoTypeKey) -> impl Iterator<Item = key::RecordField> + 'static {
        self.types[ty].fields.keys()
    }
}

impl MonoType {
    pub fn bool() -> Self {
        Self::UInt(Bitsize(8))
    }

    pub fn pointer(to: MonoType) -> MonoType {
        MonoType::Pointer(Box::new(to))
    }

    pub fn u8_pointer() -> MonoType {
        MonoType::pointer(MonoType::UInt(Bitsize(8)))
    }

    pub fn fn_pointer(params: impl Into<Vec<MonoType>>, ret: MonoType) -> MonoType {
        MonoType::FnPointer(params.into(), Box::new(ret))
    }

    pub fn byte_array(len: usize) -> Self {
        Self::Array(Box::new(Self::UInt(Bitsize(8))), len)
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

    pub fn as_fnptr(&self) -> (&[MonoType], &MonoType) {
        match self {
            MonoType::FnPointer(ptypes, ret) => (ptypes.as_slice(), &**ret),
            ty => panic!("not a function pointer: {ty:#?}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeMap {
    pub generics: Vec<(Generic, (Type, MonoType))>,
    pub self_: Option<(Type, MonoType)>,
}

#[derive(new)]
pub struct Monomorphization<'a> {
    pub mono: &'a mut MonomorphisedTypes,

    field_types: &'a ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    variant_types: &'a ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,

    // We need this data to correctly monomorphise trait objects.
    //
    // VTables for dynamic dispatch is generated lazily
    methods: &'a ModMap<key::Trait, Map<key::Method, key::Func>>,
    funcs: &'a ModMap<key::Func, mir::FunctionStatus>,
    trait_objects: &'a ModMap<key::Trait, Option<Map<key::Method, key::Param>>>,

    pub tmap: &'a mut TypeMap,
}

macro_rules! fork {
    ($this:ident, $tmap:expr) => {
        Monomorphization::new(
            $this.mono,
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
    pub fn substitute_generics_for_unit_type<'s>(&mut self, forall: &Forall<'s, Type>) {
        let unit = self.mono.get_or_make_tuple(vec![]);

        forall.keys().for_each(|key| {
            self.tmap.generics.push((
                Generic::new(key, GenericKind::Entity),
                (
                    Type::Container(lumina_typesystem::Container::Tuple(vec![])),
                    MonoType::Monomorphised(unit),
                ),
            ));
        });
    }

    fn get_or_monomorphise(
        &mut self,
        key: M<impl Into<key::TypeKind>>,
        params: &[Type],
        gkind: GenericKind,
        or: impl FnOnce(&mut Self, TypeMap) -> MonomorphisedRecord,
    ) -> MonoTypeKey {
        let key = key.map(Into::into);
        let (mut tmap, params) = self.new_type_map_by(params, gkind);

        let key = (key, params);

        match self.mono.resolve.get(&key) {
            Some(key) => *key,
            None => {
                let mk = self.mono.types.push(MonomorphisedRecord::placeholder());
                self.mono.resolve.insert(key, mk);
                tmap.self_ = Some((Type::Self_, MonoType::Monomorphised(mk)));
                let record = or(self, tmap);
                assert_eq!(self.mono.types[mk].size, u32::MAX);
                self.mono.types[mk] = record;
                mk
            }
        }
    }

    pub fn create_capture_record<T>(
        &mut self,
        captures: &Map<key::Capture, (T, MonoType)>,
    ) -> MonoTypeKey {
        let fields = captures.values().map(|(_, t)| t.clone()).collect();
        let record = self.construct::<key::Record>(None, fields, Repr::Lumina);
        self.mono.types.push(record)
    }

    fn construct<K: Into<key::TypeKind>>(
        &mut self,
        original: Option<M<K>>,
        fields: Map<key::RecordField, MonoType>,
        repr: Repr,
    ) -> MonomorphisedRecord {
        let original = original.map(|k| k.map(Into::into));
        MonomorphisedRecord {
            size: fields.values().map(|ty| self.mono.types.size_of(ty)).sum(),
            repr,
            autoboxed: original
                .map(|key| {
                    fields
                        .iter()
                        .filter_map(|(field, ty)| {
                            self.mono.types.field_is_recursive(key, ty).then_some(field)
                        })
                        .collect()
                })
                .unwrap_or_else(HashSet::new),
            fields,
            original,
        }
    }

    pub fn record(&mut self, key: M<key::Record>, params: &[Type]) -> MonoTypeKey {
        self.get_or_monomorphise(key, params, GenericKind::Entity, |this, mut tmap| {
            let fields = &this.field_types[key];
            let fields = fork!(this, &mut tmap).applys(fields.values().map(|t| &t.value));

            this.construct(Some(key), fields, Repr::Lumina)
        })
    }

    // Sumtypes are lowered into a record containing a tag and an array of bytes sized
    // by the largest variant.
    //
    // Those arrays of bytes are then casted into the appropriate types dynamically in
    // the switch onto the tag.
    pub fn sum(&mut self, key: M<key::Sum>, params: &[Type]) -> MonoTypeKey {
        self.get_or_monomorphise(key, params, GenericKind::Entity, |this, mut tmap| {
            let mut morph = fork!(this, &mut tmap);

            let variants = &this.variant_types[key];
            let variants = variants
                .values()
                .map(|types| types.iter().map(|t| morph.apply(&**t)).collect())
                .collect::<Map<key::SumVariant, Vec<MonoType>>>();

            // The size of the enum is the size of it's largest field plus the tag
            let largest = variants
                .values()
                .map(|params| {
                    params
                        .iter()
                        .map(|ty| this.mono.types.size_of(ty) as u32)
                        .sum::<u32>()
                })
                .max()
                .unwrap();

            let fields = [MonoType::UInt(TAG_SIZE), MonoType::SumDataCast { largest }]
                .into_iter()
                .collect();

            this.construct(Some(key), fields, Repr::Lumina)
        })
    }

    pub fn trait_object(&mut self, trait_: M<key::Trait>, params: &[Type]) -> MonoTypeKey {
        let mparams = self.applys(params);
        let key = (trait_.map(key::TypeKind::Trait), mparams);

        if let Some(&key) = self.mono.resolve.get(&key) {
            return key;
        }

        // Reserve in case one of the methods contain the same trait object
        let reserved = self.mono.types.push(MonomorphisedRecord::placeholder());
        self.mono.resolve.insert(key.clone(), reserved);

        // For closures we convert `call {a} {b, c}` into `call {a} b c` because it makes partial
        // application a lot easier.
        let fields = if trait_ == self.mono.closure {
            assert_eq!(key.1.len(), 2);

            let mut ptypes = vec![MonoType::u8_pointer()];

            let param_tuple = key.1[0].as_key();
            for field in self.mono.fields(param_tuple) {
                let ty = self.mono.types.type_of_field(param_tuple, field);
                ptypes.push(ty);
            }

            let ret = key.1[1].clone();
            let call = MonoType::fn_pointer(ptypes, ret);
            vec![call]
        } else {
            let methods = &self.methods[trait_];

            // Create a tmap to monomorphise the generics from the `trait` decl when creating fnpointers
            let mut tmap = TypeMap::new();
            tmap.self_ = Some((Type::u8_ptr(), MonoType::u8_pointer()));
            for (i, (ty, weak)) in key.1.iter().zip(params).enumerate() {
                let generic = Generic::new(key::Generic(i as u32), GenericKind::Parent);
                tmap.generics.push((generic, (weak.clone(), ty.clone())));
            }

            methods
                .values()
                .map(|func| {
                    let typing = self.funcs[trait_.module.m(*func)].as_done();

                    let mut tmap = tmap.clone();
                    let mut morph = fork!(self, &mut tmap);

                    let ptypes = morph.applys::<Vec<_>>(&typing.typing.params);
                    let ret = morph.apply(&typing.typing.returns);

                    MonoType::fn_pointer(ptypes, ret)
                })
                .collect::<Vec<_>>()
        };

        let vtable = self.mono.get_or_make_tuple(fields);

        // Declare the trait object to be a record of `*u8 + *vtable`
        let mut object_fields = Map::new();
        object_fields.push(MonoType::u8_pointer());
        object_fields.push(MonoType::pointer(vtable.into()));
        let rdata = &mut self.mono.types[reserved];
        *rdata = MonomorphisedRecord {
            size: 64 * 2, // TODO: platform-specific pointer size
            repr: Repr::Lumina,
            fields: object_fields,
            autoboxed: HashSet::new(),
            original: Some(key.0),
        };

        reserved
    }

    pub fn apply(&mut self, ty: &Type) -> MonoType {
        trace!("monomorphising {ty}");

        match ty {
            Type::Container(container) => match container {
                Container::Func(kind, params, returns) => match kind {
                    // trait Closure p r
                    //   fn call as self, p -> r
                    FuncKind::Closure => {
                        let params = vec![
                            Type::Container(Container::Tuple(params.clone())),
                            (**returns).clone(),
                        ];

                        let object = self.trait_object(self.mono.closure, &params);
                        MonoType::Monomorphised(object)
                    }
                    FuncKind::FnPointer => {
                        let params = self.applys(params);
                        let ret = self.apply(returns);
                        MonoType::FnPointer(params, Box::new(ret))
                    }
                },
                Container::Tuple(elems) => {
                    let elems = self.applys(elems);
                    MonoType::Monomorphised(self.mono.get_or_make_tuple(elems))
                }
                Container::Pointer(inner) => MonoType::pointer(self.apply(&inner)),
            },
            Type::Prim(prim) => match prim {
                Prim::Float => MonoType::Float,
                Prim::Bool => MonoType::UInt(Bitsize(8)),
                Prim::Int(true, bit) => MonoType::Int(*bit),
                Prim::Int(false, bit) => MonoType::UInt(*bit),
                Prim::Never => unreachable!(),
                Prim::Poison => unreachable!(),
            },
            Type::Generic(generic) => self.generic(*generic).1.clone(),
            Type::Defined(key, params) | Type::List(key, params) => match key.value {
                key::TypeKind::Record(rkey) => {
                    let mk = self.record(key.module.m(rkey), params);
                    MonoType::Monomorphised(mk)
                }

                key::TypeKind::Sum(sum) => {
                    let mk = self.sum(key.module.m(sum), params);
                    MonoType::Monomorphised(mk)
                }

                key::TypeKind::Trait(trait_) => {
                    let mk = self.trait_object(key.module.m(trait_), params);
                    MonoType::Monomorphised(mk)
                }
            },
            Type::Self_ => self.tmap.self_.clone().unwrap().1,
        }
    }

    pub fn apply_weak(&self, ty: &Type) -> Type {
        match ty {
            Type::Container(con) => Type::Container(con.map(|t| self.apply_weak(t))),
            Type::Prim(prim) => Type::Prim(prim.clone()),
            Type::Generic(generic) => self.generic(*generic).0.clone(),
            Type::List(key, params) | Type::Defined(key, params) => {
                Type::Defined(*key, params.iter().map(|ty| self.apply_weak(ty)).collect())
            }
            Type::Self_ => self.tmap.self_.clone().unwrap().0,
        }
    }

    fn new_type_map_by(&mut self, params: &[Type], gkind: GenericKind) -> (TypeMap, Vec<MonoType>) {
        let mut map = TypeMap::new();
        let mut elems = Vec::with_capacity(params.len());
        for (i, ty) in params.iter().cloned().enumerate() {
            let p = self.apply(&ty);
            let generic = key::Generic(i as u32);
            map.generics
                .push((Generic::new(generic, gkind), (ty, p.clone())));
            elems.push(p);
        }
        (map, elems)
    }

    pub fn applys<'t, F: FromIterator<MonoType>>(
        &mut self,
        tys: impl IntoIterator<Item = &'t Type>,
    ) -> F {
        tys.into_iter().map(|ty| self.apply(ty)).collect::<F>()
    }

    pub fn apply_typing(&mut self, origin: FuncOrigin, typing: &mir::ConcreteTyping) -> MonoTyping {
        MonoTyping {
            origin,
            params: self.applys(typing.params.iter()),
            returns: self.apply(&typing.returns),
        }
    }

    pub fn generic(&self, generic: Generic) -> &(Type, MonoType) {
        match self.tmap.generics.iter().find(|(g, _)| *g == generic) {
            None => panic!("unknown generic: {generic}"),
            Some((_, ty)) => ty,
        }
    }
}

impl TypeMap {
    pub fn new() -> Self {
        Self { generics: Vec::new(), self_: None }
    }
}

impl fmt::Debug for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MonoType::Int(bitsize) => write!(f, "i{bitsize}"),
            MonoType::UInt(bitsize) => write!(f, "u{bitsize}"),
            MonoType::SumDataCast { largest } => write!(f, "<sum {largest}>"),
            MonoType::Array(ty, times) => write!(f, "[{ty:?}; {times}]"),
            MonoType::Pointer(ty) => write!(f, "*{ty:?}"),
            MonoType::FnPointer(params, ret) => {
                write!(
                    f,
                    "fn({} -> {ret:?})",
                    params.iter().map(|t| format!("{t:?}")).format(", ")
                )
            }
            MonoType::Float => write!(f, "f64"),
            MonoType::Unreachable => write!(f, "!"),
            MonoType::Monomorphised(key) => write!(f, "{key}"),
        }
    }
}
