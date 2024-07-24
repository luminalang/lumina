use super::{mono, FuncLower, MonoType, TypeMap, Value};
use crate::key;
use lumina_typesystem::{Bitsize, Container, FuncKind, Generic, GenericKind, Prim, Type};
use tracing::error;

impl<'a> FuncLower<'a> {
    pub fn create_reflection(&mut self, weak: Type) -> Value {
        error!("{}", &weak);

        let reflect_weak = to_morphization!(self.lir, self.mir, &mut self.current.tmap)
            .apply_weak(&Type::defined(self.info.reflect_type, vec![]));

        match weak {
            Type::Prim(prim) => match prim {
                Prim::Int(signed, bitsize) => self.int(signed, bitsize.0),
                Prim::Float => self.reflect_variant(FLOAT, vec![]),
                Prim::Poison => self.reflect_variant(POISON, vec![]),
                Prim::Bool => self.reflect_variant(BOOL, vec![]),
                Prim::Never => self.reflect_variant(NEVER, vec![]),
            },
            Type::Container(cont) => match cont {
                Container::Func(FuncKind::Closure, _, _) => unreachable!(),
                Container::Func(FuncKind::FnPointer, ptypes, ret) => {
                    let elems = ptypes
                        .into_iter()
                        .map(|ty| self.create_reflection(ty))
                        .collect::<Vec<_>>();

                    let params_list = self.values_to_cons_list(elems, reflect_weak);

                    let ret = self.create_reflection(*ret);

                    self.reflect_variant(FN_POINTER, vec![params_list, ret])
                }
                Container::Tuple(elems) => {
                    let elems = elems
                        .into_iter()
                        .map(|ty| self.create_reflection(ty))
                        .collect::<Vec<_>>();

                    let elems_list = self.values_to_cons_list(elems, reflect_weak);

                    self.reflect_variant(TUPLE, vec![elems_list])
                }
                Container::Pointer(inner) => {
                    let inner = self.create_reflection(*inner);
                    self.reflect_variant(POINTER, vec![inner])
                }
            },
            Type::Self_ | Type::Generic(_) => {
                unreachable!("unsubstituted generic in weak monomorphised type")
            }
            Type::List(key, params) | Type::Defined(key, params) => {
                let name = self.mir.name_of_type(key);

                let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);

                let mut tmap = TypeMap::new();
                for (i, ty) in params.into_iter().enumerate() {
                    let generic = Generic::new(key::Generic(i as u32), GenericKind::Entity);
                    let mono = morph.apply(&ty);
                    tmap.generics.push((generic, (ty, mono)));
                }

                match key.value {
                    key::TypeKind::Record(record) => {
                        let key = key.module.m(record);
                        let fields = self.mir.field_types[key.module.m(record)]
                            .iter()
                            .map(|(field, ty)| {
                                let fname = self
                                    .string_to_value(self.mir.field_names[key][field].as_bytes());

                                let ty =
                                    to_morphization!(self.lir, self.mir, &mut tmap).apply_weak(ty);
                                let field_type = self.create_reflection(ty);

                                self.elems_to_tuple(vec![fname, field_type], None)
                            })
                            .collect::<Vec<_>>();

                        let fields = self.values_to_cons_list(fields, reflect_weak);
                        let name = self.string_to_value(name.as_bytes());

                        self.reflect_variant(STRUCT, vec![name, fields])
                    }
                    key::TypeKind::Sum(sum) => {
                        let key = key.module.m(sum);
                        let variants = self.mir.variant_types[key]
                            .iter()
                            .map(|(var, tys)| {
                                let vname = self
                                    .string_to_value(self.mir.variant_names[key][var].as_bytes());

                                let variant_params = tys
                                    .iter()
                                    .map(|ty| {
                                        let ty = to_morphization!(self.lir, self.mir, &mut tmap)
                                            .apply_weak(ty);
                                        self.create_reflection(ty)
                                    })
                                    .collect();

                                let variant_params =
                                    self.values_to_cons_list(variant_params, reflect_weak.clone());

                                self.elems_to_tuple(vec![vname, variant_params], None)
                            })
                            .collect();

                        let variants = self.values_to_cons_list(variants, reflect_weak);
                        let name = self.string_to_value(name.as_bytes());

                        self.reflect_variant(SUM, vec![name, variants])
                    }
                    key::TypeKind::Trait(_) => {
                        todo!("type reflection on dynamic trait objects: {OBJECT}")
                    }
                }
            }
        }
    }

    fn int(&mut self, signed: bool, bitsize: u8) -> Value {
        let params = [signed as u128, bitsize as u128]
            .map(|n| Value::UInt(n, Bitsize(8)))
            .to_vec();

        self.reflect_variant(INT, params)
    }

    fn reflect_variant(&mut self, tag: Value, params: Vec<Value>) -> Value {
        let reflect_mono = to_morphization!(self.lir, self.mir, &mut self.current.tmap)
            .sum(self.info.reflect_type, &[]);

        let largest = self.lir.types.types.size_of_defined(reflect_mono) - mono::TAG_SIZE.0 as u32;
        let dataty = MonoType::SumDataCast { largest };
        let parameters = self.ssa().construct(params, dataty).value();

        self.ssa()
            .construct(vec![tag, parameters], reflect_mono.into())
            .value()
    }
}

const INT: Value = Value::UInt(0, mono::TAG_SIZE);
const FLOAT: Value = Value::UInt(1, mono::TAG_SIZE);
const BOOL: Value = Value::UInt(2, mono::TAG_SIZE);
const NEVER: Value = Value::UInt(3, mono::TAG_SIZE);
const POISON: Value = Value::UInt(4, mono::TAG_SIZE);
const POINTER: Value = Value::UInt(5, mono::TAG_SIZE);
const FN_POINTER: Value = Value::UInt(6, mono::TAG_SIZE);
const STRUCT: Value = Value::UInt(7, mono::TAG_SIZE);
const SUM: Value = Value::UInt(8, mono::TAG_SIZE);
const TUPLE: Value = Value::UInt(9, mono::TAG_SIZE);
const OBJECT: Value = Value::UInt(10, mono::TAG_SIZE);

// (std:prelude)
//
// pub type Type
//   = Int bool u8
//   | Float
//   | Bool
//   | Never
//   | Poison
//   | Pointer   Type
//   | FnPointer [Type] Type
//   | Struct string [(string, Type)]
//   | Sum    string [(string, [Type])]
//   | Tuple  [Type]
//   | Object string
