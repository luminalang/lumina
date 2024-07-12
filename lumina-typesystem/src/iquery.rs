use super::{Constraint, Container, Forall, FuncKind, Generic, GenericKind, Prim, Type, M};
use derive_new::new;
use lumina_key as key;
use std::collections::HashMap;
use std::fmt;

// TODO: Specialisation via explicit default notation

/// The way we check constraints and select implementations is by making type check against the
/// implementation's types. However; checking every single implementation to find a valid one would
/// of course be an absurdly slow operation.
///
/// The ImplIndex drastically reduces the amount of implementations that need to be checked by
/// hashing the root of the type in both the trait and implementor.
#[derive(new, Debug)]
pub struct ImplIndex {
    #[new(default)]
    traits: HashMap<M<key::Trait>, (IBlanked, IConcrete)>,
}

type IBlanked = Vec<M<key::Impl>>;
type IConcrete = HashMap<ConcreteType, Vec<M<key::Impl>>>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ConcreteType {
    Prim(Prim),
    Defined(M<key::TypeKind>),
    Func(FuncKind),
    Tuple(usize),
    Pointer,
}

impl TryFrom<&Type> for ConcreteType {
    type Error = ();

    fn try_from(value: &Type) -> Result<Self, Self::Error> {
        match value {
            Type::Container(c) => match c {
                Container::Func(kind, _, _) => Ok(ConcreteType::Func(*kind)),
                Container::Tuple(elems) => Ok(ConcreteType::Tuple(elems.len())),
                Container::Pointer(_) => Ok(ConcreteType::Pointer),
            },
            Type::List(ty, _) | Type::Defined(ty, _) => Ok(ConcreteType::Defined(*ty)),
            Type::Prim(prim) => Ok(ConcreteType::Prim(*prim)),
            Type::Generic(_) => Err(()),
            Type::Self_ => panic!("self type added to implementation"),
        }
    }
}

impl ImplIndex {
    pub fn insert(&mut self, trait_: M<key::Trait>, impltor: &Type, ikey: M<key::Impl>) -> () {
        let (blanked, concrete) = self
            .traits
            .entry(trait_)
            .or_insert_with(|| (Vec::new(), HashMap::new()));

        match ConcreteType::try_from(impltor) {
            Ok(c) => concrete.entry(c).or_insert_with(Vec::new).push(ikey),
            Err(()) => blanked.push(ikey),
        }
    }

    pub fn raw_find_impl(
        &self,
        trait_: M<key::Trait>,
        impltor: M<key::TypeKind>,
    ) -> &[M<key::Impl>] {
        self.traits
            .get(&trait_)
            .and_then(|(_, concrete)| {
                concrete
                    .get(&ConcreteType::Defined(impltor))
                    .map(|v| v.as_slice())
            })
            .unwrap_or(&[])
    }

    pub fn query<T>(
        &self,
        trait_: M<key::Trait>,
        impltor: Option<ConcreteType>,
        mut for_each: impl FnMut(M<key::Impl>) -> Option<T>,
    ) -> Option<T> {
        let (blanked, concrete) = self.traits.get(&trait_)?;

        match impltor {
            Some(c) => concrete
                .get(&c)
                .and_then(|these| these.iter().copied().find_map(&mut for_each)),
            None => None,
        }
        .or_else(|| blanked.iter().copied().find_map(for_each))
    }
}

pub struct Compatibility<'a, F> {
    impls: &'a ImplIndex,
    mapping: Vec<(key::Generic, Type)>,

    forall: F,

    recurse: &'a dyn Fn(Compatibility<'a, F>, &Type, M<key::Impl>, &[Type]) -> bool,
}

impl<'a, 'c, F> Compatibility<'a, F>
where
    F: FnMut(Generic) -> &'c [Constraint<Type>] + Clone,
{
    pub fn new(
        impls: &'a ImplIndex,
        forall: F,
        recurse: &'a dyn Fn(Compatibility<'a, F>, &Type, M<key::Impl>, &[Type]) -> bool,
    ) -> Self {
        Self { impls, mapping: vec![], forall, recurse }
    }

    pub fn check_constraints(
        impls: &'a ImplIndex,
        forall: F,
        recurse: &'a dyn Fn(Compatibility<'a, F>, &Type, M<key::Impl>, &[Type]) -> bool,
        got: &Type,
        cons: &[Constraint<Type>],
    ) -> bool {
        cons.iter()
            .all(|con| Self::constraint(impls, forall.clone(), recurse, got, con))
    }

    pub fn constraint(
        impls: &'a ImplIndex,
        forall: F,
        recurse: &'a dyn Fn(Compatibility<'a, F>, &Type, M<key::Impl>, &[Type]) -> bool,
        got: &Type,
        con: &Constraint<Type>,
    ) -> bool {
        match got {
            Type::Generic(generic) => {
                let mut this = Compatibility::new(impls, forall.clone(), recurse);
                if this.check_direct_constraint(*generic, con) {
                    return true;
                }
            }
            _ => {}
        }

        impls
            .query(con.trait_, got.try_into().ok(), |ikey| {
                let comp = Compatibility::new(impls, forall.clone(), recurse);
                (recurse)(comp, got, ikey, &con.params).then_some(())
            })
            .is_some()
    }

    pub fn cmp(&mut self, got: &Type, exp: &Type) -> bool {
        match (got, exp) {
            (Type::Prim(gprim), Type::Prim(eprim)) => gprim == eprim,
            (Type::Self_, _) => unreachable!(),
            (_, Type::Self_) => unreachable!(),
            (Type::List(key, params), _) => {
                let got = Type::Defined(*key, params.clone());
                self.cmp(&got, exp)
            }
            (_, Type::List(key, params)) => {
                let exp = Type::Defined(*key, params.clone());
                self.cmp(got, &exp)
            }
            (Type::Defined(gkind, gp), Type::Defined(ekind, ep)) if gkind == ekind => {
                self.cmps(gp, ep)
            }
            (Type::Container(gcon), Type::Container(econ)) => match (gcon, econ) {
                (Container::Func(kind, ptypes, ret), Container::Func(ekind, eptypes, eret)) => {
                    match (kind, ekind) {
                        (FuncKind::Closure, FuncKind::FnPointer) => {
                            self.cmps(ptypes, eptypes) && self.cmp(ret, eret)
                        }
                        _ if kind == ekind => self.cmps(ptypes, eptypes) && self.cmp(ret, eret),
                        _ => false,
                    }
                }
                (Container::Tuple(gp), Container::Tuple(ep)) => self.cmps(gp, ep),
                (Container::Pointer(gp), Container::Pointer(ep)) => self.cmp(gp, ep),
                _ => false,
            },
            (_, Type::Generic(generic)) => {
                assert_eq!(generic.kind, GenericKind::Parent);
                self.map(got, *generic)
            }
            _ => false,
        }
    }

    fn map(&mut self, got: &Type, exp: Generic) -> bool {
        let cons = (self.forall)(exp);
        Compatibility::check_constraints(self.impls, self.forall.clone(), self.recurse, got, cons);

        match self.mapping.iter().find(|(gen, _)| *gen == exp.key) {
            Some((_, ty)) => {
                got == ty
                // todo!("`this should be direct eq`");
                // self.cmp(got, &ty)
            }
            None => {
                self.mapping.push((exp.key, got.clone()));
                true
            }
        }
    }

    fn check_direct_constraint(&mut self, got: Generic, con: &Constraint<Type>) -> bool {
        todo!();
    }

    pub fn cmps(&mut self, gots: &[Type], exps: &[Type]) -> bool {
        gots.iter().zip(exps).all(|(g, e)| self.cmp(g, e))
    }
}
