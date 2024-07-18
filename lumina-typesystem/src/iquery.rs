use super::{
    map_con, Constraint, Container, Forall, FuncKind, Generic, GenericKind, Prim, Type, M,
};
use derive_new::new;
use lumina_key as key;
use std::collections::HashMap;
use tracing::info;

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

    pub fn for_each_relevant<T>(
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

// Getter for the forall of a kind
type GetForall<'a, 't, 's> = &'a dyn Fn(GenericKind) -> &'t Forall<'s, Type>;

// Getter for implementor and trait parameters of specific implementation
type GetImplData<'a, 't, 's> =
    &'a dyn Fn(M<key::Impl>) -> (&'t Forall<'s, Type>, &'t Type, &'t [Type]);

pub struct Compatibility<'a, 't, 's> {
    impls: &'a ImplIndex,
    mapping: Vec<Assignment>,

    lhs_forall: GetForall<'a, 't, 's>,
    rhs_forall: &'t Forall<'s, Type>,

    get_impl_data: GetImplData<'a, 't, 's>,
}

#[derive(new)]
pub struct Assignment {
    pub key: key::Generic,
    pub ty: Type,
}

impl<'a, 't, 's> Compatibility<'a, 't, 's> {
    pub fn new(
        impls: &'a ImplIndex,
        lhs_forall: GetForall<'a, 't, 's>,
        rhs_forall: &'t Forall<'s, Type>,
        get_impl_data: GetImplData<'a, 't, 's>,
    ) -> Self {
        Self {
            impls,
            lhs_forall,
            rhs_forall,
            mapping: vec![],
            get_impl_data,
        }
    }

    pub fn into_assignments(self) -> Vec<Assignment> {
        self.mapping
    }

    pub fn check_all(self) -> bool {
        self.mapping.iter().all(|assignment| {
            self.rhs_forall[assignment.key]
                .trait_constraints
                .iter()
                .all(|con| {
                    // Since all types have already been visited and mapped, we
                    // can instantiate the types in the constraint ahead of time so we don't need
                    // to keep around multiple `mapping`.
                    let con = map_con(con, |ty| apply_mapping(&self.mapping, ty));

                    Compatibility::constraint(
                        self.impls,
                        self.get_impl_data,
                        self.lhs_forall,
                        &assignment.ty,
                        &con,
                    )
                })
        })
    }

    /// Check whether `got` satisfies the constraint `con`.
    ///
    /// Types in both are assumed to be from the same type environment (represented by lhs_forall)
    pub fn constraint(
        impls: &'a ImplIndex,
        get_impl_data: GetImplData<'a, 't, 's>,
        lhs_forall: GetForall<'a, 't, 's>,
        got: &Type,
        con: &Constraint<Type>,
    ) -> bool {
        info!("checking if {got} implements {con:?}");

        // First check whether the constraint is directly satisfied by the given type being a
        // generic with the same constraint.
        match got {
            Type::Generic(generic) => {
                for gcon in &lhs_forall(generic.kind)[generic.key].trait_constraints {
                    if gcon.trait_ == con.trait_ {
                        info!("checking if {gcon} is a direct_eq of {con}");
                        debug_assert_eq!(gcon.params.len(), con.params.len());

                        if gcon.params.iter().eq(&con.params) {
                            return true;
                        }
                    }
                }
            }
            _ => {}
        }

        // If not then fall back to querying the implementations.
        impls
            .for_each_relevant::<()>(con.trait_, got.try_into().ok(), |ikey: _| {
                let (iforall, iimpltor, itrtp) = get_impl_data(ikey);
                let mut comp = Compatibility::new(impls, lhs_forall, iforall, get_impl_data);

                // Return None if the types aren't compatible
                comp.cmp(got, iimpltor).then(|| ())?;
                comp.cmps(&con.params, itrtp).then(|| ())?;

                // If they were compatible; then check any constraints that arose.
                comp.check_all().then(|| ())
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
            (_, Type::Generic(generic)) => self.map(got, *generic),
            _ => false,
        }
    }

    fn map(&mut self, got: &Type, exp: Generic) -> bool {
        match self.mapping.iter().find(|asgn| asgn.key == exp.key) {
            Some(asgn) => asgn.ty == *got,
            None => {
                self.mapping.push(Assignment::new(exp.key, got.clone()));
                true
            }
        }
    }

    pub fn cmps(&mut self, gots: &[Type], exps: &[Type]) -> bool {
        gots.iter().zip(exps).all(|(g, e)| self.cmp(g, e))
    }
}

fn apply_mapping(mapping: &[Assignment], ty: &Type) -> Type {
    match ty {
        Type::Generic(generic) => {
            assert_eq!(
                generic.kind,
                GenericKind::Parent,
                "apply_mapping should only be ran on the RHS"
            );

            // we *don't* want to call apply_mapping recursively because mapping[I].ty already
            // belongs to the LHS.
            mapping
                .iter()
                .find(|asgn| asgn.key == generic.key)
                .expect("RHS not assigned when checking child constraints")
                .ty
                .clone()
        }
        Type::Container(cont) => Type::Container(cont.map(|ty| apply_mapping(mapping, ty))),
        Type::Prim(prim) => Type::Prim(*prim),
        Type::Defined(key, params) => {
            let params = params.iter().map(|ty| apply_mapping(mapping, ty)).collect();
            Type::Defined(*key, params)
        }
        Type::List(key, params) => {
            let params = params.iter().map(|ty| apply_mapping(mapping, ty)).collect();
            Type::List(*key, params)
        }
        Type::Self_ => unreachable!("Self in constriant wasn't substituted"),
    }
}
