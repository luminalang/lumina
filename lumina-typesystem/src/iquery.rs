use super::{
    Constraint, Container, Forall, Generic, GenericKind, GenericMapper, IntSize, Static,
    Transformer, Ty, Type, M,
};
use derive_new::new;
use lumina_key as key;
use std::collections::HashMap;
use tracing::{info, warn};

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
    Int(IntSize),
    Singleton(&'static str),
    Defined(M<key::TypeKind>),
    Closure,
    FnPointer,
    Tuple(usize),
    Pointer,
}

impl TryFrom<&Type> for ConcreteType {
    type Error = ();

    fn try_from(value: &Type) -> Result<Self, Self::Error> {
        match value {
            Ty::Int(size) => Ok(ConcreteType::Int(*size)),
            Ty::Container(Container::FnPointer, _) => Ok(ConcreteType::FnPointer),
            Ty::Container(Container::Closure, _) => Ok(ConcreteType::Closure),
            Ty::Container(Container::Tuple, params) => Ok(ConcreteType::Tuple(params.len())),
            Ty::Container(Container::Pointer, _) => Ok(ConcreteType::Pointer),
            Ty::Simple(name) => Ok(ConcreteType::Singleton(*name)),
            _ => Err(()),
        }
    }
}

impl ImplIndex {
    pub fn insert(&mut self, trait_: M<key::Trait>, impltor: &Type, ikey: M<key::Impl>) -> () {
        let (blanked, concrete) = self
            .traits
            .entry(trait_)
            .or_insert_with(|| (Vec::new(), HashMap::new()));

        debug_assert!(!matches!(impltor, Type::Simple("self")));

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
pub(crate) type GetForall<'a, 't, 's> = &'a dyn Fn(GenericKind) -> &'t Forall<'s, Static>;

// Getter for implementor and trait parameters of specific implementation
pub(crate) type GetImplData<'a, 't, 's> =
    &'a dyn Fn(M<key::Impl>) -> (M<key::Trait>, &'t Forall<'s, Static>, &'t Type, &'t [Type]);

pub struct Compatibility<'a, 't, 's> {
    impls: &'a ImplIndex,
    mapping: GenericMapper<Static>,

    lhs_forall: GetForall<'a, 't, 's>,
    rhs_forall: &'t Forall<'s, Static>,

    get_impl_data: GetImplData<'a, 't, 's>,
}

impl<'a, 't, 's> Compatibility<'a, 't, 's> {
    pub fn new(
        impls: &'a ImplIndex,
        lhs_forall: GetForall<'a, 't, 's>,
        rhs_forall: &'t Forall<'s, Static>,
        get_impl_data: GetImplData<'a, 't, 's>,
    ) -> Self {
        Self {
            impls,
            lhs_forall,
            rhs_forall,
            mapping: GenericMapper::new(vec![], None),
            get_impl_data,
        }
    }

    pub fn into_assignments(self) -> GenericMapper<Static> {
        self.mapping
    }

    pub fn check_all(self) -> bool {
        self.mapping
            .assignments_by_kind(GenericKind::Parent)
            .all(|(key, ty)| {
                self.rhs_forall[key].trait_constraints.iter().all(|con| {
                    // Since all types have already been visited and mapped, we
                    // can instantiate the types in the constraint ahead of time so we don't need
                    // to keep around multiple `mapping`.
                    // let con = map_con(con, |ty| apply_mapping(&self.mapping, ty));
                    let con = (&self.mapping).transform_constraint(con);

                    Compatibility::constraint(
                        self.impls,
                        None,
                        self.get_impl_data,
                        self.lhs_forall,
                        ty,
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
        in_trait: Option<(M<key::Trait>, &'t Forall<'s, Static>)>,
        get_impl_data: GetImplData<'a, 't, 's>,
        lhs_forall: GetForall<'a, 't, 's>,
        got: &Type,
        con: &Constraint<Static>,
    ) -> bool {
        info!("checking if {got} implements {con}");

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
            // Edge-case for `Self` in trait method defaults
            Type::Simple("self") => {
                let (trait_, tforall) = in_trait.unwrap();

                if con.trait_ == trait_
                    && con
                        .params
                        .iter()
                        .zip(tforall.generics.keys())
                        .all(|(t, k)| match t {
                            Type::Generic(Generic { kind: GenericKind::Parent, key }) => k == *key,
                            _ => false,
                        })
                {
                    return true;
                }
            }
            _ => {}
        }

        // If not then fall back to querying the implementations.
        impls
            .for_each_relevant::<()>(con.trait_, got.try_into().ok(), |ikey: _| {
                let (_, iforall, iimpltor, itrtp) = get_impl_data(ikey);
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
            (Ty::Int(gsize), Ty::Int(esize)) => gsize == esize,
            (Ty::Simple("self"), _) => {
                warn!("indirect lookup for `self` in trait defaults currently isn't implemented");
                false
            }
            (Ty::Simple(g), Ty::Simple(e)) => g == e,
            (Ty::Container(g, gparams), Ty::Container(e, eparams)) if g == e => {
                self.cmps(gparams, eparams)
            }
            (
                Ty::Container(Container::Defined(gkey, _), gparams),
                Ty::Container(Container::Defined(ekey, _), eparams),
            ) if gkey == ekey => self.cmps(gparams, eparams),
            (Ty::Special(_), _) | (_, Ty::Special(_)) => unreachable!(),
            (_, Type::Generic(generic)) => self.map(got, *generic),
            _ => false,
        }
    }

    fn map(&mut self, got: &Type, exp: Generic) -> bool {
        match self.mapping.find(exp).cloned() {
            Some(ty) => got == &ty,
            None => {
                self.mapping.push(exp, got.clone());
                true
            }
        }
    }

    pub fn cmps(&mut self, gots: &[Type], exps: &[Type]) -> bool {
        gots.iter().zip(exps).all(|(g, e)| self.cmp(g, e))
    }
}
