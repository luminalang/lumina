use super::{
    Annotation, CallableKind, Environment, FailedReceiverLookup, Forall, GenericTag, IntSize,
    KnownType, Prim, VariableInfo, VariableSource,
};
use crate::key;
use crate::key::EntityList;
use lumina_key::Map;
use std::collections::{BTreeSet, HashMap};
use std::fmt;
use tracing::{error, info, info_span, trace};

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct Application {
    pub(crate) func: key::Var,
    pub(crate) ret: key::Var,
    pub(crate) parameters: EntityList<key::Var>,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) struct Assignment {
    pub(crate) lhs: key::Var,
    pub(crate) rhs: key::Var,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) struct Return {
    pub(crate) expr: key::Var,
    pub(crate) expected: key::Var,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct SameasUnification {
    pub(crate) main: SameasMain,
    pub(crate) members: EntityList<key::Var>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum SameasMain {
    List { elem: key::Var, list: key::Var },
    JoinExpression(key::Var),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct HasField {
    pub(crate) name: String,
    pub(crate) field_type: key::Var,
}

// We split out checks into something more granular to use as a queue during inference
#[derive(PartialEq, Copy, Eq, Clone)]
enum Check {
    Assignment(Assignment),
    ParamApplication(key::Application),
    Return(Return),
    SameAs(key::SameasUnification),
    CheckFields(key::Var),
    ResolveRecord(key::Var),
    TypeSensitiveNameres(key::Application),
    DefaultList(key::Var),
    DefaultNumber(key::Var),
    PoisonTypeSensitive(key::Application),
    DefaultUnknown(key::Var),
}

#[derive(Copy, PartialEq, Eq, Clone)]
enum LCheck {
    Check(Check),
    Parent(key::Var),
}

impl Ord for Check {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        fn discr(c: &Check) -> u32 {
            match c {
                Check::Assignment(_) => 0,
                Check::ParamApplication(_) => 1,
                Check::Return { .. } => 2,
                Check::SameAs(_) => 3,
                Check::CheckFields(_) => 4,
                Check::ResolveRecord(_) => 5,
                Check::TypeSensitiveNameres(_) => 6,
                Check::DefaultList(_) => 7,
                Check::DefaultNumber(_) => 8,
                Check::PoisonTypeSensitive(_) => 9,
                Check::DefaultUnknown(_) => 10,
            }
        }

        match (self, other) {
            (Check::Assignment(lassgn), Check::Assignment(rassgn)) => {
                match lassgn.rhs.cmp(&rassgn.rhs) {
                    std::cmp::Ordering::Less => std::cmp::Ordering::Less,
                    std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
                    std::cmp::Ordering::Equal => lassgn.lhs.cmp(&rassgn.lhs),
                }
            }
            (Check::ParamApplication(lappl), Check::ParamApplication(rappl)) => lappl.cmp(&rappl),
            (Check::SameAs(lsameas), Check::SameAs(rsameas)) => lsameas.cmp(&rsameas),
            (Check::CheckFields(lrec), Check::CheckFields(rrec)) => lrec.cmp(&rrec),
            (Check::ResolveRecord(lrec), Check::ResolveRecord(rrec)) => lrec.cmp(&rrec),
            (Check::TypeSensitiveNameres(lfunc), Check::TypeSensitiveNameres(rfunc)) => {
                lfunc.cmp(&rfunc)
            }
            (Check::DefaultList(lvar), Check::DefaultList(rvar)) => lvar.cmp(&rvar),
            (Check::DefaultNumber(lvar), Check::DefaultNumber(rvar)) => lvar.cmp(&rvar),
            (Check::PoisonTypeSensitive(lvar), Check::PoisonTypeSensitive(rvar)) => lvar.cmp(&rvar),
            (Check::DefaultUnknown(lvar), Check::DefaultUnknown(rvar)) => lvar.cmp(&rvar),

            (lhs, rhs) => discr(lhs).cmp(&discr(rhs)),
        }
    }
}

impl PartialOrd for Check {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub struct InferenceUnifier<'a, Ident, R: Resolver<Ident>> {
    env: &'a mut Environment<Ident>,

    queue: CheckQueue,

    record_field_instantiations: HashMap<key::Var, InstantiatedFields>,
    resolver: R,
    default_int_size: IntSize,

    listables: Vec<Ident>,
    default_listable: Option<Ident>,
}

// Provides an ordered queue of checks to be performed in a most-useful order.
#[derive(Debug)]
struct CheckQueue {
    checks: BTreeSet<Check>,
    dependence_graph: Map<key::Var, Vec<LCheck>>,
}

impl CheckQueue {
    fn new(vars: impl Iterator<Item = key::Var>) -> Self {
        Self {
            checks: BTreeSet::new(),
            dependence_graph: vars.map(|_| vec![]).collect(),
        }
    }

    fn extend(&mut self, var: key::Var) {
        assert_eq!(self.dependence_graph.push(vec![]), var);
    }

    fn new_check(&mut self, to: key::Var, check: Check) {
        if self.dependence_graph[to]
            .iter()
            .any(|c| *c == LCheck::Check(check))
        {
            return;
        }

        self.checks.insert(check);
        self.dependence_graph[to].push(LCheck::Check(check));
    }

    fn new_link(&mut self, from: key::Var, to: key::Var) {
        if self.dependence_graph[from]
            .iter()
            .any(|c| *c == LCheck::Parent(to))
        {
            return;
        }

        assert_ne!(from, to, "type may not be parent of itself");
        self.dependence_graph[from].push(LCheck::Parent(to));
    }

    fn link_children_upwards<Ident>(&mut self, env: &Environment<Ident>, var: key::Var) {
        for field in &env.variables[var].has_fields {
            self.new_link(field.field_type, var);
        }
        for field in &env.variables[var].has_fields {
            self.link_children_upwards(env, field.field_type);
        }

        match &env.variables[var].info {
            VariableInfo::Tuple(children) | VariableInfo::Defined(_, children) => {
                for child in children.as_slice(&env.var_pool) {
                    self.new_link(*child, var);
                }

                for child in children.as_slice(&env.var_pool) {
                    self.link_children_upwards(env, *child);
                }
            }
            VariableInfo::InferTo(child)
            | VariableInfo::Applied { func: child, .. } // TODO: Parameters are added elsewhere right?
            | VariableInfo::Pointer(child)
            | VariableInfo::List(child) => {
                self.new_link(*child, var);
                self.link_children_upwards(env, *child);
            }
            VariableInfo::Array { of, len } => {
                self.new_link(*of, var);
                self.link_children_upwards(env, *of);

                self.new_link(*len, var);
                self.link_children_upwards(env, *len);
            }
            VariableInfo::Function { params, ret, .. } => {
                if let Some(params) = params {
                    for child in params.as_slice(&env.var_pool) {
                        self.new_link(*child, var);
                    }
                }
                self.new_link(*ret, var);

                if let Some(params) = params {
                    for child in params.as_slice(&env.var_pool) {
                        self.link_children_upwards(env, *child);
                    }
                }
                self.link_children_upwards(env, *ret);
            }
            VariableInfo::Error
            | VariableInfo::Unknown
            | VariableInfo::Numeric
            | VariableInfo::Const(_)
            | VariableInfo::Generic(_)
            | VariableInfo::Prim(_)
            | VariableInfo::TypeResolvedFunction(_) => {}
        }
    }

    fn does_var_contains_parent(&self, var: key::Var, parent: key::Var) -> bool {
        if var == parent {
            return true;
        }

        self.dependence_graph[var]
            .iter()
            .any(|lcheck| match lcheck {
                LCheck::Check(_) => false,
                LCheck::Parent(p) => self.does_var_contains_parent(*p, parent),
            })
    }

    fn touch(&mut self, var: key::Var) {
        for i in 0.. {
            let Some(lcheck) = self.dependence_graph[var].get(i).copied() else {
                break;
            };

            match lcheck {
                LCheck::Check(check) => {
                    info!("re-queuing: {lcheck:?}");
                    self.checks.insert(check);
                }
                LCheck::Parent(new_var) => {
                    trace!("traversing {var} -> {new_var}");
                    self.touch(new_var)
                }
            }
        }
    }

    fn next(&mut self) -> Option<Check> {
        self.checks.pop_first()
    }

    fn include_inherent_checks<Ident>(&mut self, env: &Environment<Ident>, var: key::Var) {
        if !env.variables[var].has_fields.is_empty() {
            self.new_check(var, Check::CheckFields(var));

            if let VariableInfo::Unknown = env.variables[var].info {
                self.new_check(var, Check::ResolveRecord(var));
            }
        }

        match &env.variables[var].info {
            VariableInfo::List(_) => {
                self.new_check(var, Check::DefaultList(var));
            }
            VariableInfo::Numeric => {
                self.new_check(var, Check::DefaultNumber(var));
            }
            VariableInfo::Unknown => {
                self.new_check(var, Check::DefaultUnknown(var));
            }
            _ => {}
        }
    }
}

struct InstantiatedFields {
    fields: EntityList<key::Var>,
}

impl<'a, Ident: Clone + fmt::Debug + fmt::Display + PartialEq, R: Resolver<Ident>>
    InferenceUnifier<'a, Ident, R>
{
    pub fn new(
        env: &'a mut Environment<Ident>,
        resolver: R,
        default_int_size: IntSize,
        listables: Vec<Ident>,
        default_listable: Option<Ident>,
    ) -> Self {
        let mut queue = CheckQueue::new(env.variables.keys());

        // env.debug_print();

        // Traverse the type variable tree and link relations bottom-up
        for var in env.variables.keys() {
            queue.link_children_upwards(env, var);
        }

        for (appl, appl_data) in env.applications.iter() {
            let check = Check::ParamApplication(appl);

            queue.new_check(appl_data.func, check);

            for param in appl_data.parameters.as_slice(&env.var_pool) {
                queue.new_check(*param, check);
            }
            queue.new_check(appl_data.ret, check);

            match &env.variables[appl_data.func].info {
                VariableInfo::TypeResolvedFunction(_) => {
                    let check = Check::TypeSensitiveNameres(appl);

                    queue.new_check(appl_data.func, check);
                    queue.new_check(appl_data.func, check);

                    for param in appl_data.parameters.as_slice(&env.var_pool) {
                        queue.new_check(*param, check);
                    }
                    queue.new_check(appl_data.ret, check);
                }
                _ => {}
            }
        }

        for i in 0..env.assignments.len() {
            let assign = env.assignments[i].clone();

            queue.new_check(assign.lhs, Check::Assignment(assign.clone()));
            queue.new_check(assign.rhs, Check::Assignment(assign.clone()));
        }

        for i in 0..env.returns_.len() {
            let ret = env.returns_[i].clone();

            queue.new_check(ret.expected, Check::Return(ret));
            queue.new_check(ret.expr, Check::Return(ret));
        }

        for (sameas, sameas_data) in env.same_as_unifications.iter() {
            match sameas_data.main {
                SameasMain::List { list: var, .. } | SameasMain::JoinExpression(var) => {
                    queue.new_check(var, Check::SameAs(sameas));
                }
            };

            // TODO: Unsure whether these are correct
            for &member in sameas_data.members.as_slice(&env.var_pool) {
                queue.new_check(member, Check::SameAs(sameas));
                // TODO: Do we also link? probably not right?
            }
        }

        for var in env.variables.keys() {
            queue.include_inherent_checks(env, var);
        }

        // dbg!(&queue);
        // env.debug_print();

        Self {
            queue,

            resolver,
            record_field_instantiations: HashMap::new(),

            default_int_size,

            listables,
            default_listable,

            env,
        }
    }

    pub fn infer(&mut self) {
        let mut not_done = true;

        while not_done {
            not_done = self.next();
        }

        // dbg!(&self.queue);
        // self.env.debug_print();
    }

    pub fn next(&mut self) -> bool {
        let Some(check) = self.queue.next() else {
            return false;
        };

        let span = info_span!("pass", pass = format!("{check:?}"));
        let _handle = span.enter();

        match check {
            Check::Assignment(assignment) => self.assignment(assignment),
            Check::ParamApplication(key) => self.param_application(key),
            Check::Return(key) => self.return_(key),
            Check::SameAs(key) => self.sameas(key),
            Check::CheckFields(var) => self.check_fields(var),
            Check::ResolveRecord(var) => self.resolve_record(var),
            Check::TypeSensitiveNameres(var) => self.type_sensitive_nameres(var),
            Check::DefaultList(var) => self.default_list(var),
            Check::DefaultNumber(var) => self.default_number(var),
            Check::PoisonTypeSensitive(key) => {
                let dominant = self.env.applications[key]
                    .parameters
                    .as_slice(&self.env.var_pool)
                    .last()
                    .unwrap();

                let reason = FailedReceiverLookup::Unresolved(*dominant);

                self.poison_type_sensitive(key, reason)
            }
            Check::DefaultUnknown(var) => self.default_unknown(var),
        }

        true
    }

    fn poison_type_sensitive(&mut self, key: key::Application, reason: FailedReceiverLookup) {
        let appl_data = &self.env.applications[key];

        if let VariableInfo::TypeResolvedFunction(_) = &self.env.variables[appl_data.func].info {
            self.env
                .failed_receiver_lookups
                .insert(appl_data.func, reason);

            info!("{} -> error", appl_data.ret);
            self.env.variables[appl_data.ret].info = VariableInfo::Error;
        } else if self
            .env
            .failed_receiver_lookups
            .contains_key(&appl_data.func)
        {
            info!("{} -> error", appl_data.ret);
            self.env.variables[appl_data.ret].info = VariableInfo::Error;
        }
    }

    fn default_number(&mut self, var: key::Var) {
        if let VariableInfo::Numeric = &self.env.variables[var].info {
            let prim = Prim::Int(self.default_int_size);
            info!("{var} -> default {prim}");
            self.env.variables[var].info = VariableInfo::Prim(prim);
            self.queue.touch(var);
        }
    }

    fn default_unknown(&mut self, var: key::Var) {
        let var_data = &mut self.env.variables[var];

        if let VariableInfo::Unknown = &var_data.info {
            let default_ = match var_data.source {
                VariableSource::Signature => {
                    let generic = self.resolver.implicitly_declare_generic(GenericTag::Func);
                    info!("{var} -> default {generic}");
                    VariableInfo::Generic(generic)
                }
                VariableSource::Expression => {
                    info!("{var} -> default {{unit}}");
                    VariableInfo::Tuple(key::EntityList::new())
                }
            };

            self.env.variables[var].info = default_;
            self.queue.touch(var);
        }
    }

    fn default_list(&mut self, var: key::Var) {
        if let VariableInfo::List(inner) = &self.env.variables[var].info
            && let Some(ty) = self.default_listable.clone()
        {
            info!("{var} -> default {ty}");
            let params = EntityList::from_slice(&[*inner], &mut self.env.var_pool);
            self.env.variables[var].info = VariableInfo::Defined(ty, params);
            self.queue.touch(var);
        }
    }

    fn type_sensitive_nameres(&mut self, appl: key::Application) {
        let appl_data = self.env.applications[appl].clone();

        if let VariableInfo::TypeResolvedFunction(name) =
            self.env.variables[appl_data.func].info.clone()
        {
            let dominant = *appl_data
                .parameters
                .as_slice(&self.env.var_pool)
                .last()
                .unwrap();

            let dominant = self.env.follow(dominant);

            // if let VariableInfo::List(elem) = &self.env.variables[dominant].info {
            //     todo!();
            // }

            let Ok(root) = KnownTypeRoot::try_from(&self.env.variables[dominant].info) else {
                trace!(
                    "{dominant}: {:?}, is not a valid type root for type nameres",
                    &self.env.variables[dominant].info
                );

                return;
            };

            let previous_len = self.env.variables.len();

            match self.resolver.receiver_lookup(self.env, root.clone(), &name) {
                ReceiverLookupResult::CircularInference => {
                    self.poison_type_sensitive(appl, FailedReceiverLookup::CircularInference);
                }
                ReceiverLookupResult::NotFound => {}
                ReceiverLookupResult::Func { params, ret } => {
                    self.hack_include_new_vars(previous_len, |queue, _, var| {
                        queue.new_check(var, Check::ParamApplication(appl))
                    });

                    // Unify the previous unknown return type with the now known return type
                    let check = Check::Assignment(Assignment { lhs: appl_data.ret, rhs: ret });
                    self.queue.new_check(appl_data.func, check);
                    self.queue.new_check(appl_data.ret, check);

                    let params = Some(params);
                    let func =
                        VariableInfo::Function { kind: CallableKind::FnPointer, params, ret };
                    info!("{} -> typesensitive {func:?}", appl_data.func);
                    self.env.variables[appl_data.func].info = func;

                    self.queue.link_children_upwards(&self.env, appl_data.func);

                    self.queue.touch(appl_data.func);
                }
            }
        }
    }

    fn resolve_record(&mut self, var: key::Var) {
        let var_data = &self.env.variables[var];

        let VariableInfo::Unknown = var_data.info else {
            return;
        };

        if var_data.has_fields.is_empty() {
            return;
        }

        if let Some(name) = self
            .resolver
            .guess_by_fields(var, var_data.has_fields.iter().map(|f| f.name.as_str()))
        {
            info!("inferring {var} to be {name} because of its fields");
            let mut forall = Annotation::new();

            self.resolver.for_each_type_parameter(&name, |_| {
                let tvar = self.env.unknown();
                self.queue.extend(tvar);
                self.queue.new_check(tvar, Check::DefaultUnknown(tvar));
                forall.push(GenericTag::Type, tvar, &mut self.env.var_pool);
            });

            let defined = VariableInfo::Defined(name, forall.item);
            info!("{var} -> record {defined:?}");
            self.env.variables[var].info = defined;
            self.queue.touch(var);
        } else {
            info!("{var} -> error");
            self.env.variables[var].info = VariableInfo::Error;
        }
    }

    fn check_fields(&mut self, var: key::Var) {
        let has_fields = self.env.has_fields(var);

        for i in 0..has_fields.len() {
            let has_field = self.env.has_fields(var)[i].clone();

            let Some(name) = self.env.get_defined_ident(var) else {
                info!(
                    "{var}: {:?} is not a record, skipping fields check",
                    &self.env.variables[var]
                );
                // Not a record
                break;
            };

            let Some(field) = self.resolver.get_field(name, &has_field.name) else {
                // Does not have this field
                continue;
            };

            let previous_len = self.env.variables.len();
            let expected = self.get_or_instantiate_field(var, field);

            self.hack_include_new_vars(previous_len, |_, _, _| {});

            self.unify(expected, has_field.field_type);
            // TODO: ^ Change this to assign check and add dependence
        }
    }

    // When new type variables are spawned after the dependence graph is constructed, we need to add
    // those new vars in the dependence graph in the correct order. We do that by comparing the old
    // and new length.
    fn hack_include_new_vars<F>(&mut self, previous_len: usize, mut additional: F)
    where
        F: FnMut(&mut CheckQueue, &Environment<Ident>, key::Var),
    {
        let Some(variables) = self.env.variables.as_values_slice().get(previous_len..) else {
            return;
        };

        let mut var = key::Var::from_u32(previous_len as u32);
        // Zero-initialize the dependence graph
        for _ in variables {
            self.queue.extend(var);
            var.0 += 1;
        }

        // let mut var = key::Var::from_u32(previous_len as u32);
        // for _ in variables {
        //     self.queue.link_children_upwards(self.env, var);
        //     var.0 += 1;
        // }

        let mut var = key::Var::from_u32(previous_len as u32);
        for _ in variables {
            additional(&mut self.queue, self.env, var);
            self.queue.include_inherent_checks(self.env, var);

            var.0 += 1;
        }
    }

    fn sameas(&mut self, key: key::SameasUnification) {
        let expected = self.env.expected_of_sameas(key);
        let members = self.env.get_members(key);
        for i in 0..members.len(&self.env.var_pool) {
            let given = members.get(i, &self.env.var_pool).unwrap();
            self.unify(expected, given);
        }
    }

    fn assignment(&mut self, check: Assignment) {
        self.unify(check.lhs, check.rhs);
    }

    fn param_application_for(&mut self, appl: Application) {
        match &self.env.variables[appl.func].info {
            &VariableInfo::InferTo(new) => {
                self.param_application_for(Application { func: new, ..appl })
            }
            VariableInfo::Function { params: None, ret, .. } => {
                todo!();
            }
            VariableInfo::Function { params: Some(params), .. } => {
                let params = params.clone();

                if params.len(&self.env.var_pool) != params.len(&self.env.var_pool) {
                    info!("parameter length differs, skipping {:?}", appl.func);
                    // TODO: Should we poison it here?
                    //
                    // or perhaps just remove this check since if it isn't valid now it wont ever be?
                    return;
                }

                for i in 0..params.len(&self.env.var_pool) {
                    let expected = params.get(i, &self.env.var_pool).unwrap();
                    let given = appl.parameters.get(i, &self.env.var_pool).unwrap();
                    self.unify(expected, given);
                }
            }
            VariableInfo::Error => todo!(),
            _ => {
                if appl.parameters.is_empty() {
                    trace!("assuming {appl:?}({}) is yield and not function", appl.func);

                    // The tvar was inferred to not be a function but a yielding value.
                    //
                    // So; unify the return tvar of the application with the yielding value instead.
                    self.unify(appl.func, appl.ret);
                }
            }
        }
    }

    fn param_application(&mut self, appl_key: key::Application) {
        let appl = self.env.applications[appl_key].clone();
        self.param_application_for(appl)
    }

    fn return_(&mut self, ret: Return) {
        self.unify(ret.expected, ret.expr);
    }

    fn unify(&mut self, expected: key::Var, given: key::Var) {
        trace!("unifying {expected} <> {given}");

        if expected == given {
            return;
        }

        let [exp_data, given_data] = self
            .env
            .variables
            .get_disjoint_mut([expected, given])
            .unwrap();

        trace!("{:?} <> {:?}", exp_data.info, given_data.info);

        match [&mut exp_data.info, &mut given_data.info] {
            [VariableInfo::Error, _] | [_, VariableInfo::Error] => {}
            [VariableInfo::InferTo(expected), _] => {
                let expected = *expected;
                self.unify(expected, given)
            }
            [_, VariableInfo::InferTo(given)] => {
                let given = *given;
                self.unify(expected, given)
            }
            [VariableInfo::Applied { appl, func }, _] => {
                let (appl, func) = (*appl, *func);

                if let Some(expected) = self.applied_unify_apply_or_yield(appl, func) {
                    self.unify(expected, given);
                }
            }
            [_, VariableInfo::Applied { appl, func }] => {
                let (appl, func) = (*appl, *func);

                if let Some(given) = self.applied_unify_apply_or_yield(appl, func) {
                    self.unify(expected, given);
                }
            }
            [VariableInfo::TypeResolvedFunction(_), _]
            | [_, VariableInfo::TypeResolvedFunction(_)] => {}
            [VariableInfo::Prim(exp_prim), VariableInfo::Prim(given_prim)]
                if exp_prim == given_prim => {}
            [
                VariableInfo::Generic(exp_prim),
                VariableInfo::Generic(got_prim),
            ] if exp_prim == got_prim => {}
            [
                VariableInfo::Defined(exp_ident, exp_params),
                VariableInfo::Defined(given_ident, given_params),
            ] if given_ident == exp_ident => {
                let exp_params = *exp_params;
                let given_params = *given_params;

                self.unify_params([exp_params, given_params]);
            }

            // Coerce fnptr into closure
            [
                VariableInfo::Function { kind: CallableKind::Closure, .. },
                VariableInfo::Function { kind: given_kind @ CallableKind::FnPointer, .. },
            ] => {
                *given_kind = CallableKind::Closure;
                return self.unify(expected, given);
            }
            [
                VariableInfo::Function { kind: exp_kind @ CallableKind::FnPointer, .. },
                VariableInfo::Function { kind: CallableKind::Closure, .. },
            ] => {
                *exp_kind = CallableKind::Closure;
                return self.unify(expected, given);
            }

            // Type check the signatures of two fnptr/closure types
            [VariableInfo::Function { .. }, VariableInfo::Function { .. }] => {
                self.unify_functions(expected, given)
            }
            [VariableInfo::Numeric, VariableInfo::Numeric] => {
                trace!("equal numerics ignored of {expected} ∈ {given}");
            }
            [
                VariableInfo::Numeric,
                VariableInfo::Prim(super::Prim::Int(_)),
            ] => {
                self.infer_directly(expected, given);
            }
            [
                VariableInfo::Prim(super::Prim::Int(_)),
                VariableInfo::Numeric,
            ] => {
                self.infer_directly(given, expected);
            }
            [
                VariableInfo::Tuple(exp_elems),
                VariableInfo::Tuple(given_elems),
            ] => {
                let exp_elems = *exp_elems;
                let given_elems = *given_elems;
                self.unify_params([exp_elems, given_elems])
            }
            [
                VariableInfo::List(exp_inner),
                VariableInfo::List(given_inner),
            ] => {
                let [exp_inner, given_inner] = [*exp_inner, *given_inner];
                self.unify(exp_inner, given_inner)
            }
            [
                VariableInfo::List(exp_inner),
                VariableInfo::Defined(ident, given_params),
            ] if given_params.len(&self.env.var_pool) == 1 && self.listables.contains(ident) => {
                // self.env.variables[expected].info = VariableInfo::InferTo();
                // todo!("hm. InferTo assumes no parameters. Not sure what the best solution is");
                // hm, the old code seems reasonable...

                let given_params = *given_params;
                let params = EntityList::from_slice(&[*exp_inner], &mut self.env.var_pool);
                let defined = VariableInfo::Defined(ident.clone(), params);
                info!("{expected} -> {defined:?}");
                self.env.variables[expected].info = defined;
                self.unify_params([params, given_params]);
                self.queue.touch(expected);
            }
            [
                VariableInfo::Defined(ident, exp_params),
                VariableInfo::List(got_inner),
            ] if exp_params.len(&self.env.var_pool) == 1 && self.listables.contains(ident) => {
                let exp_params = *exp_params;
                let params = EntityList::from_slice(&[*got_inner], &mut self.env.var_pool);
                let defined = VariableInfo::Defined(ident.clone(), params);
                info!("{given} -> {defined:?}");
                self.env.variables[given].info = defined;
                self.unify_params([exp_params, params]);
                self.queue.touch(given);
            }
            [
                VariableInfo::Array { of: exp_of, len: exp_len },
                VariableInfo::Array { of: given_of, len: given_len },
            ] => {
                let [exp_of, given_of] = [*exp_of, *given_of];
                let [exp_len, given_len] = [*exp_len, *given_len];
                self.unify(exp_len, given_len);
                self.unify(exp_of, given_of)
            }
            [
                VariableInfo::Pointer(exp_inner),
                VariableInfo::Pointer(given_inner),
            ] => {
                let [exp_inner, given_inner] = [*exp_inner, *given_inner];
                self.unify(exp_inner, given_inner)
            }

            [VariableInfo::Unknown, VariableInfo::Unknown] => {
                trace!("equal ignored of {expected} ∈ {given}");
            }

            [VariableInfo::Unknown, _] => self.infer_directly(expected, given),
            [_, VariableInfo::Unknown] => self.infer_directly(given, expected),

            [expected, given] => {
                error!("during inference, {:?} != {:?}", expected, given);
            }
        }
    }

    fn unify_functions(
        &mut self,
        expected: key::Var,
        given: key::Var,
        // [expfunc, givenfunc]: [(CallableKind, Option<EntityList<key::Var>>, key::Var); 2],
    ) {
        let VariableInfo::Function { kind: exp_kind, params: exp_params, ret: exp_ret } =
            self.env.variables[expected].info.clone()
        else {
            unreachable!();
        };

        let VariableInfo::Function { kind, params, ret } = self.env.variables[given].info.clone()
        else {
            unreachable!();
        };

        match (exp_params, params) {
            (Some(exp_params), Some(params)) if exp_kind.can_apply(&kind) => {
                if exp_params.len(&self.env.var_pool) != params.len(&self.env.var_pool) {
                    trace!("ignoring {expected} == {given} due to parameter count mismatch");
                    // Mismatched parameter count
                    return;
                }

                self.unify_params([exp_params.clone(), params.clone()]);
                self.unify(exp_ret, ret);
            }

            (None, Some(params)) => {
                let count = params.len(&self.env.var_pool);
                self.infer_parameter_count_of_unknown_function(expected, count);
                self.queue.touch(expected);
                self.unify_functions(expected, given);
            }
            (Some(exp_params), None) => {
                let count = exp_params.len(&self.env.var_pool);
                self.infer_parameter_count_of_unknown_function(given, count);
                self.queue.touch(given);
                self.unify_functions(expected, given);
            }

            // Not enough data, ignore for now
            (_, _) => return,
        }
    }

    fn unify_params(&mut self, [expected, given]: [EntityList<key::Var>; 2]) {
        for i in 0.. {
            let Some(expected) = expected.get(i, &self.env.var_pool) else {
                break;
            };

            let Some(given) = given.get(i, &self.env.var_pool) else {
                break;
            };

            self.unify(expected, given)
        }
    }

    fn infer_parameter_count_of_unknown_function(&mut self, func: key::Var, count: usize) {
        info!("inferring {func} to have {count} parameters");

        let mut matching = EntityList::new();
        for _ in 0..count {
            let v = self.env.unknown();
            matching.push(v, &mut self.env.var_pool);
            self.queue.extend(v);
            self.queue.new_check(v, Check::DefaultUnknown(v));
        }

        let VariableInfo::Function { params: params @ None, .. } =
            &mut self.env.variables[func].info
        else {
            panic!(
                "infer_to_known called for non-unknown-func var: {:?}",
                self.env.variables[func].info
            );
        };

        *params = Some(matching.clone());

        for v in matching.as_slice(&self.env.var_pool) {
            self.queue.new_link(*v, func);
        }

        for v in matching.as_slice(&self.env.var_pool) {
            self.queue.touch(*v);
        }
    }

    fn applied_unify_apply_or_yield(
        &self,
        appl: key::Application,
        func: key::Var,
    ) -> Option<key::Var> {
        match &self.env.variables[func].info {
            VariableInfo::InferTo(func) => self.applied_unify_apply_or_yield(appl, *func),
            VariableInfo::Function { ret, .. } => Some(*ret),
            VariableInfo::TypeResolvedFunction(name) => {
                info!("not enough function information is known for {func}: {name}");
                None
            }
            // It's a yield, use func tvar instead
            //
            // TODO: This might be overly aggressive for this pass?
            _ if self.env.applications[appl].parameters.is_empty() => Some(func),
            info => {
                error!("attempted to apply non-function {func}: {info:?}");
                None
            }
        }
    }

    fn infer_directly(&mut self, unknown: key::Var, known: key::Var) {
        info!("direct assignment {unknown} -> {known}");

        assert!(matches!(
            self.env.variables[unknown].info,
            VariableInfo::Unknown | VariableInfo::Numeric
        ));

        assert_ne!(unknown, known);

        if self.queue.does_var_contains_parent(unknown, known) {
            panic!("InferTo assignment wouldn't caused cycle, {unknown} -> {known}");
        }

        // NOTE: The parent function ensures that `Numeric` constraint is met.
        self.env.variables[unknown].info = VariableInfo::InferTo(known);

        self.queue.new_link(known, unknown);

        self.queue.touch(unknown);
    }

    fn get_or_instantiate_field(&mut self, var: key::Var, field: key::Field) -> key::Var {
        self.record_field_instantiations
            .entry(var)
            .or_insert_with(|| {
                let (name, type_parameters) = self.env.get_defined(var).unwrap();
                let name = name.clone();

                let forall = Annotation::item(type_parameters);

                let mut fields = key::EntityList::new();
                self.resolver.for_each_field(&name, |_, type_, meta| {
                    let type_ = self.env.instantiate(&forall, &type_, &mut |ident| {
                        self.resolver.map_type_key(meta.clone(), ident)
                    });
                    fields.push(type_, &mut self.env.var_pool);
                });

                InstantiatedFields { fields }
            })
            .fields
            .get(field.0 as usize, &self.env.var_pool)
            .unwrap()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum KnownTypeRoot<Ident> {
    Prim(Prim),
    Tuple,
    // List,
    Defined(Ident),
    Array,
    Pointer,
}

impl<Ident: Clone> TryFrom<&VariableInfo<Ident>> for KnownTypeRoot<Ident> {
    type Error = ();

    fn try_from(state: &VariableInfo<Ident>) -> Result<Self, Self::Error> {
        match state {
            VariableInfo::Prim(prim) => Ok(KnownTypeRoot::Prim(*prim)),
            VariableInfo::Tuple(_) => Ok(KnownTypeRoot::Tuple),
            VariableInfo::List(_) => Err(()),
            VariableInfo::Defined(ident, _) => Ok(KnownTypeRoot::Defined(ident.clone())),
            VariableInfo::Array { .. } => Ok(KnownTypeRoot::Array),
            VariableInfo::Pointer(_) => Ok(KnownTypeRoot::Pointer),

            _ => Err(()),
        }
    }
}

impl<Ident> KnownTypeRoot<Ident> {
    pub fn from_known<T>(
        known: &KnownType<Ident>,
        f: impl FnOnce(&Ident) -> Option<T>,
    ) -> Option<KnownTypeRoot<T>> {
        match known {
            KnownType::Const(_)
            | KnownType::Function { .. }
            | KnownType::Generic(_)
            | KnownType::Error => None,
            KnownType::Defined(ident, _) => f(ident).map(KnownTypeRoot::Defined),
            KnownType::List(_) => None,
            KnownType::Array { .. } => Some(KnownTypeRoot::Array),
            KnownType::Tuple(_) => Some(KnownTypeRoot::Tuple),
            KnownType::Prim(prim) => Some(KnownTypeRoot::Prim(*prim)),
            KnownType::Pointer(_) => Some(KnownTypeRoot::Pointer),
        }
    }

    pub fn map<T>(&self, f: impl FnOnce(&Ident) -> T) -> KnownTypeRoot<T> {
        match self {
            KnownTypeRoot::Prim(prim) => KnownTypeRoot::Prim(*prim),
            KnownTypeRoot::Tuple => KnownTypeRoot::Tuple,
            // KnownTypeRoot::List => KnownTypeRoot::List,
            KnownTypeRoot::Defined(ident) => KnownTypeRoot::Defined(f(ident)),
            KnownTypeRoot::Array => KnownTypeRoot::Array,
            KnownTypeRoot::Pointer => KnownTypeRoot::Pointer,
        }
    }
}

pub enum ReceiverLookupResult {
    CircularInference,
    NotFound,
    Func {
        params: EntityList<key::Var>,
        ret: key::Var,
    },
}

pub trait Resolver<Ident> {
    type ResolvedMetadata: Clone;

    fn implicitly_declare_generic(&mut self, tag: GenericTag) -> super::TaggedGeneric;

    fn receiver_lookup(
        &mut self,
        env: &mut Environment<Ident>,
        root: KnownTypeRoot<Ident>,
        name: &str,
    ) -> ReceiverLookupResult;

    fn type_of_field(
        &self,
        name: &Ident,
        field: key::Field,
    ) -> Result<(Self::ResolvedMetadata, KnownType<Ident>), ResolverError<Ident>>;

    fn guess_by_fields<'a, I>(&self, var: key::Var, fields: I) -> Option<Ident>
    where
        I: Iterator<Item = &'a str> + Clone;

    fn get_field(&self, record: &Ident, name: &str) -> Option<key::Field>;

    fn for_each_type_parameter<F>(&self, name: &Ident, f: F)
    where
        F: FnMut(key::Generic);

    fn for_each_field<F>(&self, name: &Ident, mut f: F)
    where
        F: FnMut(key::Field, KnownType<Ident>, Self::ResolvedMetadata),
    {
        (0..)
            .map(key::Field)
            .map_while(|field| {
                self.type_of_field(name, field)
                    .ok()
                    .map(|(meta, field_type)| f(field, field_type, meta))
            })
            .collect()
    }

    fn map_type_key(&self, in_: Self::ResolvedMetadata, ident: &Ident) -> Ident;
}

#[derive(Debug)]
pub enum ResolverError<Ident> {
    RecordNotFound(Ident),
    FieldNotFound(Ident, key::Field),
}

impl fmt::Debug for LCheck {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LCheck::Check(check) => check.fmt(f),
            LCheck::Parent(var) => write!(f, "parent({var})"),
        }
    }
}

impl fmt::Debug for Check {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assignment(assignment) => write!(f, "{} <==> {}", assignment.lhs, assignment.rhs),
            Self::ParamApplication(application) => application.fmt(f),
            Self::Return(ret) => ret.fmt(f),
            Self::SameAs(sameas) => sameas.fmt(f),
            Self::CheckFields(var) => write!(f, "check-fields({var})"),
            Self::ResolveRecord(record) => write!(f, "resolve-records({record})"),
            Self::TypeSensitiveNameres(appl) => {
                write!(f, "name-res {appl}")
            }
            Self::DefaultList(var) => write!(f, "default-list({var})"),
            Self::DefaultNumber(var) => write!(f, "default-numeric({var})"),
            Self::PoisonTypeSensitive(appl) => {
                write!(f, "poison-name-res({appl})")
            }
            Self::DefaultUnknown(var) => write!(f, "default-unknown({var})"),
        }
    }
}
