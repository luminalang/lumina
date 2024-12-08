use crate::prelude::*;

#[derive(Debug)]
pub struct Bindings<'s> {
    scopes: Vec<Scope<'s>>,
    use_count: Map<key::Bind, usize>,
}

#[derive(Debug, Default)]
struct Scope<'s> {
    binds: Vec<(key::Bind, &'s str)>,
    captures: Vec<key::Bind>,
}

impl<'s> Default for Bindings<'s> {
    fn default() -> Self {
        Self { scopes: vec![Scope::default()], use_count: Map::new() }
    }
}

type Captures = Vec<key::Bind>;

impl<'s> Bindings<'s> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn enter(&mut self) {
        self.scopes.push(Scope::default());
    }
    pub fn leave(&mut self) -> Captures {
        let scope = self.scopes.pop().unwrap();
        scope.captures
    }

    pub fn declare_nameless(&mut self) -> key::Bind {
        let bind = self.use_count.push(0);
        trace!("declaring nameless {bind}");
        bind
    }

    pub fn declare(&mut self, name: &'s str) -> key::Bind {
        let bind = self.use_count.push(0);
        trace!("declaring {name} as {bind}");
        self.scopes.last_mut().unwrap().binds.push((bind, name));
        bind
    }

    pub fn resolve(&mut self, name: &'s str) -> Option<key::Bind> {
        Self::resolve_in(&mut self.scopes, name).map(|bind| {
            self.use_count[bind] += 1;
            bind
        })
    }

    fn resolve_in(scopes: &mut [Scope<'s>], name: &'s str) -> Option<key::Bind> {
        let (scope, xs) = scopes.split_last_mut()?;

        scope
            .binds
            .iter()
            .rev()
            .find_map(|(bind, n)| (name == *n).then_some(*bind))
            .or_else(|| {
                Self::resolve_in(xs, name).map(|bind| {
                    if !scope.captures.contains(&bind) {
                        trace!("capturing {bind}");
                        scope.captures.push(bind);
                    }
                    bind
                })
            })
    }
}
