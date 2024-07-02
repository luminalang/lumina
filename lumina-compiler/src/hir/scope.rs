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
        self.scopes.pop().unwrap().captures
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
        let (i, bind) = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find_map(|(i, scope)| {
                scope
                    .binds
                    .iter()
                    .find_map(|(bind, n)| (*n == name).then_some((i, *bind)))
            })?;

        if i != 0 {
            include(&mut self.scopes.last_mut().unwrap().captures, bind);
        }
        self.use_count[bind] += 1;

        Some(bind)
    }
}

fn include(captures: &mut Vec<key::Bind>, bind: key::Bind) {
    if !captures.contains(&bind) {
        captures.push(bind)
    }
}
