use super::*;

struct Builder<'a> {
    ssa: &'a SSA,
}

impl<'a> Builder<'a> {
    pub fn new(ssa: &'a SSA) -> Self {
        Builder { ssa }
    }
}
