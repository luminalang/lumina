use crate::ir;

pub struct Runtime {
    pub instructions: Vec<ir::Entity>,
}

impl Runtime {
    pub fn new(instructions: Vec<ir::Entity>) -> Self {
        Self { instructions }
    }
}
