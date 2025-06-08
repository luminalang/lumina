use crate::NodeKind;

impl NodeKind for Apply {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

pub struct Apply {}

impl Apply {
    pub fn new() -> Self {
        Self {}
    }
}
