use crate::{xml::XmlCtx, Meta, NodeKind};

impl NodeKind for Apply {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn to_xml<'a>(&'a self, xml: &mut crate::xml::XmlCtx<'a>) {}

    fn node_type(&self) -> &str {
        "simple"
    }
}

#[derive(Debug)]
pub struct Apply {}

impl Apply {
    pub fn new() -> Self {
        Self {}
    }
}
