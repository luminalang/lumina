use crate::{xml::XmlCtx, NodeKind};

impl NodeKind for Constant {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn to_xml<'a>(&'a self, xml: &mut XmlCtx<'a>) {}

    fn node_type(&self) -> &str {
        "simple"
    }
}

#[derive(Debug)]
pub struct Constant(pub usize);
