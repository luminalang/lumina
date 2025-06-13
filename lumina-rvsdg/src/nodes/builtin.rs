use crate::{xml::XmlCtx, NodeKind};

impl NodeKind for Builtin {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn to_xml<'a>(&'a self, _: &mut XmlCtx<'a>) {}

    fn node_type(&self) -> &str {
        "simple"
    }
}

#[derive(Debug)]
pub struct Builtin(pub &'static str);
