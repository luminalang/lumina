use crate::{nodes, xml::XmlCtx, Map, Meta, NodeId, NodeKind, NodeRefMut, Origin, Output, Region};

impl NodeKind for Lambda {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn to_xml<'a>(&'a self, xml: &mut XmlCtx<'a>) {
        xml.write_region("", &self.region)
    }

    fn node_type(&self) -> &str {
        "lambda"
    }
}

#[derive(Debug)]
pub struct Lambda {
    name: &'static str,
    pub region: Region,
}

impl Lambda {
    pub fn new(name: &'static str) -> Self {
        Self { name, region: Region::new() }
    }
}
