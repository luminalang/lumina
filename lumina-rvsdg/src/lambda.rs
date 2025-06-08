use crate::{nodes, Map, Meta, NodeId, NodeKind, NodeRefMut, Region};

impl NodeKind for Lambda {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

pub struct Lambda {
    name: &'static str,
    pub region: Region,
}

impl Lambda {
    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            region: Region {
                arguments: Map::new(),
                results: todo!(),
                nodes: todo!(),
                edges: todo!(),
            },
        }
    }
}
