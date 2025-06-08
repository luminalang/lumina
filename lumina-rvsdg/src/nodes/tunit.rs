use crate::{nodes, Map, Meta, Node, NodeId, NodeKind, NodeRefMut, Region};

impl NodeKind for TranslationUnit {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

// TODO: could be nice to have Node<Lambda> instead
pub struct TranslationUnit {
    pub region: Region,
}

impl TranslationUnit {
    pub fn new() -> Self {
        todo!();
    }
}

// TODO: Could be nice to have `NodeId<Lambda>` instead
impl<'a> NodeRefMut<'a, TranslationUnit> {
    // pub fn define_function_item(&mut self, public: bool, name: &'static str) -> NodeId {
    //     let kind = nodes::Lambda::new();
    //     let node = Node::new(Map::new(), Map::new(), Box::new(kind));
    //     let nodeid = self.kind.region.push(node);
    //     if public {
    //         self.outputs.push(Meta::new(name, "closure"));
    //     }
    //     nodeid
    // }
}
