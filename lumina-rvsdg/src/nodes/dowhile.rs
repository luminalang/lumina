use crate::{Map, Meta, NodeKind, Region};

impl NodeKind for DoWhile {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    // fn on_new_input(node: NodeRefMut<Self>, input: &Meta) {
    //     node.outputs.push(input.clone());
    //     node.kind.region.arguments.push(input.clone());
    //     node.kind.region.results.push(input.clone());
    // }
}

pub struct DoWhile {
    region: Region,
}

impl DoWhile {
    pub fn new(pred: Meta) -> Self {
        DoWhile {
            region: Region {
                arguments: Map::new(),
                results: [pred].into(),
                nodes: Map::new(),
                edges: vec![],
            },
        }
    }
}

// |ctx, predicate| {
//   ctx.resolve();
// }
//
// Ok but if we *are* gonna make a builder like this. Then; once again it definitely means we
// should make this initial RVSDG IR thing much simpler and not have any fancy abstractions.
//
// One really cool thing about using trait objects for this is that it makes adding different
// simple nodes for different passes really simple
//
// But I'm not sure what it implies for opts yet
//
// Hm. We might want to have a combined approach. Dynamic object for simple nodes and sum type for
// the rest.
