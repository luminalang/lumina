use crate::{nodes, Argument, Map, Meta, Node, NodeId, NodeKind, NodeRefMut, Origin, Region, User};

impl NodeKind for RecEnv {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

pub struct RecEnv {
    // We protect Region so that only lambdas can be created and verified
    region: Region,
    // lambdas: Map<NodeId, Node>,
}

impl RecEnv {
    pub fn new() -> Self {
        RecEnv { region: Region::new() }
    }
}

impl<'a> NodeRefMut<'a, RecEnv> {
    pub fn get_mut(&mut self, lambda: NodeId) -> NodeRefMut<'_, nodes::Lambda> {
        self.kind.region.get_mut::<nodes::Lambda>(lambda)
    }

    pub fn add_lambda_node(&mut self, name: &'static str) -> NodeId {
        let meta = Meta::closure(name);
        let region = &mut self.kind.region;

        let node_id = region.add_lambda_node(name);

        let lambda_argument = region.arguments.push(meta);

        // Make this closure available to all lambda nodes
        for lambda_id in region.nodes.keys() {
            let lambdaref = region.get_mut::<nodes::Lambda>(lambda_id);
            let input = lambdaref.inputs.push(meta);
            lambdaref.kind.region.arguments.push(meta);
            region.connect(
                Origin::Argument(lambda_argument),
                User::Input(lambda_id, input),
            );
        }

        node_id
    }

    pub fn add_parameter_to_lambda(&mut self, lambda: NodeId, param: Meta) -> Argument {
        let lambda_offset = self.outputs.len();
        self.kind
            .region
            .get_mut::<nodes::Lambda>(lambda)
            .kind
            .region
            .insert_argument(lambda_offset, param)
    }
}
