use crate::{
    nodes, xml::XmlCtx, Argument, Input, KNodeId, Map, Meta, Node, NodeId, NodeKind, NodeRefMut,
    Origin, Output, Region, User,
};

impl NodeKind for RecEnv {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn to_xml<'a>(&'a self, xml: &mut XmlCtx<'a>) {
        xml.write_region("Recursive Environment", &self.region);
    }

    fn node_type(&self) -> &str {
        "phi"
    }
}

#[derive(Debug)]
pub struct RecEnv {
    // We protect Region so that only lambdas can be created and verified
    region: Region,

    // The arguments in `Region` that correspond to an added lambda
    lambdas: Vec<(Argument, Meta)>,
}

impl RecEnv {
    pub fn new() -> Self {
        RecEnv { region: Region::new(), lambdas: vec![] }
    }
}

impl<'a> NodeRefMut<'a, RecEnv> {
    pub fn get_mut(&mut self, lambda: KNodeId<nodes::Lambda>) -> NodeRefMut<'_, nodes::Lambda> {
        self.kind.region.get_mut(lambda)
    }

    pub fn add_lambda_node(&mut self, name: &'static str) -> (KNodeId<nodes::Lambda>, Output) {
        let meta = Meta::closure(name);
        let region = &mut self.kind.region;

        let lambda_argument = region.arguments.push(meta);

        // Add this lambda closure to all previous lambdas
        for lambda_id in region.nodes.keys() {
            let lambdaref = region.get_mut(KNodeId::<nodes::Lambda>::new(lambda_id));
            let input = lambdaref.inputs.push(meta);
            lambdaref.kind.region.add_argument(meta);

            // Implicitly connect the closure from the Phi region to the lambda node
            region.connect(
                Origin::Argument(lambda_argument),
                User::Input(lambda_id, input),
            );
        }

        let (node_id, _) = region.add_lambda_node(name, meta);

        self.kind.lambdas.push((lambda_argument, meta));

        // Add all closures as implicit inputs+arguments to this lambda
        for (argument, meta) in &self.kind.lambdas {
            let lambdaref = region.get_mut(node_id);
            let input = lambdaref.inputs.push(*meta);
            lambdaref.kind.region.add_argument(*meta);

            // Implicitly connect the closure from the Phi region to the lambda node
            region.connect(Origin::Argument(*argument), User::Input(node_id.id, input));
        }

        // Export this lambda in the phi nodes outputs
        let output = self.outputs.push(meta);

        // Add a result in the phi region for this lambda and connect the lambda to it
        {
            let result = self.kind.region.add_result(meta);

            let (origin, _) = self.kind.region.closure_output(node_id);
            self.kind.region.connect(origin, result);
        }

        (node_id, output)
    }

    // pub fn get_phi_lambda_param(&self, param: usize) -> Origin {
    //     Origin::Argument(Argument(param as u32))
    // }

    pub fn get_phi_closure(
        &self,
        from: KNodeId<nodes::Lambda>,
        closure: KNodeId<nodes::Lambda>,
    ) -> Origin {
        let num_of_closures = self.kind.lambdas.len();
        let lambda_arguments = &self.kind.region.get(from).kind.region.arguments;
        dbg!(lambda_arguments.len() as u32 - num_of_closures as u32 + closure.id.0);
        Origin::arg(lambda_arguments.len() as u32 - num_of_closures as u32 + closure.id.0)
    }

    // pub fn get_lambda_parameter(&self, lambda: KNodeId<nodes::Lambda>, param: usize) -> Origin {
    // }

    pub fn add_parameter_to_lambda(
        &mut self,
        lambda: KNodeId<nodes::Lambda>,
        param: Meta,
    ) -> Origin {
        // TODO: Wait; but isn't this supposed to always start from 0? What are these offsets!?!
        //
        // Oh right; we want to insert inbetween.

        let region = &mut self.kind.region.get_mut(lambda).kind.region;
        let lambda_offset = region.arguments.len() - self.kind.lambdas.len();
        let argument = region.insert_argument(lambda_offset, param);
        Origin::Argument(argument)
    }
}
