use super::{Input, Node, NodeKey, Output, Value};
use derive_more::From;
use derive_new::new;
use lumina_collections::{map_key_impl, Map, M};
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct RegionKey(u32);
map_key_impl!(RegionKey(u32), "region");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Argument(pub u32);
map_key_impl!(Argument(u32), "a");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Result(pub u32);
map_key_impl!(Result(u32), "r");

// TODO: Figure out whether we can encapsulate this in such a way that these may remain private.
pub struct Region<Meta> {
    pub nodes: Map<NodeKey, Node<Meta>>,
    pub edges: Vec<Edge>,

    pub sig: RegionSignature<Meta>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, new)]
pub struct RegionSignature<Meta> {
    #[new(default)]
    pub arguments: Map<Argument, Value<Meta>>,
    #[new(default)]
    pub results: Map<Result, Value<Meta>>,
}

impl<Meta> RegionSignature<Meta> {
    pub fn from(arguments: Vec<Value<Meta>>, results: Vec<Value<Meta>>) -> Self {
        Self { arguments: arguments.into(), results: results.into() }
    }
}

#[derive(Debug)]
pub struct Edge {
    origin: Origin,
    user: User,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Origin {
    Output(NodeKey, Output),
    Argument(Argument),
}

impl Origin {
    pub fn arg(i: u32) -> Origin {
        Origin::Argument(Argument(i))
    }

    pub fn output(node: NodeKey, i: u32) -> Origin {
        Origin::Output(node, Output(i))
    }
}

impl User {
    pub fn input(node: NodeKey, i: u32) -> User {
        User::Input(node, Input(i))
    }

    pub fn result(i: u32) -> User {
        User::Result(Result(i))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum User {
    Input(NodeKey, Input),
    Result(Result),
}

impl<Meta> From<RegionSignature<Meta>> for Region<Meta> {
    fn from(sig: RegionSignature<Meta>) -> Self {
        Region { nodes: Map::new(), edges: vec![], sig }
    }
}

impl<Meta> Region<Meta> {
    pub fn new(sig: RegionSignature<Meta>) -> Self {
        Self::from_raw_parts(Map::new(), vec![], sig)
    }

    fn from_raw_parts(
        nodes: Map<NodeKey, Node<Meta>>,
        edges: Vec<Edge>,
        sig: RegionSignature<Meta>,
    ) -> Self {
        Self { nodes, edges, sig }
    }

    fn get_user_sig(&self, user: User) -> &Value<Meta> {
        match user {
            User::Input(node_key, input) => &self.nodes[node_key].sig.inputs[input],
            User::Result(result) => &self.sig.results[result],
        }
    }

    fn get_origin_sig(&self, origin: Origin) -> &Value<Meta> {
        // PERHAPS: we don't need Value::Lambda and we can instead be completely opaque
        // by cloning the Meta from the output of the discovered node here?
        //
        // That's a very interesting idea.

        match origin {
            Origin::Output(node_key, output) => &self.nodes[node_key].sig.outputs[output],
            Origin::Argument(argument) => &self.sig.arguments[argument],
        }
    }

    pub fn connect(&mut self, origin: Origin, user: User) {
        self.edges.push(Edge { origin, user });
    }
}

impl<Meta: PartialEq + std::fmt::Debug> Region<Meta> {
    pub fn verify(&self) {
        for edge in &self.edges {
            match edge.user {
                User::Input(node_key, input) => {
                    let ty = self.get_origin_sig(edge.origin);
                    assert_eq!(&self.nodes[node_key].sig.inputs[input], ty);
                }
                User::Result(_) => {}
            }
        }
    }
}

impl<Meta: Clone> Region<Meta> {
    fn get_origin_signatures<F>(&self, origins: &[Origin]) -> F
    where
        F: FromIterator<Value<Meta>>,
    {
        origins
            .iter()
            .map(|&origin| self.get_origin_sig(origin))
            .cloned()
            .collect()
    }

    pub fn int(&mut self, int: Value<Meta>, n: usize) -> NodeKey {
        self.nodes.push(Node::int(int, n))
    }
    pub fn add(&mut self, int: Value<Meta>) -> NodeKey {
        self.nodes.push(Node::add(int))
    }
    pub fn sub(&mut self, int: Value<Meta>) -> NodeKey {
        self.nodes.push(Node::sub(int))
    }

    // TODO: maybe callback pattern is a bad idea? Probably makes this API less flexible.
    pub fn gamma<F, D>(
        &mut self,
        pred: Origin,
        dependencies: &[Origin],
        regions: usize,
        f: F,
    ) -> NodeKey
    where
        F: FnOnce(&mut Map<RegionKey, Region<Meta>>),
    {
        let pred_ty = self.get_origin_sig(pred).clone();
        let deps = dependencies.iter().map(|&o| self.get_origin_sig(o).clone());

        let node = Node::gamma(pred_ty, deps, regions, f);
        let node = self.nodes.push(node);

        self.connect(pred, User::input(node, 0));

        for (i, origin) in dependencies.iter().cloned().enumerate() {
            self.connect(origin, User::input(node, i as u32 + 1));
        }

        node
    }

    pub fn theta<F>(&mut self, pred: Origin, inputs: &[Origin], f: F) -> NodeKey
    where
        F: FnOnce(&mut Region<Meta>),
    {
        let spred = self.get_origin_sig(pred).clone();
        let sinputs = self.get_origin_signatures(inputs);
        let node = self.nodes.push(Node::theta(spred, sinputs, f));

        for (i, &origin) in inputs.iter().enumerate() {
            self.connect(origin, User::input(node, i as u32));
        }

        node
    }

    pub fn lambda(
        &mut self,
        depdenencies_first: bool,
        dependencies: &[Origin],
        params: &[Origin],
        returns: impl IntoIterator<Item = Value<Meta>> + Clone,
        f: impl FnOnce(&mut Region<Meta>, usize),
    ) -> NodeKey {
        let sdependencies = self.get_origin_signatures(dependencies);
        let sparams = self.get_origin_signatures::<Vec<_>>(params);

        let node = self.nodes.push(Node::lambda(
            depdenencies_first,
            sdependencies,
            sparams,
            returns,
            f,
        ));

        for (i, &origin) in params.iter().enumerate() {
            self.connect(origin, User::input(node, i as u32));
        }

        node
    }

    pub fn delta<F>(&mut self, inputs: &[Origin], output: Value<Meta>, f: F) -> NodeKey
    where
        F: FnOnce(&mut Region<Meta>),
    {
        let sinputs = self.get_origin_signatures(inputs);
        let node = self.nodes.push(Node::delta(sinputs, output, f));

        for (i, &origin) in inputs.iter().enumerate() {
            self.connect(origin, User::input(node, i as u32));
        }

        node
    }

    pub fn phi<const N: usize>(
        &mut self,
        signatures: [RegionSignature<Meta>; N],
        bodies: impl FnMut(usize, [Origin; N], &mut Region<Meta>),
    ) -> NodeKey {
        self.nodes.push(Node::phi(signatures, bodies))
    }
}

impl<Meta: Clone> Region<Meta> {
    pub fn apply(&mut self, lambda: Origin, params: &[Origin]) -> NodeKey {
        let node = self.nodes.push(Node::apply());

        let lambda_signature = self.get_origin_sig(lambda).clone();
        let f = self.nodes[node].sig.inputs.push(lambda_signature);
        self.connect(lambda, User::Input(node, f));

        // TODO: Do we need this information elsewhere? Is discarding it really valid?
        //
        // How do we make this more convenient and safe?
        params.into_iter().for_each(|&origin| {
            let ty = self.get_origin_sig(origin).clone();
            let param = self.nodes[node].sig.inputs.push(ty);
            self.connect(origin, User::Input(node, param));
        });

        node
    }
}
