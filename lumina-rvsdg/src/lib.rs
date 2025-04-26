use derive_new::new;
use lumina_collections::{map_key_impl, Map, MapKey, M};

mod region;
mod verifier;
use region::{Argument, Edge, Origin, Region, RegionKey, RegionSignature, Result, User};
#[cfg(test)]
mod tests;

#[derive(Clone, Copy, PartialEq, Eq)]
struct NodeKey(u32);
map_key_impl!(NodeKey(u32), "node");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Input(u32);
map_key_impl!(Input(u32), "i");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Output(u32);
map_key_impl!(Output(u32), "o");

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum Value<Meta> {
    Meta(Meta),
    Lambda(RegionSignature<Meta>),
    State,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, new)]
struct NodeSignature<Meta> {
    #[new(default)]
    inputs: Map<Input, Value<Meta>>,
    #[new(default)]
    outputs: Map<Output, Value<Meta>>,
}

struct Node<Meta> {
    regions: Map<RegionKey, Region<Meta>>,
    kind: NodeKind,
    sig: NodeSignature<Meta>,
}

enum NodeKind {
    Simple(SimpleNodeKind),
    Structural(StructuralNodeKind),
}

enum SimpleNodeKind {
    Const(usize),
    Unit,
    Apply,
    Add,
    Sub,
}

enum StructuralNodeKind {
    // Intra-procedural nodes
    Gamma,
    Theta,

    // Inter-procedural nodes
    Lambda { dependencies: usize },
    Delta,
    Phi,
    Omega,
}

impl<Meta: Clone> Node<Meta> {
    /// The γ node acts like a switch expression.
    pub fn gamma<F, D>(pred: Value<Meta>, dependencies: D, regions: usize, f: F) -> Node<Meta>
    where
        F: FnOnce(&mut Map<RegionKey, Region<Meta>>),
        D: IntoIterator<Item = Value<Meta>> + Clone,
    {
        // TODO: Is it acceptable for there to be multiple outputs as long as they match the inner
        // region?

        let region_arguments: Map<Argument, _> = dependencies.clone().into_iter().collect();
        let inputs = [pred].into_iter().chain(dependencies).collect();

        let mut regions = (0..regions)
            .map(|_| Region {
                nodes: Map::new(),
                edges: vec![],
                sig: RegionSignature { arguments: region_arguments.clone(), results: Map::new() },
            })
            .collect::<Map<RegionKey, _>>();

        f(&mut regions);

        let mut output = None;
        for (_, region) in regions.iter() {
            let o = &region.sig.results;

            match &output {
                None => output = Some(o.cast_key()),
                Some(_prev) => {
                    // TODO: Would be nice to verify that these match here.
                }
            }
        }

        Node {
            regions,
            kind: NodeKind::Structural(StructuralNodeKind::Gamma),
            sig: NodeSignature { inputs, outputs: output.unwrap() },
        }
    }

    /// The Θ node is a tail controlled loop
    pub fn theta<F>(pred: Value<Meta>, inputs: Map<Input, Value<Meta>>, f: F) -> Node<Meta>
    where
        F: FnOnce(&mut Region<Meta>),
    {
        let sig = RegionSignature {
            arguments: inputs.cast_key(),
            results: [pred].into_iter().chain(inputs.values().cloned()).collect(),
        };
        let mut region = Region::new(sig);

        f(&mut region);

        Node {
            regions: [region].into(),
            kind: NodeKind::Structural(StructuralNodeKind::Theta),
            sig: NodeSignature { outputs: inputs.cast_key(), inputs },
        }
    }

    /// Arguments:
    ///
    /// * `dependencies`: External dependencies such as lambdas or deltas needed by this lambda
    /// * `params`: Function parameter values for the lambda
    /// * `returns`: Function return values for the lambda
    /// * `f`: Visitor of created region placed in this lambda
    pub fn lambda(
        depdenencies_first: bool,
        dependencies: Map<Input, Value<Meta>>,
        params: impl IntoIterator<Item = Value<Meta>> + Clone,
        returns: impl IntoIterator<Item = Value<Meta>> + Clone,
        f: impl FnOnce(&mut Region<Meta>, usize),
    ) -> Node<Meta> {
        // The lambda inputs are its dependencies, *not* parameters.
        let inputs = dependencies.clone();

        // The lambda node outputs a singular lambda value
        let outputs = [Value::Lambda(RegionSignature {
            arguments: params.clone().into_iter().collect(),
            results: returns.clone().into_iter().collect(),
        })]
        .into();

        // The region arguments will be the dependencies followed by the function parameters
        let dep_offset = dependencies.len();

        let dependencies = dependencies.into_iter().map(|(_, v)| v);
        let arguments = if depdenencies_first {
            dependencies.chain(params).collect()
        } else {
            params.into_iter().chain(dependencies).collect()
        };

        let results = returns.into_iter().collect();

        // Set up the arguments automatically
        let mut region = Region::from(RegionSignature { arguments, results });

        f(&mut region, dep_offset);

        Node {
            sig: NodeSignature { inputs, outputs },
            regions: [region].into(),
            kind: NodeKind::Structural(StructuralNodeKind::Lambda { dependencies: dep_offset }),
        }
    }

    /// The δ node has one region representing the constants value.
    ///
    /// δ nodes may take inputs for dependencies which will be available as arugments in the region. .
    fn delta<F>(inputs: Map<Input, Value<Meta>>, output: Value<Meta>, f: F) -> Node<Meta>
    where
        F: FnOnce(&mut Region<Meta>),
    {
        let sig = RegionSignature {
            arguments: inputs.cast_key(),
            results: [output.clone()].into(),
        };
        let mut region = Region::from(sig);
        f(&mut region);
        Node {
            regions: [region].into(),
            kind: NodeKind::Structural(StructuralNodeKind::Delta),
            sig: NodeSignature { inputs, outputs: [output].into() },
        }
    }

    /// The ϕ node has one region which acts as the environment for lambdas which may use each
    /// other recursively.
    ///
    /// ϕ nodes may take inputs for dependencies.
    /// The ϕ nodes arguments is for each of the contained lambdas, these are then given as input
    /// to each of those lambdas so they may depend on each other.
    pub fn phi<const N: usize>(
        signatures: [RegionSignature<Meta>; N],
        // larguments: [Map<Argument, Value<Meta>>; N],
        // lresults: [Map<Result, Value<Meta>>; N],
        mut bodies: impl FnMut(usize, [Origin; N], &mut Region<Meta>),
    ) -> Node<Meta> {
        let dependencies = Map::new();

        // Turn the signature into lambda *values* for the node outputs and region results
        let lambda_nodes_as_values = signatures
            .iter()
            .cloned()
            .map(Value::Lambda)
            .collect::<Map<Output, _>>();

        let nodes = signatures
            .into_iter()
            .enumerate()
            .map(|(i, sig)| {
                let lambda_origins = {
                    let mut i = 0;
                    [(); N].map(|_| {
                        i += 1;
                        Origin::arg(sig.arguments.len() as u32 + i - 1)
                    })
                };

                let dependencies = lambda_nodes_as_values.cast_key();
                let params = sig.arguments.into_values();
                let results = sig.results.into_values();
                Node::lambda(false, dependencies, params, results, |region, _| {
                    bodies(i, lambda_origins, region);
                })
            })
            .collect();

        let sig = RegionSignature {
            arguments: lambda_nodes_as_values.cast_key(),
            results: lambda_nodes_as_values.cast_key(),
        };
        let mut region = Region::new(sig);
        region.nodes = nodes;

        // TODO: This assumes that the inner lambdas *only* use the phi lambdas as inputs.
        //
        // I'm not sure whether that's valid or not.

        // Connect each lambda argument from the phi region to each lambda
        for node in region.nodes.keys() {
            for i in 0..N {
                region.connect(Origin::arg(i as u32), User::input(node, i as u32));
            }
        }

        Node {
            sig: NodeSignature { inputs: dependencies, outputs: lambda_nodes_as_values },
            regions: [region].into(),
            kind: NodeKind::Structural(StructuralNodeKind::Phi),
        }
    }

    /// The OMEGA node has no inputs or outputs but its results are the lambdas that are exposed by
    /// this translation unit.
    //
    // For our current testing phase, we simply do everything in a single translation unit and
    // thus the only result is the main lambda.
    fn omega(f: impl FnOnce(&mut Region<Meta>)) -> Node<Meta> {
        let mut region = Region::new(RegionSignature::new());
        f(&mut region);
        Node {
            sig: NodeSignature::new(),
            regions: [region].into(),
            kind: NodeKind::Structural(StructuralNodeKind::Omega),
        }
    }
}

impl<Meta: Clone> Node<Meta> {
    fn int(int: Value<Meta>, n: usize) -> Self {
        Node {
            kind: NodeKind::Simple(SimpleNodeKind::Const(n)),
            regions: Map::new(),
            sig: NodeSignature { inputs: Map::new(), outputs: [int].into() },
        }
    }

    fn add(int: Value<Meta>) -> Self {
        Node::simple(
            SimpleNodeKind::Add,
            [int.clone(), int.clone()].into(),
            [int].into(),
        )
    }

    fn sub(int: Value<Meta>) -> Self {
        Node::simple(
            SimpleNodeKind::Sub,
            [int.clone(), int.clone()].into(),
            [int].into(),
        )
    }
}

impl<Meta> Node<Meta> {
    fn apply() -> Self {
        Self::simple(SimpleNodeKind::Apply, Map::new(), Map::new())
    }

    pub fn simple(
        kind: SimpleNodeKind,
        inputs: Map<Input, Value<Meta>>,
        outputs: Map<Output, Value<Meta>>,
    ) -> Self {
        Node {
            kind: NodeKind::Simple(kind),
            regions: Map::new(),
            sig: NodeSignature { inputs, outputs },
        }
    }
}

struct ParentQuest {
    additional_dependencies: (),
}

fn cast_key_or_empty<U: MapKey, K: MapKey, T: Clone>(map: &[Map<K, T>]) -> Map<U, T> {
    map.first()
        .map(|v| v.cast_key::<U>())
        .unwrap_or_else(Map::new)
}
