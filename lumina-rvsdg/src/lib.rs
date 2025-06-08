use derive_more::TryInto;
use derive_new::new;
use lumina_collections::{map_key_impl, Map, MapKey, M};
use std::any::Any;

#[cfg(test)]
mod tests;

mod nodes;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct NodeId(u32);
map_key_impl!(NodeId(u32), "node");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Argument(pub u32);
map_key_impl!(Argument(u32), "a");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Result(pub u32);
map_key_impl!(Result(u32), "r");

#[derive(Clone, Copy, Debug, new)]
pub struct Meta {
    name: &'static str,
    type_: &'static str,
}

impl Meta {
    pub fn int(name: &'static str) -> Self {
        Self { name, type_: "int" }
    }

    pub fn predicate() -> Self {
        Meta { name: "predicate", type_: "bool" }
    }

    pub fn closure(name: &'static str) -> Self {
        Meta { name, type_: "closure" }
    }
}

struct Region {
    arguments: Map<Argument, Meta>,
    results: Map<Result, Meta>,
    nodes: Map<NodeId, Node>,

    edges: Vec<Edge>,
}

#[derive(Debug)]
pub struct Edge {
    origin: Origin,
    user: User,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Origin {
    Output(NodeId, Output),
    Argument(Argument),
}

impl Origin {
    pub fn arg(i: u32) -> Origin {
        Origin::Argument(Argument(i))
    }

    pub fn output(node: NodeId, i: u32) -> Origin {
        Origin::Output(node, Output(i))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum User {
    Input(NodeId, Input),
    Result(Result),
}

impl User {
    pub fn input(node: NodeId, i: u32) -> User {
        User::Input(node, Input(i))
    }

    pub fn result(i: u32) -> User {
        User::Result(Result(i))
    }
}

pub trait NodeKind: std::any::Any {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    // fn on_new_input(node: NodeRefMut<Self>, v: &Meta)
    // where
    //     Self: Sized;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Input(u32);
map_key_impl!(Input(u32), "i");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Output(u32);
map_key_impl!(Output(u32), "o");

#[derive(new)]
pub struct Node {
    inputs: Map<Input, Meta>,
    outputs: Map<Output, Meta>,
    kind: Box<dyn NodeKind>,
}

impl Node {
    pub fn create_translation_unit() -> Node {
        Node {
            inputs: Map::new(),
            outputs: Map::new(),
            kind: Box::new(nodes::TranslationUnit::new()),
        }
    }

    pub fn downcast<Kind: NodeKind>(&self) -> NodeRef<'_, Kind> {
        NodeRef {
            inputs: &self.inputs,
            outputs: &self.outputs,
            kind: self.kind.as_any().downcast_ref().unwrap(),
        }
    }

    pub fn downcast_mut<Kind: NodeKind>(&mut self) -> NodeRefMut<'_, Kind> {
        NodeRefMut {
            inputs: &mut self.inputs,
            outputs: &mut self.outputs,
            kind: self.kind.as_any_mut().downcast_mut().unwrap(),
        }
    }

    // TODO: I'm not sure this makes sense since requests for dependencies go upwards not downwards
    //
    // If we store the entire translation unit in a Context and then have all methods on that
    // context, we could request from anywhere and have it propegate all the way up.
    //
    // Although; considering it's *acyclic* i feel like we shouldn't *need* that.
    //
    // We probably should pass a magic request thing in the ctx.
    // pub fn add_input<Kind: NodeKind>(&mut self, v: Meta) -> Input {
    //     let noderef = self.downcast_mut::<Kind>();
    //     NodeKind::on_new_input(noderef, &v);
    //     self.inputs.push(v)
    // }
}

impl Region {
    fn new() -> Region {
        Region {
            arguments: Map::new(),
            results: Map::new(),
            nodes: Map::new(),
            edges: vec![],
        }
    }

    pub fn insert_argument(&mut self, at: usize, meta: Meta) -> Argument {
        self.arguments.as_mut_vec().insert(at, meta);
        for edge in self.edges.iter_mut() {
            if let Origin::Argument(arg) = &mut edge.origin {
                if arg.0 >= at as u32 {
                    arg.0 += 1;
                }
            }
        }
        Argument(at as u32)
    }

    pub fn get_mut<Kind: NodeKind>(&mut self, id: NodeId) -> NodeRefMut<'_, Kind> {
        self.nodes[id].downcast_mut()
    }

    pub fn add_simple_node<Kind: NodeKind>(&mut self, kind: Kind) -> NodeId {
        let node = Node::new(Map::new(), Map::new(), Box::new(kind));
        self.nodes.push(node)
    }

    pub fn add_do_while_node(&mut self) -> NodeId {
        let pred = Meta::predicate();
        let kind = nodes::DoWhile::new(pred);
        let node = Node::new(Map::new(), [pred].into(), Box::new(kind));
        self.nodes.push(node)
    }

    pub fn add_recenv_node(&mut self) -> NodeId {
        let kind = nodes::RecEnv::new();
        let node = Node::new(Map::new(), Map::new(), Box::new(kind));
        self.nodes.push(node)
    }

    pub fn add_lambda_node(&mut self, name: &'static str) -> NodeId {
        let kind = nodes::Lambda::new(name);
        let node = Node::new(Map::new(), Map::new(), Box::new(kind));
        self.nodes.push(node)
    }

    fn get_origin_meta(&self, origin: Origin) -> Meta {
        match origin {
            Origin::Output(node_id, output) => self.nodes[node_id].outputs[output].clone(),
            Origin::Argument(argument) => self.arguments[argument].clone(),
        }
    }

    pub fn connect_to_new_result(&mut self, origin: Origin) -> Result {
        let meta = self.get_origin_meta(origin);
        let result = self.results.push(meta);
        self.connect(origin, User::Result(result));
        result
    }

    pub fn connect(&mut self, origin: Origin, user: User) {
        todo!("we want connect to also *declare* the points no?");

        // hm... do we? if we do then will that actually properly type check them?
        //
        // yes. I suppose it's the `apply` node that'll actually type check things.
        // let's try it.
        //
        // Perhaps; The input/output shouldn't be metadata but rather the connected stuff directly?
        //
        // Hm. But they *can* end up unconnected. So I don't think that's a good idea.

        match user {
            User::Input(node_id, input) => {
                todo!("");
            }
            User::Result(_) => todo!(),
        }
        self.edges.push(Edge { origin, user });
    }
}

pub struct NodeRefMut<'a, Kind> {
    inputs: &'a mut Map<Input, Meta>,
    outputs: &'a mut Map<Output, Meta>,
    kind: &'a mut Kind,
}

// impl<'a, Kind> NodeRefMut<'a, Kind> {
//     pub fn add_input(self, v: Meta) -> Input {
//         NodeKind::on_new_input(self, &v);
//         // self.kind.on_new_input(&v);
//         self.inputs.push(v)
//     }
// }

pub struct NodeRef<'a, Kind> {
    inputs: &'a Map<Input, Meta>,
    outputs: &'a Map<Output, Meta>,
    kind: &'a Kind,
}
