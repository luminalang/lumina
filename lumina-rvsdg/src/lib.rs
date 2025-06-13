use derive_more::TryInto;
use derive_new::new;
use lumina_collections::{map_key_impl, Map, MapKey, M};
use std::ops::{Index, IndexMut};
use std::{any::Any, marker::PhantomData};
use tracing::trace;

#[cfg(test)]
mod tests;

mod nodes;

mod xml;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct NodeId(u32);
map_key_impl!(NodeId(u32), "node");

pub struct KNodeId<T> {
    pub id: NodeId,
    _data: PhantomData<T>,
}

impl<T> Clone for KNodeId<T> {
    fn clone(&self) -> Self {
        KNodeId::new(self.id)
    }
}

impl<T> Copy for KNodeId<T> {}

impl<T> KNodeId<T> {
    fn new(id: NodeId) -> Self {
        Self { id, _data: PhantomData }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Argument(pub u32);
map_key_impl!(Argument(u32), "a");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Result(pub u32);
map_key_impl!(Result(u32), "r");

// TODO: PERHAPS: We shouldn't have any sort of meta at all inside the RVSDG.
//
// Instead; the user can have their own lookups/hashmaps that map the region arguments to types.
//
// Then; the RVSDG could simply expose some method that traverses an origin until the upmost argument.
//
// Hm. I suppose we want an arena-allocation of regions/nodes and that way we can more easily
// attach information?
#[derive(Clone, Copy, Debug, new, PartialEq)]
pub struct Meta {
    name: &'static str,
    type_: &'static str,
}

impl Meta {
    pub fn int(name: &'static str) -> Self {
        Self { name, type_: "int" }
    }

    pub fn closure(name: &'static str) -> Self {
        Meta { name, type_: "closure" }
    }
}

#[derive(Debug)]
pub struct Region {
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

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
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

pub trait NodeKind: std::any::Any + std::fmt::Debug {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn node_type(&self) -> &str;
    fn to_xml<'a>(&'a self, xml: &mut xml::XmlCtx<'a>);
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Input(u32);
map_key_impl!(Input(u32), "i");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Output(u32);
map_key_impl!(Output(u32), "o");

#[derive(new, Debug)]
pub struct Node {
    inputs: Map<Input, Meta>,
    outputs: Map<Output, Meta>,
    kind: Box<dyn NodeKind>,
    name: String,
}

impl Node {
    pub fn create_translation_unit(name: &str) -> Node {
        Node {
            inputs: Map::new(),
            outputs: Map::new(),
            kind: Box::new(nodes::TranslationUnit::new()),
            name: name.to_string(),
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

    pub fn add_output<Kind: NodeKind>(&mut self, node: KNodeId<Kind>, m: Meta) -> Origin {
        let output = self.get_mut(node).outputs.push(m);
        Origin::Output(node.id, output)
    }
    pub fn add_input<Kind: NodeKind>(&mut self, node: KNodeId<Kind>, m: Meta) -> User {
        let input = self.get_mut(node).inputs.push(m);
        User::Input(node.id, input)
    }
    pub fn add_argument(&mut self, m: Meta) -> Origin {
        let arg = self.arguments.push(m);
        Origin::Argument(arg)
    }
    pub fn add_result(&mut self, m: Meta) -> User {
        let result = self.results.push(m);
        User::Result(result)
    }

    pub fn get<Kind: NodeKind>(&self, id: KNodeId<Kind>) -> NodeRef<'_, Kind> {
        self.nodes[id.id].downcast()
    }

    pub fn get_mut<Kind: NodeKind>(&mut self, id: KNodeId<Kind>) -> NodeRefMut<'_, Kind> {
        self.nodes[id.id].downcast_mut()
    }

    pub fn add_apply_node(&mut self, ret: Meta) -> (KNodeId<nodes::Apply>, Origin) {
        let kind = nodes::Apply {};
        let node = Node::new(Map::new(), [ret].into(), Box::new(kind), "apply".into());
        let node_id = self.nodes.push(node);
        (KNodeId::new(node_id), Origin::output(node_id, 0))
    }

    pub fn add_const_node(&mut self, n: usize, meta: Meta) -> (KNodeId<nodes::Constant>, Origin) {
        let kind = nodes::Constant(n);
        let node = Node::new(Map::new(), [meta], Box::new(kind), n.to_string());
        let node_id = self.nodes.push(node);
        (KNodeId::new(node_id), Origin::output(node_id, 0))
    }

    pub fn add_placeholder_node(&mut self, builtin: &'static str) -> KNodeId<nodes::Placeholder> {
        let kind = nodes::Placeholder(builtin);
        let node = Node::new(Map::new(), Map::new(), Box::new(kind), builtin.to_string());
        let node_id = self.nodes.push(node);
        KNodeId::new(node_id)
    }

    pub fn add_do_while_node(&mut self, pred: Meta) -> (KNodeId<nodes::DoWhile>, Origin) {
        let kind = nodes::DoWhile::new(pred);
        let node = Node::new(Map::new(), [pred].into(), Box::new(kind), "do while".into());
        let node_id = self.nodes.push(node);
        (KNodeId::new(node_id), Origin::output(node_id, 0))
    }

    pub fn add_recenv_node(&mut self) -> KNodeId<nodes::RecEnv> {
        let kind = nodes::RecEnv::new();
        let node = Node::new(Map::new(), Map::new(), Box::new(kind), "rec env".into());
        let node_id = self.nodes.push(node);
        KNodeId::new(node_id)
    }

    pub fn add_lambda_node(
        &mut self,
        name: &'static str,
        meta: Meta,
    ) -> (KNodeId<nodes::Lambda>, Origin) {
        let kind = nodes::Lambda::new(name);
        let node = Node::new(Map::new(), [meta].into(), Box::new(kind), name.to_string());
        let node_id = self.nodes.push(node);
        (KNodeId::new(node_id), Origin::output(node_id, 0))
    }

    fn get_origin_meta(&self, origin: Origin) -> Meta {
        trace!("getting metadata for {origin:?}");
        match origin {
            Origin::Output(node_id, output) => self.nodes[node_id].outputs[output].clone(),
            Origin::Argument(argument) => self.arguments[argument].clone(),
        }
    }

    fn get_user_meta(&self, user: User) -> Meta {
        match user {
            User::Input(node_id, input) => self.nodes[node_id].inputs[input],
            User::Result(result) => self.results[result],
        }
    }

    pub fn connect_to_new_result(&mut self, origin: Origin) -> Result {
        let meta = self.get_origin_meta(origin);
        let result = self.results.push(meta);
        self.connect(origin, User::Result(result));
        result
    }

    pub fn connect(&mut self, origin: Origin, user: User) {
        assert_eq!(
            self.get_origin_meta(origin).type_,
            self.get_user_meta(user).type_
        );
        self.edges.push(Edge { origin, user });
    }

    pub fn closure_output(&self, node: KNodeId<nodes::Lambda>) -> (Origin, Meta) {
        // Lambda nodes always only have one output (the closure)
        let outputs = &self.get(node).outputs;
        assert_eq!(outputs.len(), 1);
        (Origin::output(node.id, 0), outputs[Output(0)])
    }
}

pub struct NodeRefMut<'a, Kind> {
    inputs: &'a mut Map<Input, Meta>,
    outputs: &'a mut Map<Output, Meta>,
    kind: &'a mut Kind,
}

pub struct NodeRef<'a, Kind> {
    inputs: &'a Map<Input, Meta>,
    outputs: &'a Map<Output, Meta>,
    kind: &'a Kind,
}
