use super::*;
use std::fmt;
use xmlwriter::{Options, XmlWriter};

pub struct XmlCtx<'ctx> {
    stack: Vec<StackEntry<'ctx>>,
    xml: XmlWriter,
}

enum StackEntry<'ctx> {
    Node(NodeId, &'ctx str),
    Region(&'ctx str),
}

impl<'ctx> XmlCtx<'ctx> {
    pub fn write_node(&mut self, id: NodeId, node: &'ctx Node) {
        self.xml.start_element("node");
        self.stack.push(StackEntry::Node(id, &node.name));

        self.xml.write_attribute("id", &id);
        self.xml.write_attribute("name", &node.name);
        self.xml.write_attribute("type", node.kind.node_type());

        for (i, meta) in &node.inputs {
            self.xml.start_element("input");
            self.xml.write_attribute("id", &self.prefixed(i));
            // self.xml.write_attribute("id", &format_args!("{id}.{i}"));
            if meta.name != "" {
                self.xml.write_attribute("name", meta.name);
            }
            self.xml.end_element();
        }

        for (o, meta) in &node.outputs {
            self.xml.start_element("output");
            self.xml.write_attribute("id", &self.prefixed(o));
            // self.xml.write_attribute("id", &format_args!("{id}.{o}"));
            if meta.name != "" {
                self.xml.write_attribute("name", meta.name);
            }
            self.xml.end_element();
        }

        node.kind.to_xml(self);

        self.stack.pop();
        self.xml.end_element();
    }

    pub fn write_region(&mut self, id: &'ctx str, region: &'ctx Region) {
        self.xml.start_element("region");
        self.xml.write_attribute("id", &id);
        self.stack.push(StackEntry::Region(id));

        for (a, m) in &region.arguments {
            self.xml.start_element("argument");
            self.xml.write_attribute("id", &self.prefixed(a));
            self.xml.write_attribute("name", &m.name);
            self.xml.end_element();
        }

        for (r, m) in &region.results {
            self.xml.start_element("result");
            self.xml.write_attribute("id", &self.prefixed(r));
            self.xml.write_attribute("name", &m.name);
            self.xml.end_element();
        }

        for (node_id, node) in &region.nodes {
            self.write_node(node_id, node);
        }

        for edge in &region.edges {
            self.xml.start_element("edge");
            self.xml.write_attribute(
                "source",
                &match edge.origin {
                    Origin::Output(node, output) => self.prefixed(format!("{node}.{output}")),
                    Origin::Argument(argument) => self.prefixed(argument),
                },
            );
            self.xml.write_attribute(
                "target",
                &match edge.user {
                    User::Input(node, input) => self.prefixed(format!("{node}.{input}")),
                    User::Result(result) => self.prefixed(result),
                },
            );
            self.xml.end_element();
        }

        self.stack.pop();
        self.xml.end_element();
    }

    fn prefixed(&self, v: impl fmt::Display) -> String {
        let mut buf = String::new();
        for entry in &self.stack {
            match entry {
                StackEntry::Node(node_id, _) => buf.push_str(&node_id.to_string()),
                StackEntry::Region(name) => buf.push_str(&format!("region[{name}]")),
            }
            buf.push('.');
        }
        // if buf.ends_with('.') {
        //     buf.pop();
        // }
        buf.push_str(&v.to_string());
        buf
    }
}

/// Output XML compatible with rvsdg-viewer
pub fn to_xml(root: &Node) -> String {
    let opt = Options::default();
    let mut xml = XmlWriter::new(opt);

    xml.start_element("rvsdg");

    let mut ctx = XmlCtx { xml, stack: vec![] };
    ctx.write_node(NodeId(0), root);

    ctx.xml.end_element();

    ctx.xml.end_document()
}
