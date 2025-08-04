use crate::key;
use cranelift::codegen::entity::{entity_impl, PrimaryMap};
use lumina_parser as parser;
use lumina_util::Tr;
use rvsdg::{
    id::{AnyNode, Node},
    node_kind_impl, Input, NodeKind, Origin, Output,
};

#[derive(Debug, Clone)]
pub enum Literal {
    Int(bool, u128),
    Float(f64),
    String(Vec<u8>),
    Char(char),
}
node_kind_impl!(Literal, "literal");

#[derive(Debug, Clone)]
pub struct Poison {}
node_kind_impl!(Poison, "poison");

#[derive(Debug, Clone)]
pub struct UnresolvedFieldAccessor(pub String);
node_kind_impl!(UnresolvedFieldAccessor, "unres_field_accessor");

#[derive(Debug, Clone)]
pub struct FieldAccessor(pub usize);
node_kind_impl!(FieldAccessor, "field");

/// The identity node takes a single input and has a single output.
///
/// It directly gives its input as its output.
///
/// The main purpose of this is if you need to associate additional information with a node that
/// does not have a known node origin.
#[derive(Debug, Clone)]
pub struct Identity;
node_kind_impl!(Identity, "identity");

#[derive(Debug, Clone)]
pub struct UnresolvedRecordConstructor {
    inputs: Vec<Tr<String>>,
}
node_kind_impl!(UnresolvedRecordConstructor, "unres_record");

#[derive(Debug, Clone)]
pub struct UnresolvedPatRecord {
    mapping: Vec<Option<String>>,
}
node_kind_impl!(UnresolvedPatRecord, "unres_pat_record");

impl<'s> From<&parser::Literal<'s>> for Literal {
    fn from(lit: &parser::Literal<'s>) -> Self {
        match lit {
            parser::Literal::Int(b, n) => Literal::Int(*b, *n),
            parser::Literal::Float(f) => Literal::Float(*f),
            parser::Literal::String(raw) => Literal::String(escape(raw)),
            parser::Literal::Char(raw) => {
                let bytes = escape(raw);
                let mut chars = std::str::from_utf8(&bytes).unwrap().chars();
                match (chars.next(), chars.next()) {
                    (Some(char), None) => Literal::Char(char),
                    _ => panic!("ET: invalid char literal size"),
                }
            }
        }
    }
}

fn escape(str: &str) -> Vec<u8> {
    let mut buffer = Vec::with_capacity(str.len());
    let mut bytes = str.bytes();

    loop {
        let Some(mut b) = bytes.next() else {
            break buffer;
        };

        match b {
            b'\\' => match bytes.next() {
                Some(b'n') => b = b'\n',
                Some(b'r') => b = b'\r',
                Some(b't') => b = b'\t',
                Some(b'\\') => b = b'\\',
                Some(b'"') => b = b'"',
                Some(b'\'') => b = b'\'',
                Some(b'0') => b = b'\0',
                b => {
                    panic!("ET: invalid escape sequence: {b:?}");
                }
            },
            _ => {}
        }

        buffer.push(b);
    }
}

pub trait LuminaNodes {
    fn add_expr_record_field(
        &mut self,
        node: Node<UnresolvedRecordConstructor>,
        name: Tr<impl Into<String>>,
    ) -> Input<UnresolvedRecordConstructor>;
    fn add_expr_record_node<'s>(&mut self) -> Output<UnresolvedRecordConstructor>;
    fn add_unresolved_field_accessor_node(
        &mut self,
        src: impl Into<Origin>,
        fname: Tr<&str>,
    ) -> Output<UnresolvedFieldAccessor>;
    fn add_identity_node(&mut self, src: impl Into<Origin>) -> Output<Identity>;

    fn add_accessor_node(&mut self, src: impl Into<Origin>, i: usize) -> Output<FieldAccessor>;
    // fn add_pat_record_node<'s>(
    //     &mut self,
    //     src: impl Into<Origin>,
    // ) -> Vec<Output<UnresolvedPatRecord>>;
}

impl LuminaNodes for rvsdg::TranslationUnitContext {
    fn add_expr_record_field(
        &mut self,
        node: Node<UnresolvedRecordConstructor>,
        name: Tr<impl Into<String>>,
    ) -> Input<UnresolvedRecordConstructor> {
        self.get_mut(node).inputs.push(name.map(Into::into));
        self.add_input(node)
    }

    fn add_expr_record_node<'s>(&mut self) -> Output<UnresolvedRecordConstructor> {
        let node = self.add_node(|_, _| {
            let accessor = UnresolvedRecordConstructor { inputs: vec![] };
            (accessor, [])
        });

        self.add_output(node)
    }

    fn add_unresolved_field_accessor_node(
        &mut self,
        src: impl Into<Origin>,
        field_name: Tr<&str>,
    ) -> Output<UnresolvedFieldAccessor> {
        let node = self.add_node(|_, _| {
            let accessor = UnresolvedFieldAccessor((**field_name).to_string());
            (accessor, [])
        });

        let src_input = self.add_input(node);
        let output = self.add_output(node);

        self.connect(src, src_input);

        output
    }

    fn add_identity_node(&mut self, src: impl Into<Origin>) -> Output<Identity> {
        todo!();
    }

    // Destructs multiple fields from an unresolved record.
    //
    // We use this as its own node instead of individual field accessors so that we can attach the
    // type annotation for
    // fn add_unresolved_fields_accessors_node(&mut self, src: impl Into<Origin>)

    fn add_accessor_node(&mut self, src: impl Into<Origin>, i: usize) -> Output<FieldAccessor> {
        let node = self.add_node(|_, _| {
            let accessor = FieldAccessor(i);
            (accessor, [])
        });

        let src_input = self.add_input(node);
        let output = self.add_output(node);

        self.connect(src, src_input);

        output
    }

    // fn add_pat_record_node<'s>(
    //     &mut self,
    //     src: impl Into<Origin>,
    //     // fields: Vec<parser::Field<'s, Output<AnyNode>>>,
    // ) -> Vec<Output<UnresolvedPatRecord>> {
    //     let node = self.add_node(|_, _| (UnresolvedPatRecord { mapping: vec![] }, []));

    //     let src_input = self.add_input(node);
    //     let output = self.add_output(node);

    //     self.connect(src, src_input);

    //     todo!("does one output per field really make sense?");
    //     // I suppose it does since the point is to destruct it
    //     //
    //     // But like, can't we already sort of do that with accessors then?
    //     //
    //     // This node is technically not needed.
    //     //
    //     // Although; It could still be useful for errors and reachability purposes.
    //     //
    //     // But that could also be cleaned up by us having a match node with all the matching being
    //     // done in the region. Unless; perhaps we already did that?
    //     //
    //     // nope. but we probably should. That's for later though

    //     // output
    // }
}
