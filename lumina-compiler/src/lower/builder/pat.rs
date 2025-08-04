use super::*;

impl<'a, 's> FuncNodeLower<'a, 's> {
    pub fn destruct(&mut self, v: rvsdg::Origin, pat: Tr<&parser::Pattern<'s>>) -> () {
        match pat.value {
            parser::Pattern::Name(identifier, params) => {
                if identifier.is_name()
                    && identifier.as_slice()[0]
                        .chars()
                        .next()
                        .unwrap()
                        .is_lowercase()
                {
                    let name = identifier.as_name().unwrap();
                    assert!(
                        params.is_empty(),
                        "TODO: how to solve string pat ambiguiety"
                    );

                    self.scopes.declare(name, v);
                } else {
                    // let params = self.destructs(v, params);
                    todo!("let's do var construction first");
                }
            }
            parser::Pattern::String(_, trs) => todo!(),
            parser::Pattern::Char(_, trs) => todo!(),
            parser::Pattern::Extractor(tr, tr1, trs) => todo!(),
            parser::Pattern::Fields(curly_init, fields) => {
                // TODO: I think we should just expand the field_path right away. easier that way.

                // fields.iter().for_each(|f| match f {
                //     parser::Field::Punned(path) => {
                //         todo!();
                //     }
                //     parser::Field::Value(tr) => unreachable!("tuple syntax is no longer ambigious"),
                //     parser::Field::Assigned { field_path, bind, value } => todo!(),
                // });
                // fields.iter().map(|f| f.map(|pat| self.destruct()))
                // self.destruct_fields(fields);

                // match &**curly_init {
                //     parser::CurlyInit::Construct(tr) => {}
                //     parser::CurlyInit::Modify(tr) => todo!(),
                //     parser::CurlyInit::None => todo!(),
                // }
                todo!();
            }
            parser::Pattern::List(trs, list_length) => todo!(),
            parser::Pattern::Tuple(trs) => todo!(),
            parser::Pattern::Int(_) => todo!(),
            parser::Pattern::Float(_) => todo!(),
            parser::Pattern::Operators { init, ops } => todo!(),
            parser::Pattern::Poison => todo!(),
        }
    }

    fn destruct_record(
        &mut self,
        v: Origin,
        init: &parser::CurlyInit<'s>,
        fields: &[parser::Field<'s, parser::Pattern<'s>>],
    ) {
        let fields = record::from_parsed(fields);

        let v = match init {
            parser::CurlyInit::Construct(type_) => {
                let v = match v {
                    Origin::Output(any_node, output) => {
                        Output { node: rvsdg::id::Node::from(any_node), id: output }
                    }
                    // HACK: Create an identity node so we have something to attach the type ascription to
                    Origin::Argument(..) => self.ctx.add_identity_node(v).downcast(),
                };
                self.type_ascriptions[v.node.id].push((**type_).clone());
                v.into()
            }
            parser::CurlyInit::Modify(tr) => panic!("ET: modifying node is not allowed in pattern"),
            parser::CurlyInit::None => v,
        };

        self.destruct_fields(v, fields);
    }

    fn destruct_fields<'e>(
        &mut self,
        record: Origin,
        fields: Vec<record::Field<'s, Tr<&'e parser::Pattern<'s>>>>,
    ) {
        for field in fields {
            let field_value = self
                .ctx
                .add_unresolved_field_accessor_node(record, field.name);

            match field.value {
                record::FieldValue::Record(fields) => {
                    self.destruct_fields(field_value.into(), fields);
                }

                record::FieldValue::Tail(pattern) => self.destruct(field_value.into(), pattern),

                // Bind the identifier to the destructed field
                record::FieldValue::Punned => self.scopes.declare(*field.name, field_value.into()),
            }
        }

        todo!();
    }
}
