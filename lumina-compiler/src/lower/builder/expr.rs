use super::*;

impl<'a, 's> FuncNodeLower<'a, 's> {
    pub fn expr(&mut self, expr: &parser::Expr<'s>) -> Output<AnyNode> {
        // TODO: and then we could also collect like a `id::AnyNode -> Type` mapping somewhere
        // which is later used during type checking?

        match expr {
            parser::Expr::Lit(lit) => {
                let lit = nodes::Literal::from(lit);

                let node = self.ctx.add_node(|_, _| (lit, []));
                self.ctx.add_output(node).downcast()
            }
            parser::Expr::Call(apath, params) => {
                if let [name] = apath.path.as_slice() {
                    if let Some(called) = self.resolve(name) {
                        return self.apply(called, params).downcast();
                    }
                }

                let arg = self.add_func_ext(apath.path.clone());
                self.apply(arg, params).downcast()
            }
            parser::Expr::Group(expr) => self.expr(&*expr),
            parser::Expr::DotPipe(_) => todo!(),
            parser::Expr::FieldAccess(src, field_name) => {
                let src = self.expr(&src);
                self.ctx
                    .add_unresolved_field_accessor_node(src, *field_name)
                    .downcast()
            }
            parser::Expr::TupleAccess(src, i) => {
                let src = self.expr(&src);
                self.ctx.add_accessor_node(src, **i).downcast()
            }
            parser::Expr::Lambda(patterns, params, expr) => {
                let lambda = self.lambda(patterns, (**expr).as_ref());
                todo!("pretty sure we need to add the capture record to the params");
                self.apply(lambda, params).downcast()
            }
            parser::Expr::CallExpr(f, params) => {
                let f = self.expr(&f);
                self.apply(f, params).downcast()
            }
            parser::Expr::Operators { init, ops } => todo!(),
            parser::Expr::Match(tr, items) => {
                todo!();
            }
            parser::Expr::CastAs(tr, tr1) => todo!(),
            parser::Expr::List(trs, list_length) => todo!(),
            parser::Expr::Tuple(trs) => todo!(),
            parser::Expr::Record { init, fields } => self.record(init, fields),
            parser::Expr::If(_) => todo!(),
            parser::Expr::Do(_) => todo!(),
            parser::Expr::Let(tr, _) => todo!(),
            parser::Expr::Pass(tr) => todo!(),
            parser::Expr::PassFptr(annotated_path) => todo!(),
            parser::Expr::Poison => todo!(),
        }
    }

    fn record(
        &mut self,
        init: &parser::CurlyInit<'s>,
        fields: &[parser::Field<'s, parser::Expr<'s>>],
    ) -> Output<AnyNode> {
        // TODO: Do we lower types? Probably right.
        //
        // So; we should do that first so we know how it'll work.

        let fields = record::from_parsed(fields);

        let record = self.ctx.add_expr_record_node();

        match init {
            parser::CurlyInit::Construct(type_) => {
                self.type_ascriptions[record.node.id].push((**type_).clone());
            }
            parser::CurlyInit::Modify(tr) => todo!(),
            parser::CurlyInit::None => {}
        }

        self.fields(record, fields);

        record.downcast()
    }

    fn fields<'e>(
        &mut self,
        of: Output<nodes::UnresolvedRecordConstructor>,
        fields: Vec<record::Field<'s, Tr<&'e parser::Expr<'s>>>>,
    ) {
        for field in fields {
            let input = self.ctx.add_expr_record_field(of.node, field.name);
            let output = self.field(field);
            self.ctx.connect(output, input)
        }
    }

    fn field<'e>(&mut self, field: record::Field<'s, Tr<&'e parser::Expr<'s>>>) -> Origin {
        match field.value {
            record::FieldValue::Record(fields) => {
                let record = self.ctx.add_expr_record_node();
                self.fields(record, fields);
                record.downcast().into()
            }
            record::FieldValue::Tail(expr) => self.expr(&expr).into(),
            record::FieldValue::Punned => match self.resolve(*field.name) {
                Some(origin) => origin,
                None => panic!("ET: not found"),
            },
        }
    }
}
