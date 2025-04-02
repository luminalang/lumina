use super::*;

// Operators which bind instantly as a parameter
// f a@b c
// should collect as
// f (a@b) c
// and not
// (f a) @ (b c)
pub const PARAM_BINDED: &[&str] = &["@"];

fn tightness(op: &str) -> usize {
    match op {
        "," => 1,
        "|" => 2,
        "." => 3,
        "can" => 4,
        _ => 5,
    }
}

impl<'s> Parser<'s> {
    pub fn unary(&mut self, span: Span) -> Meta<Entity<'s>> {
        let token = self.lexer.peek();

        let init = self
            .try_parameter(token)
            .unwrap_or_else(|| Meta::n(Entity::Missing, Span::null()));

        (0..span.length as u32).rev().fold(init, |acc, i| {
            let span = Span::new(span.indice + i, 1);
            let name = self.take(span).tr(span);
            acc.map(|next| Entity::Unary(name, Box::new(next)))
        })
    }

    pub fn handle_operator_if_not_expected(&mut self, _token: TokenKind, span: Span) -> Entity<'s> {
        for ctx in self.ctx.iter().rev() {
            match ctx {
                Context::Header(_)
                | Context::Indent(_)
                | Context::Clause(_)
                | Context::InMatch(_) => {}

                // We expected something else here, and we expect the operator later
                Context::Operators(_) => return Entity::Missing,
            }
        }

        self.lexer.next();
        let name = self.take(span);
        Entity::Symbol(name)
    }

    pub fn handle_parameter_operator(
        &mut self,
        lhs: Meta<Entity<'s>>,
        operator: Meta<&'s str>,
    ) -> Meta<Entity<'s>> {
        let token = self.lexer.peek();

        let rhs = self
            .try_parameter(token)
            .unwrap_or_else(|| Meta::n(Entity::Missing, operator.span));

        let lhs = lhs.union(operator, |lhs, _| lhs.kind);
        lhs.union(rhs, |lhs, rhs| Entity::Operators {
            lhs: Box::new(lhs),
            operator: operator.kind.tr(operator.span),
            parts: vec![rhs],
        })
    }

    pub fn handle_operator_followup(
        &mut self,
        mut entity: Meta<Entity<'s>>,
        name: Tr<&'s str>,
    ) -> Result<Meta<Entity<'s>>, Meta<Entity<'s>>> {
        if PARAM_BINDED.contains(&*name) {
            let name = Meta::n(name.value, name.span);
            self.lexer.next();
            return Ok(self.handle_parameter_operator(entity, name));
        }

        let src = self.lexer.source();

        let mut ctx_iter = self.ctx.iter_mut().rev();

        while let Some(ctx) = ctx_iter.next() {
            match ctx {
                Context::Operators(op) if *op == name => {
                    return Err(entity);
                }

                Context::Operators(previous_op) => {
                    // We're already getting operators. ignore it unless tighter
                    if tightness(*name) > tightness(**previous_op) {
                        // NOTE: We assume that `lhs` is *not* an `Entity::Operator` corresponding
                        // to `previous_op`.
                        // 1 . 2 + 3
                        //   ^       previous_op
                        //     ^     `entity`
                        //       ^   name
                        //         ^ `rhs`
                        assert_ne!(
                            *name, "|",
                            "TODO: make sure we safely handle `|` in `match` contexts"
                        );
                        trace!("accepting tighter operator {name} when context has {previous_op} for lhs\n{entity}");
                        return Ok(self.eat_operator_followup(entity, name));
                    } else {
                        trace!(
                            "refusing to use {entity} as left-side for {name} due to precedence"
                        );
                        return Err(entity);
                    }
                }
                // Context::Header("->" | "else" | "then" | "in") if *name == "|" => {
                Context::Header("->" | "if" | "do" | "let") if *name == "|" => {
                    trace!("Ignoring {name} past {ctx:?}");
                    return Err(entity);
                }
                Context::Indent(_) | Context::Clause(_) | Context::Header(_) => {
                    trace!("accepting operator {name} under {ctx:?}");
                    return Ok(self.eat_operator_followup(entity, name));
                }
                Context::InMatch(first_bar) => match *name {
                    // Handle indentation sensitivity for match branches
                    "|" => match first_bar {
                        Some(first_span) => match find_match_context(ctx_iter) {
                            // We don't have to be picky about indentation if there's no nesting
                            None => {
                                match &mut entity.kind {
                                    Entity::Operators { parts, operator, .. } => {
                                        assert_eq!(*operator, name);
                                        trace!("Accepting {name}");

                                        let op_token = self.lexer.next();
                                        let rhs = self.next(true);
                                        let rhs = rhs.union(op_token, |rhs, _| rhs.kind); // move comment to operator
                                        parts.push(rhs);
                                        return Ok(entity);
                                    }
                                    // Handle it in the followup loop that set `first_span` instead
                                    _ => return Err(entity),
                                }
                            }
                            // Compare the indentation of `name` to see if it belongs to
                            // `first_span` or `upper_span`
                            Some(upper_first_span) => {
                                let competing = [upper_first_span, *first_span];
                                let icheck =
                                    bar_belongs_to_currents_span(src, name.span, competing);
                                trace!("indentation check: {icheck:?}");
                                match icheck {
                                    IndentCheck::NotAtStartOfLine => todo!(),
                                    IndentCheck::Ambigious(ambigious_indentation) => {
                                        todo!();
                                    }
                                    IndentCheck::DoEat => {
                                        return Ok(self.eat_bar_followup(entity, name));
                                    }

                                    IndentCheck::DoNotEat => {
                                        return Err(entity);
                                    }
                                }
                            }
                        },
                        None => {
                            trace!("initializing bar");
                            *first_bar = Some(name.span);
                            return Ok(self.eat_bar_followup(entity, name));
                        }
                    },
                    _ => {
                        trace!("accepting operator {name} with lhs\n{entity}");
                        return Ok(self.eat_bar_followup(entity, name));
                    }
                },
            }
        }

        trace!("accepting operator {name} with lhs\n{entity}");
        Ok(self.eat_bar_followup(entity, name))
    }

    fn eat_bar_followup(&mut self, lhs: Meta<Entity<'s>>, name: Tr<&'s str>) -> Meta<Entity<'s>> {
        // todo!("BUT: if we never add `Operators` context then they won't be merged?");
        // no, they should be merged in `new_operator`

        // We don't add an `Operators` context because we use `InMatch` instead
        let op_token = self.lexer.next();
        let rhs = self.next(true);
        let rhs = rhs.union(op_token, |rhs, _| rhs.kind); // move comment to operator
        Entity::as_operator(lhs, name, rhs)
    }

    fn eat_operator_followup(
        &mut self,
        lhs: Meta<Entity<'s>>,
        name: Tr<&'s str>,
    ) -> Meta<Entity<'s>> {
        let op_token = self.lexer.next();
        let rhs = self.in_ctx(Context::Operators(name), |this| this.next(true));
        let rhs = rhs.union(op_token, |rhs, _| rhs.kind); // move comment to operator
        Entity::as_operator(lhs, name, rhs)
    }
}

impl<'s> Entity<'s> {
    fn as_operator(lhs: Meta<Entity<'s>>, name: Tr<&'s str>, rhs: Meta<Entity<'s>>) -> Meta<Self> {
        lhs.union(rhs, |lhs, rhs| match lhs.kind {
            Entity::Operators { lhs, operator, mut parts } if *operator == *name => {
                parts.push(rhs);
                Entity::Operators { lhs, operator, parts }
            }
            _ => Entity::Operators { lhs: Box::new(lhs), operator: name, parts: vec![rhs] },
        })
    }
}

fn bar_belongs_to_currents_span(src: &str, span: Span, [upper, current]: [Span; 2]) -> IndentCheck {
    let Some(span_indent) = get_indent_level(src, span, true) else {
        return IndentCheck::NotAtStartOfLine;
    };

    let [u_indent, c_indent] = [upper, current].map(|sp| get_indent_level(src, sp, false).unwrap());

    match span_indent
        .abs_diff(u_indent)
        .cmp(&span_indent.abs_diff(c_indent))
    {
        Ordering::Greater => IndentCheck::DoEat,
        Ordering::Less => IndentCheck::DoNotEat,
        Ordering::Equal => {
            IndentCheck::Ambigious(AmbigiousIndentation { previous: [upper, current], span })
        }
    }
}
