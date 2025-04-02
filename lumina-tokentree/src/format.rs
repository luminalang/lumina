use super::*;
use std::fmt::Write;
use tracing::{info, warn};

pub struct Formatter<'s> {
    src: &'s str,
    previous: Item,
}

// Used to set appropriate newlines between items
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Item {
    Use,
    Other,
    // Comment,
    Attribute,
    ModAttribute,
    Alias,
    When,
    Impl,
    None,
}

impl<'s> Formatter<'s> {
    pub fn new(src: &'s str) -> Self {
        Self { src, previous: Item::None }
    }

    pub fn toplevel(&mut self, buf: &mut String, entity: Meta<&Entity<'s>>) {
        dbg!(&entity.comment);
        println!("{}", entity.comment.get_str(self.src));
        info!("tree form: \n{entity}");
        self.item_spacing(buf, entity).unwrap();

        let snippet = self.entity(entity);

        buf.push_str(&snippet.buf);
    }

    fn item_spacing(&mut self, buf: &mut String, entity: Meta<&Entity<'s>>) -> std::fmt::Result {
        let mut irules = |item: Item, single: &[Item]| {
            let previous = std::mem::replace(&mut self.previous, item);
            match previous {
                Item::None => Ok(()),
                item if single.contains(&item) => write!(buf, "\n"),
                _ => write!(buf, "\n\n"),
            }
        };

        match &entity.kind {
            Entity::Header(Tr { value: "pub", .. }, item) => {
                self.item_spacing(buf, (**item).as_ref())
            }
            Entity::Header(Tr { value: "when", .. }, _) => {
                irules(Item::When, &[Item::When, Item::Attribute])
            }
            Entity::Header(Tr { value: "impl", .. }, _) => {
                irules(Item::Impl, &[Item::When, Item::Attribute])
            }
            Entity::Header(Tr { value: "use", .. }, _) => {
                irules(Item::Use, &[Item::When, Item::Attribute, Item::Use])
            }
            Entity::Headers(elems) if *elems[0].0 == "alias" => {
                irules(Item::Alias, &[Item::Alias, Item::Attribute])
            }
            Entity::Unary(Tr { value: "@", .. }, next)
                if matches!(**next, Entity::Unary(Tr { value: "!", .. }, _)) =>
            {
                irules(Item::ModAttribute, &[Item::ModAttribute])
            }
            Entity::Unary(Tr { value: "@", .. }, _) => {
                irules(Item::Attribute, &[Item::When, Item::Attribute])
            }
            _ => irules(Item::Other, &[Item::When, Item::Attribute]),
        }
    }

    fn entity(&mut self, entity: Meta<&Entity<'s>>) -> Snippet {
        let snippet = match entity.kind {
            Entity::SingleQuote(text) => self.str_literal('\'', text),
            Entity::DoubleQuote(text) => self.str_literal('"', text),
            Entity::DotAccess { lhs, field } => self.dot_access((**lhs).as_ref(), *field),
            Entity::Unary(op, then) => self.unary(*op, then),
            Entity::Header(header, then) => self.header(*header, (**then).as_ref()),
            Entity::Headers(headers) => self.headers(headers),
            Entity::Clause(open, close, inner) => self.clause(open, close, (**inner).as_ref()),
            Entity::Operators { lhs, operator, parts } => {
                self.operators((**lhs).as_ref(), *operator, parts, false)
            }
            Entity::Symbol(symbol) => Snippet::singleline(symbol.to_string()),
            Entity::IndentBlock(header, members) => self.block(*header, members),
            Entity::Sequence(elems) => self.seq(elems),
            Entity::Int(n) => Snippet::singleline(n.to_string()),
            Entity::Float(n) => Snippet::singleline(n.to_string()),
            Entity::Identifier(identifier, anot) => self.identifier(identifier, anot),
            // Entity::Comment(text) => Snippet::multiline(format!("//{text}")),
            // Entity::Commented(text, then) => {
            //     Snippet::multiline(format!("//{text}\n{}", self.entity((**then).as_ref())))
            // }
            Entity::Missing => Snippet::singleline("".into()),
            Entity::EOF => Snippet::singleline(String::new()),
        };

        match entity.get_comment(self.src) {
            Some(text) => {
                let buf = format!("{text}\n{snippet}");
                Snippet::multiline(buf)
            }
            None => snippet,
        }
    }

    fn when(&mut self, span: Span, then: Meta<&Entity<'s>>) -> Snippet {
        match then.kind {
            Entity::Operators { lhs, operator, parts } if **operator == "can" => {
                self.when_binds((**lhs).as_ref(), parts)
            }
            _ => self.generic_header("when".tr(span), then),
        }
    }

    fn when_binds(&mut self, lhs: Meta<&Entity<'s>>, parts: &[Meta<Entity<'s>>]) -> Snippet {
        match parts {
            [single] => Snippet::singleline(format!(
                "when {} can {}",
                self.entity(lhs),
                self.entity(single.as_ref())
            )),
            _ => {
                let mut buf = format!("when");
                Entity::for_double_sided_operator(lhs, parts, |lhs, rhs| {
                    let lhs = self.entity(lhs);
                    let mut multiline = false;
                    let rhs = self
                        .entities(rhs)
                        .into_iter()
                        .map(|snippet| {
                            multiline |= snippet.multiline;
                            snippet.buf
                        })
                        .format(" ")
                        .to_string();

                    if multiline {
                        write!(buf, "\n  {lhs} can\n    {}", rhs.indent().indent())
                    } else {
                        write!(buf, "\n  {lhs} can {rhs}")
                    }
                    .unwrap()
                });
                Snippet::multiline(buf)
            }
        }
    }

    fn dot_access(&mut self, lhs: Meta<&Entity<'s>>, field: Tr<&'s str>) -> Snippet {
        let mut lhs = self.entity(lhs);
        lhs.buf.push('.');
        lhs.buf.push_str(*field);
        lhs
    }

    fn entities(&mut self, vs: &[Meta<Entity<'s>>]) -> Vec<Snippet> {
        vs.iter().map(|v| self.entity(v.as_ref())).collect()
    }

    fn unary(&mut self, op: Tr<&'s str>, rhs: &Entity<'s>) -> Snippet {
        let rhs = self.entity(Meta::n(&rhs, op.span.move_indice(1)));
        Snippet { multiline: rhs.multiline, buf: format!("{op}{rhs}") }
    }

    fn r#type(&mut self, header: Tr<&str>, then: Meta<&Entity<'s>>) -> Snippet {
        match then.kind {
            Entity::Sequence(elems) => {
                let (body, ident) = elems.split_last().unwrap();
                let ident = self.entities(ident);
                let ident_multiline = ident.iter().any(|snippet| snippet.multiline);
                let ident = ident.into_iter().format(" ");

                let body = match &body.kind {
                    Entity::Clause("{", "}", fields) => self.record_fields((**fields).as_ref()),
                    Entity::Header(Tr { value: "=", .. }, rhs) => {
                        match &rhs.kind {
                            // Strip unecesarry `=` on struct type definition
                            Entity::Clause("{", "}", fields) => {
                                self.record_fields((**fields).as_ref())
                            }
                            _ => {
                                let variants = self.entity((**rhs).as_ref());
                                if variants.multiline {
                                    Snippet::multiline(format!("\n  = {}", variants.indent()))
                                } else {
                                    Snippet::singleline(format!("= {variants}"))
                                }
                            }
                        }
                    }
                    _ => self.entity(body.as_ref()),
                };

                Snippet {
                    multiline: ident_multiline | body.multiline,
                    buf: format!("{header} {ident} {body}"),
                }
            }
            _ => {
                let then = self.entity(then);
                Snippet { multiline: then.multiline, buf: format!("{header} {then}") }
            }
        }
    }

    fn record_fields(&mut self, fields: Meta<&Entity<'s>>) -> Snippet {
        let fields = match fields.kind {
            Entity::Sequence(elems) => self.line_separated_fields(elems),
            Entity::Operators { lhs, operator, parts } => {
                self.operators((**lhs).as_ref(), *operator, parts, true)
            }
            _ => self.entity(fields),
        };

        if fields.multiline {
            Snippet::multiline(format!("{{\n  {}\n}}", fields.indent()))
        } else if fields.buf.is_empty() {
            Snippet::singleline("{}".into())
        } else {
            Snippet::singleline(format!("{{ {fields} }}"))
        }
    }

    fn line_separated_fields(&mut self, elems: &[Meta<Entity<'s>>]) -> Snippet {
        let mut iter = elems.iter().peekable();
        let mut buf = String::new();
        let mut multiline = false;

        while let Some(v) = iter.next() {
            if follows_a_blank_line(self.src, v.span) {
                buf.push('\n');
            }

            let name = self.entity(v.as_ref());
            buf.push_str(&name.buf);

            while let Some(ty) = iter.peek() {
                if self.different_lines(&[v.span, ty.span]) {
                    multiline = true;
                    buf.push_str("\n");
                    break;
                }
                let ty = self.entity(ty.as_ref());
                buf.push_str(" ");
                buf.push_str(&ty.buf);
                iter.next();
            }
        }

        Snippet { multiline, buf: buf.trim_matches('\n').to_string() }
    }

    fn header(&mut self, header: Tr<&str>, then: Meta<&Entity<'s>>) -> Snippet {
        match header.value {
            "type" => self.r#type(header, then),
            "match" => self.r#match(header, then),
            "when" => self.when(header.span, then),
            _ => self.generic_header(header, then),
        }
    }

    fn generic_header(&mut self, header: Tr<&str>, thenv: Meta<&Entity<'s>>) -> Snippet {
        let then = self.entity(thenv);

        let mut multiline = match *header {
            "val" | "trait" | "pub" | "as" | "use" => false,
            _ => {
                then.multiline || self.different_lines(&[header.span, thenv.span])
                // || then.buf.len() > HEADER_LINEBREAK_LIMIT
            }
        };

        let buf = if multiline {
            multiline = true;
            format!("{header}\n  {}", then.indent())
        } else {
            format!("{header} {then}")
        };

        Snippet { multiline, buf }
    }

    fn headers(&mut self, headers: &[(Tr<&'s str>, Meta<Entity<'s>>)]) -> Snippet {
        let is_fn = |v| ["fn", "fnptr"].contains(&v);

        match &headers[..] {
            [x, y, z, w] if is_fn(*x.0) => {
                self.function(*x.0, &x.1, Some(&y.1), Some(&z.1), Some(&w.1), " -> ")
            }
            [x, y, z] if is_fn(*x.0) && *y.0 == "as" && *z.0 == "=" => {
                self.function(*x.0, &x.1, None, Some(&y.1), Some(&z.1), " as ")
            }
            [x, y, z] if is_fn(*x.0) && *y.0 == "as" && *z.0 == "->" => {
                self.function(*x.0, &x.1, Some(&y.1), Some(&z.1), None, " -> ")
            }
            [x, y, z] if is_fn(*x.0) => {
                self.function(*x.0, &x.1, None, Some(&y.1), Some(&z.1), " -> ")
            }
            [x, y] if is_fn(*x.0) && *y.0 == "->" => {
                self.function(*x.0, &x.1, None, Some(&y.1), None, " -> ")
            }
            [x, y] if is_fn(*x.0) && *y.0 == "=" => {
                self.function(*x.0, &x.1, None, None, Some(&y.1), " -> ")
            }
            [x, y] if is_fn(*x.0) && *y.0 == "as" => {
                self.function(*x.0, &x.1, None, Some(&y.1), None, " as ")
            }
            _ => {
                let snippets = self.parts_from_headers(headers);

                match &snippets[..] {
                    [_] => self.header(headers[0].0, headers[0].1.as_ref()),
                    [x, y] if *x.header == "\\" && *y.header == "->" => self.lambda(x, y),
                    [x, y] => self.headers_two([x, y], NextInChain::get(&headers[1].1)),
                    [x, y, z] => self.headers_three([x, y, z], NextInChain::get(&headers[2].1)),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn parts_from_headers(
        &mut self,
        headers: &[(Tr<&'s str>, Meta<Entity<'s>>)],
    ) -> Vec<HeaderPart<'s>> {
        headers
            .iter()
            .map(|(header, then)| HeaderPart { header: *header, then: self.entity(then.as_ref()) })
            .collect()
    }

    fn lambda(&mut self, x: &HeaderPart<'s>, y: &HeaderPart<'s>) -> Snippet {
        let buf = if y.then.multiline {
            format!("{}{} {}\n  {}", x.header, x.then, y.header, y.then.indent())
        } else {
            format!("{}{} {} {}", x.header, x.then, y.header, y.then)
        };

        Snippet { multiline: y.then.multiline || x.then.multiline, buf }
    }

    fn function(
        &mut self,
        header: &str,
        names: &Meta<Entity<'s>>,
        types: Option<&Meta<Entity<'s>>>,
        ret: Option<&Meta<Entity<'s>>>,
        expr: Option<&Meta<Entity<'s>>>,
        shorthand_return_kw: &str,
    ) -> Snippet {
        let mut last_span = names.span;

        let mut multiline;
        let mut buf = format!("{header}");

        match &names.kind {
            Entity::Sequence(elems) => multiline = self.function_names(&mut buf, elems),
            Entity::Operators { lhs, operator: Tr { value: ",", .. }, parts } => {
                warn!("removing outdated function parameter type syntax");
                let mut elems = vec![(**lhs).clone()];
                elems.extend(parts.iter().cloned());
                multiline = self.function_names(&mut buf, &elems);
            }
            _ => {
                buf.push(' ');
                let names = self.entity(names.as_ref());
                multiline = names.multiline;
                buf.push_str(&names.buf);
            }
        };

        if let Some(ret) = ret {
            last_span = ret.span;

            if let Some(types) = types {
                buf.push_str(" as ");
                let types = match &types.kind {
                    Entity::Operators { lhs, operator: Tr { value: ",", .. }, parts } => {
                        let mut elems = vec![(**lhs).clone()];
                        elems.extend(parts.iter().cloned());
                        let seq = Meta::new(Entity::Sequence(elems), types.span, types.comment);
                        self.entity(seq.as_ref())
                    }
                    _ => self.entity(types.as_ref()),
                };
                multiline |= types.multiline;
                buf.push_str(&types.buf);
                buf.push_str(" -> ");
            } else {
                buf.push_str(shorthand_return_kw);
            }

            let ret = self.entity(ret.as_ref());
            multiline |= ret.multiline;
            buf.push_str(&ret.buf);
        } else {
            assert!(types.is_none());
        }

        if let Some(expr) = expr {
            multiline |= self.different_lines(&[last_span, expr.span]);
            let expr = self.entity(expr.as_ref());
            multiline |= expr.multiline;

            if multiline {
                buf.push_str(&format!(" =\n  {}", expr.buf.indent()));
            } else {
                buf.push_str(&format!(" = {}", expr.buf))
            }
        }

        Snippet { multiline, buf }
    }

    fn function_names(&mut self, buf: &mut String, elems: &[Meta<Entity<'s>>]) -> bool {
        buf.push(' ');
        let elems = self.entities(elems);
        let multiline = elems.iter().any(|elem| elem.multiline);

        if multiline {
            let (name, patterns) = elems.split_first().unwrap();
            write!(
                buf,
                "{name}\n  {}",
                patterns.into_iter().format("\n").indent()
            )
        } else {
            write!(buf, "{}", elems.into_iter().format(" "))
        }
        .unwrap();

        multiline
    }

    fn headers_two(
        &mut self,
        [x, y]: [&HeaderPart<'s>; 2],
        chain: Option<NextInChain<'_, 's>>,
    ) -> Snippet {
        let buf = match chain {
            Some(next) => {
                let same_kind = *next.headers[0].0 == *x.header;
                if same_kind {
                    format!("{} {} {}\n{}", x.header, x.then, y.header, y.then)
                } else {
                    format!(
                        "{} {} {}\n  {}",
                        x.header,
                        x.then,
                        y.header,
                        y.then.indent()
                    )
                }
            }
            None => match *x.header {
                "let" | "do" if !y.then.multiline => {
                    format!("{} {}\n {} {}", x.header, x.then, y.header, y.then)
                }
                _ => {
                    self.long_headers_two([x, y]).buf
                    // let ythen = y.then.indent();
                    // format!("{} {} {}\n  {}", x.header, x.then, y.header, ythen,)
                }
            },
        };

        Snippet::multiline(buf)
    }

    fn headers_three(
        &mut self,
        [x, y, z]: [&HeaderPart<'s>; 3],
        chain: Option<NextInChain<'_, 's>>,
    ) -> Snippet {
        let buf = match chain {
            Some(next) => {
                if next.is_tail {
                    // Weird edge-case to make the last part of the chain not use its short variant
                    // if it's a part of a chain.
                    let z_parts = self.parts_from_headers(next.headers);
                    let new_z = self.expanded_headers(&z_parts);
                    let ythen = y.then.indent();
                    format!(
                        "{} {} {}\n  {}\n{} {}",
                        x.header, x.then, y.header, ythen, z.header, new_z,
                    )
                } else {
                    let ythen = y.then.indent();
                    format!(
                        "{} {} {}\n  {}\n{} {}",
                        x.header, x.then, y.header, ythen, z.header, z.then
                    )
                }
            }
            None => {
                if y.then.multiline || z.then.multiline {
                    self.long_headers_three([x, y, z]).buf
                } else {
                    format!(
                        "{} {}\n  {} {}\n  {} {}",
                        x.header, x.then, y.header, y.then, z.header, z.then
                    )
                }
            }
        };

        Snippet::multiline(buf)
    }

    fn expanded_headers(&mut self, elems: &[HeaderPart<'s>]) -> Snippet {
        match elems {
            [x, y] => self.long_headers_two([x, y]),
            [x, y, z] => self.long_headers_three([x, y, z]),
            _ => panic!("strange length of header chain: {elems:?}"),
        }
    }

    fn long_headers_two(&mut self, [x, y]: [&HeaderPart<'s>; 2]) -> Snippet {
        let ythen = y.then.indent();
        Snippet::multiline(format!("{} {} {}\n  {}", x.header, x.then, y.header, ythen))
    }

    fn long_headers_three(&mut self, [x, y, z]: [&HeaderPart<'s>; 3]) -> Snippet {
        let ythen = y.then.indent();
        let zthen = z.then.indent();
        Snippet::multiline(format!(
            "{} {} {}\n  {}\n{}\n  {}",
            x.header, x.then, y.header, ythen, z.header, zthen,
        ))
    }

    fn r#match(&mut self, header: Tr<&str>, then: Meta<&Entity<'s>>) -> Snippet {
        match then.kind {
            Entity::Operators { lhs, operator, parts } => {
                let lhs = self.entity((**lhs).as_ref());
                let mut buf = if lhs.multiline {
                    format!("{header}\n  {}", lhs.indent())
                } else {
                    format!("{header} {lhs}")
                };

                for branch in parts {
                    let branch_start_span = branch.span.start_of_line(self.src.as_bytes());
                    let spacing = select_spacing(self.src, branch_start_span, "\n\n", "\n");
                    if branch.comment != Span::null() {
                        write!(buf, "\n{}", branch.comment.get_str(self.src)).unwrap();
                    }
                    let branch = self.entity(branch.without_comment());
                    write!(buf, "{spacing}{operator} {}", branch).unwrap();
                }

                Snippet::multiline(buf)
            }
            _ => self.generic_header(header, then),
        }
    }

    fn block(&mut self, header: Tr<&str>, members: &[Meta<Entity<'s>>]) -> Snippet {
        if members.is_empty() {
            return Snippet::singleline(header.to_string());
        }

        match header.value {
            "impl" => self.block_with_item_members("impl", &members),
            "trait" => self.block_with_item_members("trait", &members),
            _ => {
                let members = self.indented_keep_spacing(members, true, "\n\n  ", "\n  ");
                let buf = format!("{header}\n  {}", members.into_iter().format("\n"));
                Snippet::multiline(buf)
            }
        }
    }

    fn indented_keep_spacing(
        &mut self,
        members: &[Meta<Entity<'s>>],
        skip_first: bool,
        large_spacing: &str,
        small_spacing: &str,
    ) -> Vec<Snippet> {
        members
            .iter()
            .enumerate()
            .map(|(i, entity)| {
                let spacing = if skip_first && i == 0 {
                    ""
                } else {
                    select_spacing(self.src, entity.span, large_spacing, small_spacing)
                };
                let snippet = self.entity(entity.as_ref());
                Snippet {
                    multiline: snippet.multiline,
                    buf: format!("{spacing}{}", snippet.indent()),
                }
            })
            .collect()
    }

    fn block_with_item_members(&mut self, kw: &str, members: &[Meta<Entity<'s>>]) -> Snippet {
        let (name, members) = members.split_first().unwrap();
        let name = self.entity(name.as_ref());
        let members = self.indented_keep_spacing(members, true, "\n\n  ", "\n  ");
        if members.is_empty() {
            let buf = format!("{kw} {name}");
            Snippet::singleline(buf)
        } else {
            let buf = format!("{kw} {name}\n  {}", members.into_iter().format(""));
            Snippet::multiline(buf)
        }
    }

    fn clause(&mut self, open: &str, close: &str, inner: Meta<&Entity<'s>>) -> Snippet {
        let inner = self.entity(inner);
        if inner.multiline {
            let buf = format!("{open}\n  {}\n{close}", inner.indent());
            Snippet::multiline(buf)
        } else if open == "{" {
            if inner.buf.is_empty() {
                Snippet::singleline("{}".into())
            } else {
                let buf = format!("{open} {} {close}", inner.buf);
                Snippet::singleline(buf)
            }
        } else {
            let buf = format!("{open}{}{close}", inner.buf);
            Snippet::singleline(buf)
        }
    }

    fn operators(
        &mut self,
        lhs: Meta<&Entity<'s>>,
        operator: Tr<&'s str>,
        parts: &[Meta<Entity<'s>>],
        comma_at_line_end: bool,
    ) -> Snippet {
        const POSTFIX: &[&str] = &[",", ";"];
        const PARAMETER: &[&str] = &["@"];

        let Snippet { mut buf, multiline } = self.entity(lhs);

        let sides = parts
            .iter()
            .map(|v| (self.entity(v.without_comment()), v.comment))
            .collect::<Vec<_>>();

        let any_multiline = sides.iter().any(|(side, _)| side.multiline);
        let total_size = sides.iter().map(|(side, _)| side.buf.len()).sum::<usize>();
        let requested = self.different_lines(&[lhs.span, parts.last().unwrap().span]);
        let multiline = multiline || any_multiline || (requested && total_size > 8);

        for (rhs, comment) in sides.iter() {
            if *comment != Span::null() {
                write!(buf, "\n{}", comment.get_str(self.src)).unwrap();
            }

            if rhs.buf.is_empty() {
                write!(buf, "{operator}")
            } else if multiline {
                if comma_at_line_end && *operator == "," {
                    write!(buf, "{operator}\n")
                } else {
                    write!(buf, "\n{operator} ")
                }
            } else if buf.is_empty() || POSTFIX.contains(&operator) {
                write!(buf, "{operator} ")
            } else if PARAMETER.contains(&operator) {
                write!(buf, "{operator}")
            } else {
                write!(buf, " {operator} ")
            }
            .unwrap();

            write!(buf, "{rhs}").unwrap();
        }

        // Edge-case for removing trailing commas on single-line lists
        if !multiline && *operator == "," {
            while let Some(b' ' | b',' | b'\n') = buf.as_bytes().last() {
                buf.pop();
            }
        }

        Snippet { multiline, buf }
    }

    fn different_lines(&self, spans: &[Span]) -> bool {
        let tspan = Span::from_elems(spans, |&span| span);
        self.span_contains('\n', tspan)
    }

    fn identifier(&mut self, ident: &Identifier<'s>, anot: &Annotation<'s>) -> Snippet {
        let mut segments = ident.as_slice().into_iter().enumerate();
        let (i, x) = segments.next().unwrap();

        let mut buf = x.to_string();
        let mut multiline = false;

        let mut add_annotation = |buf: &mut String, i: usize| {
            if let Some(assign) = anot.get(&i) {
                let assign = self.entity(assign.as_ref());
                multiline |= assign.multiline;

                if assign.multiline {
                    write!(buf, "(\n  {}\n)", assign.indent()).unwrap();
                } else {
                    write!(buf, "({assign})").unwrap();
                }
            }
        };

        add_annotation(&mut buf, i);

        for (i, segment) in segments {
            add_annotation(&mut buf, i);
            write!(buf, ":{segment}").unwrap();
        }

        Snippet { multiline, buf }
    }

    fn str_literal(&self, clause: char, text: &str) -> Snippet {
        Snippet {
            multiline: text.contains("\n"),
            buf: format!("{clause}{text}{clause}"),
        }
    }

    fn seq(&mut self, elems: &[Meta<Entity<'s>>]) -> Snippet {
        if elems.len() == 1 {
            return self.entity(elems[0].as_ref());
        }

        let snippets = self.entities(elems);

        match elems.split_last().unwrap() {
            (
                Meta {
                    kind: Entity::IndentBlock(Tr { value: "where", span }, members),
                    ..
                },
                earlier,
            ) => {
                let mut snippet = self.seq(earlier);
                if follows_a_blank_line(self.src, *span) {
                    snippet.buf.push_str("\n\n");
                } else {
                    snippet.buf.push_str("\n");
                };

                let members = self.block("where".tr(*span), members);
                write!(snippet.buf, " {}", members).unwrap();
                snippet.multiline |= members.multiline;

                snippet
            }
            // If we end with a header, we don't want its newline to affect the sequence newline
            (Meta { kind: Entity::Header(header, then), .. }, earlier) => {
                let seq = self.seq(earlier);
                let then = self.generic_header(*header, (**then).as_ref());

                let buf = if seq.multiline {
                    format!("{seq}\n  {}", then.indent())
                } else {
                    format!("{seq} {then}")
                };

                Snippet { multiline: seq.multiline | then.multiline, buf }
            }
            _ => {
                let tspan = Span::from_elems(elems, |v| v.span);
                let requested = self.span_contains('\n', tspan);
                let multiline = requested || snippets.iter().any(|snippet| snippet.multiline);

                let buf = if multiline {
                    let (x, xs) = snippets.split_first().unwrap();
                    format!("{}\n  {}", x.buf, indented(xs))
                } else {
                    sep_by(" ", &snippets).to_string()
                };

                Snippet { multiline, buf }
            }
        }
    }

    fn span_contains(&self, c: char, span: Span) -> bool {
        span.get_str(self.src).contains(c)
    }
}

fn select_spacing<'a>(src: &str, span: Span, large: &'a str, small: &'a str) -> &'a str {
    if follows_a_blank_line(src, span) {
        large
    } else {
        small
    }
}

fn follows_a_blank_line(src: &str, mut span: Span) -> bool {
    let mut newlines = 0;
    loop {
        if span.indice == 0 {
            return true;
        }
        span.indice -= 1;
        match src.as_bytes().get(span.indice as usize).copied() {
            Some(b'\n') => newlines += 1,
            Some(b' ' | b'\t') => {}
            _ => return newlines > 1,
        }
    }
}

fn sep_by<'a>(sep: &'a str, elems: &'a [Snippet]) -> impl fmt::Display + 'a {
    elems.iter().map(|v| &v.buf).format(sep)
}

fn indented<'a>(elems: &'a [Snippet]) -> impl fmt::Display + 'a {
    elems.iter().map(|v| v.buf.indent()).format("\n  ")
}

#[derive(Debug)]
pub struct Snippet {
    pub multiline: bool,
    pub buf: String,
}

#[derive(Debug)]
struct HeaderPart<'s> {
    header: Tr<&'s str>,
    then: Snippet,
}

impl Snippet {
    fn multiline(buf: String) -> Self {
        Self { buf, multiline: true }
    }

    fn singleline(buf: String) -> Self {
        Self { buf, multiline: false }
    }
}

impl fmt::Display for Snippet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.buf.fmt(f)
    }
}

struct NextInChain<'a, 's> {
    headers: &'a [(Tr<&'s str>, Meta<Entity<'s>>)],
    is_tail: bool,
}

impl<'a, 's> NextInChain<'a, 's> {
    fn get(entity: &'a Entity<'s>) -> Option<Self> {
        match entity {
            Entity::Headers(headers) => Some(NextInChain {
                is_tail: match &headers.last().unwrap().1.kind {
                    Entity::Header(..) | Entity::Headers(..) => false,
                    _ => true,
                },
                headers,
            }),
            _ => None,
        }
    }
}
