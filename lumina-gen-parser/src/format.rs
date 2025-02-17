use super::*;
use itertools::Itertools;
use std::fmt::Write;

pub struct Formatter<'s> {
    // indent_spaces: usize,
    indent_limit: usize,

    previous_kind: &'s str,

    src: &'s str,
}

fn kind_of<'s>(entity: &Entity<'s>) -> &'s str {
    match entity {
        Entity::Commented(_, _, inner) => kind_of(&inner),
        Entity::Header(header, _, _) => *header,
        Entity::Headers(headers) => match headers.first() {
            Some((header, _, _)) => *header,
            _ => "node",
        },
        Entity::Unary(_, "@", _) => "@",
        Entity::Missing => "",
        Entity::IndentBlock("where", _) => "where",
        Entity::IndentBlock("when", _) => "when",
        _ => "node",
    }
}

impl<'s> Formatter<'s> {
    pub fn new(src: &'s str) -> Self {
        Self { src, indent_limit: 60, previous_kind: "" }
    }

    pub fn toplevel(&mut self, entity: Tr<&Entity<'s>>) -> (usize, String) {
        let kind = kind_of(*entity);
        let spacing = if (self.previous_kind == "use" && kind == "use") || self.previous_kind == "@"
        {
            1
        } else if self.previous_kind == "" {
            0
        } else if kind == "where" || self.previous_kind == "when" {
            1
        } else {
            2
        };

        self.previous_kind = kind;

        (spacing, self.entity(entity).buf)
    }

    fn entity(&mut self, entity: Tr<&Entity<'s>>) -> Snippet {
        match entity.value {
            // TODO: allow comments to be detached from an entity. Ideally; it should just be a
            // part of Sequence.
            Entity::Commented(comment, doc, and_then) => Snippet::multiline(format!(
                "//{}{comment}\n{}",
                if *doc { "/" } else { "" },
                self.entity((**and_then).as_ref()).buf
            )),
            Entity::Isolated(_) => {
                let raw = entity.span.get_str(self.src);
                Snippet { multiline: raw.contains("\n"), buf: raw.to_string() }
            }
            Entity::Literal(lit) => self.literal(lit),
            Entity::Clause(open, close, inner) => self.clause(open, close, inner.as_deref()),
            Entity::Identifier(ident) => self.identifier(ident),
            Entity::Symbol(sym) => Snippet::singleline(sym.to_string()),
            Entity::Match(_, branches) => self.r#match(branches),
            Entity::DotPostfix(elems) => self.dot_postfix(elems),
            Entity::Header("type", _, and_then) => self.type_decl((**and_then).as_ref()),
            Entity::Header("fn", _, and_then) => self.function((**and_then).as_ref()),
            Entity::Header(name, span, and_then) => {
                self.headers(&[(*name, *span, (**and_then).clone())])
            }
            // {self
            //     .entity((**and_then).as_ref())
            //     .prepend(&format!("{name} "))
            // },
            Entity::Headers(headers) => self.headers(headers),
            Entity::Unary(_, op, elem) => self.entity((**elem).as_ref()).prepend(*op),
            Entity::Sequence(seq) => self.sequence(seq, false, "->"),
            Entity::IndentBlock("impl", members) => {
                let Some((header, members)) = members.split_first() else {
                    return Snippet::singleline("impl".into());
                };

                let mut buf = format!("impl {}\n", self.entity(header.as_ref()).buf);

                for (i, member) in members.iter().enumerate() {
                    if i != 0 {
                        write!(buf, "\n\n").unwrap();
                    }
                    let decl = self.entity(member.as_ref());
                    buf.push_str("  ");
                    write!(buf, "{}", decl.buf.lines().format("\n  ")).unwrap();
                }

                Snippet::multiline(buf)
            }
            Entity::IndentBlock("when", members) => self.when_block(members),
            Entity::IndentBlock(kw, members) => self.indent_block(*kw, members),
            Entity::Missing => Snippet::singleline(String::new()),
        }
    }

    fn literal(&mut self, lit: &Literal<'s>) -> Snippet {
        Snippet {
            buf: lit.to_string(),
            multiline: match lit {
                Literal::DoubleQuote(str) | Literal::SingleQuote(str) => str.contains('\n'),
                _ => false,
            },
        }
    }

    fn identifier(&mut self, ident: &Identifier<'s>) -> Snippet {
        Snippet::singleline(ident.to_string())
    }

    fn type_decl(&mut self, and_then: Tr<&Entity<'s>>) -> Snippet {
        match &and_then.value {
            Entity::Sequence(vec) => {
                let (last, xs) = vec.split_last().unwrap();
                let body = match &last.value {
                    Entity::Clause("{", "}", Some(inner)) => self.record_clause((**inner).as_ref()),
                    Entity::Header("=", _, and_then) => match &and_then.value {
                        Entity::Clause("{", "}", Some(inner)) => {
                            self.record_clause((**inner).as_ref())
                        }
                        Entity::Sequence(variants) => self.sum_variants(variants),
                        _ => self.entity((**and_then).as_ref()).prepend("="),
                    },
                    _ => todo!("{last:?}"),
                };

                return Snippet {
                    multiline: body.multiline,
                    buf: format!(
                        "type {} {}",
                        xs.iter().map(|v| self.entity(v.as_ref()).buf).format(" "),
                        body.buf,
                    ),
                };
            }
            Entity::Clause(_, _, _) => todo!(),
            _ => {}
        }

        self.entity(and_then).prepend("type ")
    }

    fn record_clause(&mut self, inner: Tr<&Entity<'s>>) -> Snippet {
        let ml_requested = inner.span.get_str(self.src).contains('\n');
        match &inner.value {
            Entity::Sequence(elems) => {
                let snippets: Vec<_> = Entity::iter_as_type_decl_fields(self.src, elems)
                    .map(|fieldline| {
                        fieldline
                            .into_iter()
                            .map(|f| self.entity(f).buf)
                            .format(" ")
                            .to_string()
                    })
                    .collect();

                if ml_requested
                    || elems.len() > 2
                    || snippets.iter().map(|buf| buf.len()).sum::<usize>() > self.indent_limit
                {
                    Snippet::multiline(format!(
                        "{{\n  {}\n}}",
                        snippets
                            .iter()
                            .map(|buf| buf.lines().format("\n  "))
                            .format("\n  ")
                    ))
                } else {
                    Snippet::singleline(format!("{{ {} }}", snippets.iter().format(", ")))
                }
            }
            _ => self.entity(inner),
        }
    }

    fn sum_variants(&mut self, variants: &[Tr<Entity<'s>>]) -> Snippet {
        let mut i = 0;
        let mut snippets = Vec::with_capacity(variants.len() / 2);
        loop {
            let Some(variant) = variants.get(i) else {
                break;
            };
            i += 1;
            match &variant.value {
                Entity::Sequence(elems) => {
                    let (x, xs) = elems.split_first().unwrap();
                    match &x.value {
                        Entity::Symbol("|") => {
                            snippets.push(
                                xs.iter()
                                    .map(|p| self.entity(p.as_ref()).buf)
                                    .format(" ")
                                    .to_string(),
                            );
                        }
                        _ => snippets.push(
                            elems
                                .iter()
                                .map(|p| self.entity(p.as_ref()).buf)
                                .format(" ")
                                .to_string(),
                        ),
                    }
                }
                _ => snippets.push(self.entity(variant.as_ref()).buf),
            }
        }

        let requested = Span::from_elems(variants, |v| v.span)
            .get_str(self.src)
            .contains('\n');

        if requested
            || snippets.iter().map(|snippet| snippet.len()).sum::<usize>() > self.indent_limit
        {
            let buf = format!("\n  = {}", snippets.into_iter().format("\n  | "));
            Snippet::multiline(buf)
        } else {
            let buf = format!("= {}", snippets.into_iter().format(" | "));
            Snippet::singleline(buf)
        }
    }

    fn function(&mut self, and_then: Tr<&Entity<'s>>) -> Snippet {
        if let Some(([patterns, ptypes, ret], expr)) = and_then.split_header_seq(["as", "->", "="])
        {
            // TODO: Multi-line check patterns and come up with some formatting for it
            let patterns = patterns
                .iter()
                .map(|pat| self.entity(pat.as_ref()).buf)
                .format(" ");

            let mut buf = format!("fn {patterns}");

            let ptypes = self.sequence(ptypes, true, "");
            let ret = self.sequence(ret, true, "");
            let expr = self.entity(expr);

            let multiline = ptypes.multiline || expr.multiline || ret.multiline;

            if ptypes.multiline {
                Snippet::fully_multiline_func_sig(&mut buf, ptypes, ret, expr);
            } else if ret.multiline
                || (buf.len() + ptypes.buf.len() + ret.buf.len() > self.indent_limit
                    && ret.buf.len() > 10)
            {
                Snippet::semi_multiline_func_sig(&mut buf, ptypes, ret, expr)
            } else {
                Snippet::singleline_func_sig(&mut buf, ptypes, ret, expr)
            }

            return Snippet { buf, multiline };
        }

        if let Some(([patterns, ret], expr)) = and_then.split_header_seq(["as", "="]) {
            // TODO: Multi-line check patterns and come up with some formatting for it
            let patterns = patterns
                .iter()
                .map(|pat| self.entity(pat.as_ref()).buf)
                .format(" ");

            let mut buf = format!("fn {patterns}");

            let ret = self.sequence(ret, true, "");
            let expr = self.entity(expr);

            let mut multiline = false;

            if ret.multiline || buf.len() + ret.buf.len() > self.indent_limit {
                multiline = true;
                write!(buf, "\n  as {} =", ret.buf)
            } else {
                write!(buf, " as {} =", ret.buf)
            }
            .unwrap();

            if expr.multiline {
                multiline = true;
                write!(buf, "\n  {}", expr.indent(2).buf)
            } else {
                write!(buf, " {}", expr.buf)
            }
            .unwrap();

            return Snippet { buf, multiline };
        }

        if let Some(([patterns], expr)) = and_then.split_header_seq(["="]) {
            let patterns = patterns.iter().map(|v| self.entity(v.as_ref())).collect();
            let expr = self.entity(expr).prepend_linebreak("=");
            return self
                .finalize_sequence(false, false, patterns, Some(expr))
                .prepend("fn ");
        }

        self.entity(and_then).prepend("fn ")
    }

    fn when_block(&mut self, members: &[Tr<Entity<'s>>]) -> Snippet {
        let binds: Vec<_> = members.iter().map(|v| self.entity(v.as_ref())).collect();

        match binds.as_slice() {
            [] => Snippet::singleline("when".into()),
            [bind] => Snippet::singleline(format!("when {}", bind.buf)),
            _ => Snippet::multiline(format!(
                "when\n  {}",
                binds.iter().map(|v| &v.buf).format("\n  ")
            )),
        }
        // println!("{}", members.iter().format("\n&&&&&&&\n"));
        // let mut bindings = vec![];
        // let mut decls = vec![];

        // for member in members {
        //     let snippet = self.entity(member.as_ref());

        //     if decls.is_empty() {
        //         match &member.value {
        //             Entity::Sequence(_) => bindings.push(snippet.buf),
        //             Entity::Identifier(ident) if ident.is_name() => bindings.push(snippet.buf),
        //             Entity::Header(_, _, _) => decls.push(snippet.buf),
        //             _ => decls.push(snippet.buf),
        //         }
        //     } else {
        //         decls.push(snippet.buf);
        //     }
        // }

        // let decls = decls.into_iter().format("\n\n");

        // let buf = match bindings.as_slice() {
        //     [] => format!("when\n{decls}"),
        //     [bind] => format!("when {bind}\n{decls}"),
        //     _ => format!("when\n  {}\n{decls}", bindings.into_iter().format("\n  ")),
        // };

        // Snippet::multiline(buf)
    }

    fn indent_block(&mut self, kw: &'s str, members: &[Tr<Entity<'s>>]) -> Snippet {
        let mut buf = match kw {
            "where" => format!(" {kw}\n"),
            _ => format!(" {kw}\n"),
        };
        let mut first = true;
        for entity in members {
            let (spacing, out) = self.toplevel(entity.as_ref());
            if !first {
                first = false;
                for _ in 0..spacing {
                    buf += "\n";
                }
            }
            write!(buf, "  {}", out.lines().format("\n  ")).unwrap();
        }
        Snippet::multiline(buf)
    }

    fn sequence(
        &mut self,
        seq: &[Tr<Entity<'s>>],
        remove_old_comma_syntax: bool,
        tail: &str,
    ) -> Snippet {
        let indent_after_fst = match &seq[0].value {
            Entity::Headers(..) | Entity::Identifier(..) => true,
            _ => false,
        };

        let (elems, end_with_header) = match seq.last() {
            Some(Tr { value: Entity::Headers(headers), .. })
                if headers[0].0 == tail && headers.len() == 1 =>
            {
                let (tail, elems) = seq.split_last().unwrap();
                let tail = self.entity(tail.as_ref());
                (elems, Some(tail))
            }
            Some(Tr { value: Entity::Header(name, _, _), .. }) if *name == tail => {
                let (tail, elems) = seq.split_last().unwrap();
                let tail = self.entity(tail.as_ref());
                (elems, Some(tail))
            }
            _ => (seq, None),
        };

        let (members, requested) = self.prepare_sequence(elems, remove_old_comma_syntax);

        self.finalize_sequence(indent_after_fst, requested, members, end_with_header)
    }

    fn finalize_sequence(
        &mut self,
        indent_after_fst: bool,
        requested: bool,
        elems: Vec<Snippet>,
        end_with_header: Option<Snippet>,
    ) -> Snippet {
        let indent = if indent_after_fst { "\n  " } else { "\n" };

        if requested
            || elems.iter().any(|snippet| snippet.multiline)
            || elems.iter().map(|snippet| snippet.buf.len()).sum::<usize>() > self.indent_limit
        {
            let mut members = elems.into_iter();
            let mut buf = members.next().unwrap().buf;
            for param in members {
                write!(buf, "{indent}{}", param.buf.lines().format(indent)).unwrap();
            }
            if let Some(snippet) = end_with_header {
                write!(buf, " {}\n  ", snippet.buf.lines().format(indent)).unwrap();
            }
            Snippet::multiline(buf)
        } else {
            let mut buf = String::new();
            let mut multiline = false;
            for snippet in elems.into_iter() {
                if snippet.buf.starts_with(",") || buf.is_empty() {
                    write!(buf, "{}", snippet.buf).unwrap();
                } else {
                    write!(buf, " {}", snippet.buf).unwrap();
                }
            }
            if let Some(tail) = end_with_header {
                if tail.multiline {
                    multiline = true;
                    write!(buf, " {}\n  ", tail.buf.lines().format("\n  ")).unwrap();
                } else {
                    write!(buf, " {}", tail.buf.lines().format("\n  ")).unwrap();
                }
            }
            Snippet { buf, multiline }
        }
    }

    fn upgrade_type_annotation_format(&mut self, and_then: Tr<&Entity<'s>>) -> Snippet {
        match and_then.value {
            Entity::Sequence(types) => self.sequence(types, true, "->").prepend("as "),
            _ => self.entity(and_then).prepend("as "),
        }
    }

    fn prepare_sequence(
        &mut self,
        elems: &[Tr<Entity<'s>>],
        remove_old_comma_syntax: bool,
    ) -> (Vec<Snippet>, bool) {
        let mut fixed = Vec::with_capacity(elems.len());
        let mut iter = elems.iter().peekable();

        while let Some(elem) = iter.next() {
            match &elem.value {
                // HACK: Try to make an guess at how to fix outdated syntax for fn type annotations
                Entity::Symbol(",") if remove_old_comma_syntax => {
                    let mut buf = format!("(");
                    let mut multiline = false;
                    while let Some(next) = iter.peek() {
                        match &next.value {
                            Entity::Identifier(_)
                            | Entity::Clause("(", ..)
                            | Entity::Unary(_, "*", _) => {
                                let v = iter.next().unwrap();
                                let ty = self.entity(v.as_ref());
                                multiline |= ty.multiline;
                                if buf != "(" {
                                    buf.push(' ');
                                }
                                buf.push_str(&ty.buf);
                            }
                            _ => break,
                        }
                    }
                    buf.push(')');
                    fixed.push(Snippet { multiline, buf });
                }
                Entity::Symbol(sym) => match iter.next() {
                    None => fixed.push(Snippet::singleline(sym.to_string())),
                    Some(elem) => {
                        let snippet = self.entity(elem.as_ref());
                        fixed.push(Snippet {
                            multiline: snippet.multiline,
                            buf: format!("{sym} {}", snippet.buf.lines().format("\n  ")),
                        })
                    }
                },
                _ => {
                    let snippet = self.entity(elem.as_ref());
                    fixed.push(snippet);
                }
            }
        }

        let span = Span::from_elems(elems, |v| v.span);
        let requested = span.get_str(self.src).contains('\n');

        (fixed, requested)
    }

    fn headers(&mut self, headers: &[(&'s str, Span, Tr<Entity<'s>>)]) -> Snippet {
        match headers {
            [("pub", _, and_then)] => self.entity(and_then.as_ref()).prepend("pub "),
            [("as", _, and_then)] => self.upgrade_type_annotation_format(and_then.as_ref()),
            [("=", _, and_then)] => self.entity(and_then.as_ref()).prepend_linebreak("="),
            [("->", _, and_then)] => self.entity(and_then.as_ref()).prepend_linebreak("->"),
            [(header, _, and_then)] => self
                .entity(and_then.as_ref())
                .prepend(&format!("{header} ")),
            [_if_, _then_, _else_] => self.headers3(headers),
            [("let", _, assignment), ("in", _, and_then)] => {
                let fst = self.entity(assignment.as_ref());
                self.headers2("let", fst, "in", and_then.as_ref())
            }
            [("do", _, fst), ("then", _, and_then)] => {
                let fst = self.entity(fst.as_ref());
                self.headers2("do", fst, "then", and_then.as_ref())
            }
            // [("=", _, one), ("")] => {
            //     todo!("PROBLEM: ");
            //     // handling `|` as a header continuation means it'll only occur *once*
            //     //
            //     // I think we should just edge-case `type` in `lib.rs`
            // }
            // [("=", _, fst), ("|", _, and_then)] => {
            //     todo!("keep in mind that this doesn't chain...");
            // }
            [x, xs @ ..] => {
                dbg!(&x, &xs);
                todo!();
                // let next_header = tail_as_header(xs);
                // self.traverse_header(next_header, x, xs)
            }
            [] => unreachable!(),
        }
    }

    fn r#match(&mut self, branches: &[Tr<Entity<'s>>]) -> Snippet {
        let mut iter = branches.iter().map(|v| self.entity(v.as_ref()));
        let matched = iter.next().unwrap();
        let branches = iter.collect::<Vec<_>>();

        let mut buf = if matched.multiline {
            format!("match\n  {}", matched.buf.lines().format("\n  "))
        } else {
            format!("match {}", matched.buf)
        };

        for snippet in branches {
            // TODO: go deeper for `->` line breaks
            write!(buf, "\n| {}", snippet.buf.lines().format("\n")).unwrap();
        }

        Snippet::multiline(buf)
    }

    fn dot_postfix(&mut self, elems: &[Tr<Entity<'s>>; 2]) -> Snippet {
        let lhs = self.entity(elems[0].as_ref());
        let rhs = self.entity(elems[1].as_ref());
        Snippet {
            buf: format!("{}.{}", lhs.buf, rhs.buf),
            multiline: lhs.multiline || rhs.multiline,
        }
    }

    fn clause(&mut self, open: &str, close: &str, inner: Option<&Tr<Entity<'s>>>) -> Snippet {
        match inner {
            Some(inner) => {
                // Edge-cases for removing unecesarry clauses
                if open == "(" {
                    match &inner.value {
                        Entity::Clause(o, _, iinner) if *o == "(" => {
                            return self.clause(open, close, iinner.as_deref());
                        }
                        Entity::Literal(lit) => return self.literal(lit),
                        Entity::Identifier(ident) => return self.identifier(ident),
                        _ => {}
                    }
                }

                let inner = self.entity(inner.as_ref());

                let spacer = if open == "{" { " " } else { "" };

                let buf = if inner.multiline {
                    format!("{open} {}\n{close}", inner.buf)
                } else {
                    format!("{open}{spacer}{}{spacer}{close}", inner.buf)
                };

                Snippet { multiline: inner.multiline, buf }
            }
            None => Snippet::singleline(format!("{open}{close}")),
        }
    }

    // if ... then
    //   ...
    // else
    //   ...
    //
    // if ...
    //   then ...
    //   else ...
    //
    // if
    //   long
    //   condition
    // then
    //   ...
    // else
    fn headers3(&mut self, headers: &[(&'s str, Span, Tr<Entity<'s>>)]) -> Snippet {
        let snippets = headers
            .iter()
            .map(|(name, _, entity)| (*name, self.entity(entity.as_ref())))
            .collect::<Vec<_>>();

        let buf = if snippets[0].1.multiline {
            // if
            //   ...
            // then
            //   ...
            // else
            //   ...
            snippets
                .into_iter()
                .format_with("\n  ", |(name, value), f| {
                    f(&format_args!(
                        "{name}\n  {}",
                        value.buf.lines().format("\n  ")
                    ))
                })
                .to_string()
        } else if snippets.iter().any(|(_, snippet)| snippet.multiline) {
            self.header_expanded_3_or_more(&snippets, headers[2].2.as_ref())
        } else {
            // if ...
            //   then ...
            //   else ...
            snippets
                .iter()
                .format_with("\n  ", |(name, value), f| {
                    f(&format_args!("{name} {}", value.buf))
                })
                .to_string()
        };

        Snippet::multiline(buf)
    }

    fn header_expanded_3_or_more(
        &mut self,
        snippets: &[(&'s str, Snippet)],
        else_cond: Tr<&Entity<'s>>,
    ) -> String {
        let mut buf = format!("{} {} ", snippets[0].0, snippets[0].1.buf);

        let result: Result<&[_; 3], _> = snippets.try_into();
        if let Ok([if_, then_, else_]) = result {
            if let Entity::Headers(headers) = &*else_cond {
                if headers[0].0 == if_.0 && headers.len() == 3 {
                    let else_if_condition = self.entity(headers[0].2.as_ref());
                    if !else_if_condition.multiline {
                        // if ... then
                        //   ...
                        // else if ... then
                        //   ...
                        // else
                        //   ...
                        let snippets = headers
                            .iter()
                            .map(|(name, _, entity)| (*name, self.entity(entity.as_ref())))
                            .collect::<Vec<_>>();

                        let next_in_chain =
                            self.header_expanded_3_or_more(&snippets, headers[2].2.as_ref());

                        write!(
                            buf,
                            "{}\n  {}\n{} {}",
                            then_.0,
                            then_.1.buf.lines().format("\n  "),
                            else_.0,
                            next_in_chain,
                        )
                        .unwrap();

                        return buf;
                    }
                }
            }
        }

        // if ... then
        //   ...
        // else
        //   ...
        write!(
            buf,
            "{}\n  {}",
            snippets[1].0,
            snippets[1].1.buf.lines().format("\n  ")
        )
        .unwrap();
        for (name, snippet) in snippets.iter().skip(2) {
            write!(buf, "\n{name}\n  {}", snippet.buf.lines().format("\n  ")).unwrap();
        }

        buf
    }

    fn headers2(
        &mut self,
        kw: &str,
        fst: Snippet,
        then: &str,
        and_then: Tr<&Entity<'s>>,
    ) -> Snippet {
        let chained = match and_then.value {
            Entity::Headers(headers) => headers[0].0 == kw,
            _ => false,
        };

        if chained {
            let and_then = self.entity(and_then);
            let buf = if fst.multiline {
                format!(
                    "{kw}\n  {}\n{then}\n{}",
                    fst.buf.lines().format("\n  "),
                    and_then.buf
                )
            } else {
                format!("{kw} {} {then}\n{}", fst.buf, and_then.buf)
            };

            Snippet::multiline(buf)
        } else if fst.multiline {
            let and_then = self.entity(and_then);
            Snippet::multiline(format!(
                "{kw}\n  {}\n{then}\n  {}",
                fst.buf.lines().format("\n  "),
                and_then.buf.lines().format("\n  ")
            ))
        } else {
            let and_then = self.entity(and_then);
            if and_then.multiline {
                Snippet::multiline(format!(
                    "{kw} {} {then}\n  {}",
                    fst.buf,
                    and_then.indent(2).buf
                ))
            } else if fst.buf.len() + and_then.buf.len() < self.indent_limit / 2 {
                Snippet::singleline(format!("{kw} {} {then} {}", fst.buf, and_then.buf))
            } else {
                Snippet::multiline(format!("{kw} {}\n {then} {}", fst.buf, and_then.buf))
            }
        }
    }
}

#[derive(Debug)]
struct Snippet {
    buf: String,
    multiline: bool,
}

impl Snippet {
    pub fn multiline(buf: String) -> Self {
        Self { buf, multiline: true }
    }
    pub fn singleline(buf: String) -> Self {
        Self { buf, multiline: false }
    }

    fn prepend(self, kw: &str) -> Snippet {
        Snippet { multiline: self.multiline, buf: format!("{kw}{}", self.buf) }
    }

    fn prepend_linebreak(self, kw: &str) -> Snippet {
        Snippet {
            multiline: self.multiline,
            buf: if self.multiline {
                format!("{kw}\n{}", self.buf.lines().format("\n"))
            } else {
                format!("{kw} {}", self.buf)
            },
        }
    }

    fn indent(self, by: usize) -> Snippet {
        let delim = std::iter::once('\n')
            .chain(std::iter::repeat(' ').take(by))
            .collect::<String>();

        Snippet {
            buf: self.buf.lines().format(&delim).to_string(),
            multiline: self.multiline,
        }
    }

    fn fully_multiline_func_sig(buf: &mut String, ptypes: Snippet, ret: Snippet, expr: Snippet) {
        write!(buf, "\n  as {}", ptypes.indent(5).buf).unwrap();
        write!(buf, "\n   ->\n     ").unwrap();
        write!(buf, "{}", ret.indent(5).buf).unwrap();
        write!(buf, "\n  =\n  {}", expr.indent(2).buf).unwrap();
    }

    fn semi_multiline_func_sig(buf: &mut String, ptypes: Snippet, ret: Snippet, expr: Snippet) {
        write!(
            buf,
            " as {} ->\n    {} =\n  {}",
            ptypes.buf,
            ret.indent(4).buf,
            expr.indent(2).buf
        )
        .unwrap()
    }

    fn singleline_func_sig(buf: &mut String, ptypes: Snippet, ret: Snippet, expr: Snippet) {
        write!(buf, " as {} -> {} =", ptypes.buf, ret.buf).unwrap();
        if expr.multiline || buf.len() + expr.buf.len() > 50 {
            write!(buf, "\n  {}", expr.indent(2).buf)
        } else {
            write!(buf, " {}", expr.buf)
        }
        .unwrap()
    }
}
