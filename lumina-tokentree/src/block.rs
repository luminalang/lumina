use super::*;

impl<'s> Parser<'s> {
    pub fn handle_whereblock_if_not_expected(
        &mut self,
        allow_missing: bool,
        span: Span,
    ) -> Entity<'s> {
        if let Some(ctx) = self.ctx.last() {
            match ctx {
                Context::Header("fn" | "fnptr") => {
                    return self.next_then(|this| this.indent_block(span))
                }
                // If we're inside a clause then this is an invalid but still an parseable where binding set
                Context::Clause(_) => return self.next_then(|this| this.indent_block(span)),

                Context::Operators(_)
                | Context::InMatch(_)
                | Context::Indent(_)
                | Context::Header(_) => {}
            }
        }

        if !allow_missing {
            return self.next_then(|this| this.indent_block(span));
        }

        assert!(allow_missing);
        Entity::Missing
    }

    pub fn indent_block(&mut self, span: Span) -> Entity<'s> {
        let name = self.take(span);
        self.ctx.push(Context::Indent(name));
        let src = self.lexer.source();

        let mut buf = vec![];

        let finalize = |this: &mut Self, buf| {
            assert!(matches!(this.ctx.pop().unwrap(), Context::Indent(..)));
            Entity::IndentBlock(name.tr(span), buf)
        };

        loop {
            let token = self.lexer.peek();

            if token.kind == TokenKind::Eof {
                return finalize(self, buf);
            }

            let where_indent = get_indent_level(src, span, false).unwrap();
            if let Some(indent) = get_indent_level(src, token.span, true) {
                if indent <= where_indent {
                    return finalize(self, buf);
                }
            }
            // No need to handle `None` because we allow `where fn ...`

            let declaration = self.next(false);
            if declaration.kind == Entity::EOF {
                todo!()
            }
            buf.push(declaration);
        }
    }
}
