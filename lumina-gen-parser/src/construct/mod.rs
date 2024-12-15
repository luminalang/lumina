use super::*;

trait BuilderT<'s, T> {
    fn build(&mut self, elem: &Entity<'s>) -> T;

    fn numerous<V, F>(&mut self, elem: &Entity<'s>, f: F) -> Vec<V>
    where
        F: FnMut(&mut Self, &Entity<'s>) -> V,
    {
        let mut buf = Vec::new();
        self.numerous_into(&mut buf, elem, f);
        buf
    }

    fn numerous_into<V>(
        &mut self,
        buf: &mut Vec<V>,
        elem: &Entity<'s>,
        mut f: impl FnMut(&mut Self, &Entity<'s>) -> V,
    ) {
        match elem {
            Entity::Commented(_, _, xs) => self.numerous_into(buf, xs, f),
            Entity::Sequence(elems) => {
                buf.reserve_exact(elems.len());
                elems.iter().for_each(|elem| buf.push(f(self, elem)))
            }
            single => {
                buf.reserve_exact(1);
                let v = f(self, single);
                buf.push(v);
            }
        }
    }
}

struct Builder<'s> {
    a: &'s (),
}

enum Declaration<'s> {
    Function(Identifier<'s>, Signature<'s>, Vec<Pattern<'s>>, Expr<'s>),
}

impl<'s> BuilderT<'s, Declaration<'s>> for Builder<'s> {
    fn build(&mut self, tree: &Entity<'s>) -> Declaration<'s> {
        match tree {
            Entity::Keyword("fn", decl) => self.func(decl),
            _ => panic!("unexpected: {tree:?}"),
        }
    }
}

impl<'s> Builder<'s> {
    fn func(&mut self, tree: &Entity<'s>) -> Declaration<'s> {
        match tree {
            Entity::Delim(_, "as", elems) => {
                let (name, params): (Identifier<'s>, _) = match &elems[0].value {
                    Entity::Sequence(elems) => match &elems[0].value {
                        Entity::Identifier(ident) => {
                            todo!("actual numerous");
                        }
                        _ => panic!("no name"),
                    },
                    Entity::Identifier(ident) => (ident.clone(), vec![]),
                    _ => panic!("unexpected"),
                };
                // elems[0].numerous(&mut buf, Self::pattern);
                let (sig, body) = self.signature(&elems[1]);
                let body = self.body(body);
                Declaration::Function(name, sig, params, body)
            }
            Entity::Delim(_, "=", elems) => {
                todo!();
            }
            _ => panic!("???"),
        }
    }

    fn signature<'e>(&mut self, tree: &'e Entity<'s>) -> (Signature<'s>, &'e Entity<'s>) {
        match tree {
            Entity::Delim(_, "->", lhs) => {
                println!("\n{}", &lhs[0]);
                println!("\n{}", &lhs[1]);

                let params = self.numerous(&lhs[0], |this, v| this.ty(false, v));

                match &lhs[1].value {
                    Entity::Commented(..) => todo!(),
                    Entity::Delim(_, "=", elems) => {
                        let ret = self.ty(true, &elems[0]);
                        let signature = Signature(params, Box::new(ret));
                        let body = &elems[1];
                        (signature, body)
                    }
                    a => panic!("{a}"),
                }
            }
            _ => panic!("unexpected: {tree}"),
        }
    }

    fn body(&mut self, tree: &Entity<'s>) -> Expr<'s> {
        todo!();
    }

    fn ty(&mut self, params: bool, tree: &Entity<'s>) -> Type<'s> {
        match tree {
            Entity::Clause("(", ")", None) => Type::Tuple(vec![]),
            Entity::Clause("[", "]", None) => Type::List(vec![]),
            Entity::Clause("(", ")", Some(inner)) => match &inner.value {
                Entity::Delim(_, ",", _) => todo!("tuples"),
                single => self.ty(true, single),
            },
            Entity::Clause("[", "]", Some(inner)) => match &inner.value {
                Entity::Delim(_, ",", _) => todo!("lists"),
                single => {
                    let ty = self.ty(true, single);
                    Type::List(vec![ty])
                }
            },
            Entity::Identifier(ident) => Type::Identifier(ident.clone()),
            Entity::Sequence(elems) if params => {
                let takes = self.ty(false, &elems[0]);
                let params = todo!("actual multiple");
                // let params = self.numerous(&elems[1..], |v| v.ty(false));
                Type::WithParams(Box::new(takes), params)
            }
            Entity::Keyword("fn", elem) if params => {
                let sig = self.func_types(elem);
                Type::Func(FuncKind::Pointer, sig)
            }
            _ => panic!("{tree:?}"),
        }
    }

    fn func_types(&mut self, tree: &Entity<'s>) -> Signature<'s> {
        match tree {
            Entity::Delim(_, "->", elems) => {
                let params = self.numerous(&elems[0], |this, v| this.ty(false, v));
                let ret = self.ty(true, &elems[1]);
                Signature(params, Box::new(ret))
            },
            _ => panic!("missing return type? do we want to default to unit or default to ret, probably error"),
        }
    }

    fn pattern(&self, tree: &Entity<'s>) -> Pattern<'s> {
        match tree {
            Entity::Identifier(ident) => Pattern::Identifier(ident.clone()),
            _ => panic!("not a pattern: {tree}"),
        }
    }
}

enum Pattern<'s> {
    Identifier(Identifier<'s>),
}
struct Signature<'s>(Vec<Type<'s>>, Box<Type<'s>>); // TODO: we can optimize the layout if we hide internal repr
enum Expr<'s> {
    Identifier(Identifier<'s>),
}
enum Type<'s> {
    WithParams(Box<Self>, Vec<Self>),
    Func(FuncKind, Signature<'s>),
    Identifier(Identifier<'s>),
    Tuple(Vec<Self>),
    List(Vec<Self>),
}

enum FuncKind {
    Closure,
    Pointer,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_from_tree() {
        let src = "
        fn fold f acc list as (fn b a -> b) b [a] -> b = 0
          match list
          | [x : xs] -> fold #f (f x acc) xs
          | [] -> []
            ";
        let entities = Parser::new(src).everything();

        let mut builder = Builder { a: &() };

        let _ = entities
            .iter()
            .map(|entity| builder.func(entity))
            .collect::<Vec<_>>();

        todo!();
    }
}
