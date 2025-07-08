use super::*;
use lumina_parser as parser;

pub(super) struct FuncNodeLower<'v, 's> {
    header: &'v parser::func::Header<'s>,
    ctx: &'v mut rvsdg::TranslationUnitContext,
    // externals: &'v mut SecondaryMap<rvsdg::id::Argument, External<'s>>,
    scopes: Scopes<'s>,
    unresolved_deps: SecondaryMap<rvsdg::id::Input, Option<Identifier<'s>>>,

    func_node: rvsdg::id::Node<rvsdg::nodes::Lambda>,
}

impl<'v, 's> FuncNodeLower<'v, 's> {
    pub fn new(
        header: &'v parser::func::Header<'s>,
        ctx: &'v mut rvsdg::TranslationUnitContext,
        // externals: &'v mut SecondaryMap<rvsdg::id::Argument, External<'s>>,
    ) -> Self {
        let func_node = ctx.add_lambda_node().node;

        ctx.add_symbol(func_node.id, header.name.to_string());

        Self {
            func_node,
            header,
            ctx,
            scopes: Scopes::new(),
            unresolved_deps: SecondaryMap::new(),
        }
    }

    pub fn add_func_ext(&mut self, ident: Identifier<'s>) -> rvsdg::Argument {
        let input = self.ctx.add_input(self.func_node);
        self.unresolved_deps[input.id] = Some(ident);
        self.ctx.input_as_argument(input)
    }

    pub fn body(&mut self, body: &parser::func::Body<'s>) {
        // NOTE: if we *aren't* gonna let the recenv do its own thing implicitly, then we do need
        // to put the function in the recenv as well. Not just the where-bindings.

        // let lambda = self.ctx.add_lambda_node();
        // let region = self.ctx.region(lambda.node.id);

        let region = self.ctx.region(self.func_node.id);
        self.ctx.switch_region(region);
        let result = self.ctx.add_result();

        let expr_node = self.expr(&body.expr);
        self.ctx.add_symbol(expr_node.node.id, "entry");
        self.ctx.connect(expr_node, result);

        // TODO: do we even need scope at this step?
        //
        // Hm, ye I think so. Because we *do* want to connect those nodes right?
        //
        // OR: do we just want to leave everything completely unconnected?
        //
        // It might be better to leave everything disconnected. Although; then we wont really know
        // which node to declare...
        //
        // Ye no I think we'll use scope. It makes more sense.

        // TODO: Do I just recenv them right away?
        //
        // I suspect that'd be unecesarry.
        //
        // But it could also be convenient.
        //
        // And we're definitely gonna have an optimization later that removes unecesarry phi nodes.
        //
        // we can probably just push them to Scopes.

        // TODO: Remove the recenv stuff. we'll let connect implicitly sort it out instead.
        //
        // We sort of have to do it that way because there's still caess of which we can't know
        // about recursion.

        // NVM: It's kind of convenient to have them in the same thing since we can then use
        // `unresolved` for all these functions combined.
        //
        // *ALTERNATIVLY*: We create a seperate ExprNodeLower for each where binding?
        //
        // I suspect that makes sense...
        //
        // But; how will we handle captures?
        //
        // That's actually surprisingly tricky...
        //
        // I think we should just have all closures have one capture group and then use destructor
        // nodes.
        //
        // We can lower that right away.
        // So; where-binds will just be lowered to lambdas.
        //
        // PERHPAS: literally all functions should just have a capture group for consistency?
        //
        // And then we leave the other ones empty.
        //
        // Honestly? I kind of like that.

        // To allow for where-bindings to call each other recursively; we put them in an RecEnv.
        //
        // If this doesn't end up happening, the RecEnv will be optimized out regardless.
        // let env = self.ctx.add_recenv_node();
        // let env_region = self.ctx.region(env.id);
        // self.ctx.switch_region(env_region);

        // // Declare a lambda node for each where-binding in this function
        // for where_bind in &body.where_binds {
        //     let node = self.ctx.add_lambda_node();
        //     self.scopes
        //         .scopes
        //         .last_mut()
        //         .unwrap()
        //         .push((*where_bind.header.name, node.into()));
        // }

        // let expr_node = self.expr(&body.expr);
        // self.ctx.connect(expr_node, result);
    }

    pub fn patterns(&mut self, patterns: &[Tr<parser::Pattern<'s>>]) {
        for pat in patterns {
            let arg = self.ctx.add_argument();
            // todo!("No this is not at all reasonable either. ");
            // Because; creating a node this way means that using a bind will then no longer be
            // using an output.
            //
            // Pretty sure we need an intermediate data structure.
            //
            // PERHAPS: the tree merging stuff can simply be repeated expression elimiation and go
            // under ordinary optimizations?
            //
            // It might not even be all that tricky to generate an exhaustivity graph from an
            // RVSDG. I think I might do that.
            // self.match_node(arg.into(), pat);
            self.destruct(arg.into(), pat.as_ref());
        }
    }

    fn destruct(&mut self, v: rvsdg::Origin, pat: Tr<&parser::Pattern<'s>>) -> () {
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
            parser::Pattern::Fields(curly_init, fields) => todo!(),
            parser::Pattern::List(trs, list_length) => todo!(),
            parser::Pattern::Tuple(trs) => todo!(),
            parser::Pattern::Int(_) => todo!(),
            parser::Pattern::Float(_) => todo!(),
            parser::Pattern::Operators { init, ops } => todo!(),
            parser::Pattern::Poison => todo!(),
        }
    }

    // fn destructs(
    //     &mut self,
    //     v: rvsdg::Origin,
    //     pat: &[Tr<parser::Pattern<'s>>],
    // ) -> Vec<rvsdg::Output<rvsdg::id::AnyNode>> {
    //     todo!();
    // }

    pub fn expr(&mut self, expr: &parser::Expr<'s>) -> rvsdg::Output<rvsdg::id::AnyNode> {
        // TODO: and then we could also collect like a `id::AnyNode -> Type` mapping somewhere
        // which is later used during type checking?

        match expr {
            parser::Expr::Lit(lit) => {
                let lit = nodes::Literal::from(lit);

                let node = self.ctx.add_node(|_, _| (lit, []));
                self.ctx.add_output(node).downgrade()
            }
            parser::Expr::Call(apath, params) => {
                let apply = self.ctx.add_apply_node();
                let output = self.ctx.add_output(apply.node).downgrade();

                if let [name] = apath.path.as_slice() {
                    if let Some(called) = self.scopes.get(name) {
                        self.ctx.connect(called, apply);
                        self.apply_params(apply.node, params);
                        return output;
                    }
                }

                let arg = self.add_func_ext(apath.path.clone());
                self.ctx.connect(arg, apply);
                self.apply_params(apply.node, params);
                output
            }
            parser::Expr::Group(expr) => self.expr(&*expr),
            parser::Expr::DotPipe(_) => todo!(),
            parser::Expr::FieldAccess(tr, tr1) => todo!(),
            parser::Expr::TupleAccess(tr, tr1) => todo!(),
            parser::Expr::Lambda(trs, trs1, tr) => todo!(),
            parser::Expr::CallExpr(tr, trs) => todo!(),
            parser::Expr::Operators { init, ops } => todo!(),
            parser::Expr::Match(tr, items) => {
                todo!();
            }
            parser::Expr::CastAs(tr, tr1) => todo!(),
            parser::Expr::List(trs, list_length) => todo!(),
            parser::Expr::Tuple(trs) => todo!(),
            parser::Expr::Record { init, fields } => todo!(),
            parser::Expr::If(_) => todo!(),
            parser::Expr::Do(_) => todo!(),
            parser::Expr::Let(tr, _) => todo!(),
            parser::Expr::Pass(tr) => todo!(),
            parser::Expr::PassFptr(annotated_path) => todo!(),
            parser::Expr::Poison => todo!(),
        }
    }

    fn apply_params(
        &mut self,
        apply: rvsdg::id::Node<rvsdg::nodes::Apply>,
        params: &[Tr<parser::Expr<'s>>],
    ) {
        for p in params {
            let pnode = self.expr(p);

            // TODO: do we need to context switch? or will we try to garentee that we'll be
            // always be working with a clean state that resets itself accordingly?

            dbg!();
            let input = self.ctx.add_input(apply);
            self.ctx.connect(pnode, input);
        }
    }

    fn resolve_in_scopes(&self, name: &str) -> Option<rvsdg::Output<rvsdg::id::AnyNode>> {
        // IDEA: what if instead of having current: id::Node we instead have `current: Vec<id::Node>`?
        //
        // If we do that we could theoretically automate a lot more of the connecting.
        //
        // Actually, we don't even need that as long as nodes are aware of their parents. Ye that's
        // much simpler
        //
        // Infact; we could even just make that a part of connect.
        // for scope in self.scopes.scopes.iter().rev() {
        //     todo!();
        // }
        todo!();
    }
}

struct Scopes<'s> {
    scopes: Vec<Vec<(&'s str, rvsdg::Origin)>>,
}

impl<'s> Scopes<'s> {
    fn new() -> Self {
        Self { scopes: vec![] }
    }

    fn declare(&mut self, name: &'s str, out: rvsdg::Origin) {
        self.scopes.last_mut().unwrap().push((name, out))
    }

    fn get(&self, name: &str) -> Option<rvsdg::Origin> {
        todo!();
    }
}

impl<'v, 's> FuncNodeLower<'v, 's> {
    fn poison_node(&mut self) -> rvsdg::Output<nodes::Poison> {
        let node = self.ctx.add_node(|ctx, node| (nodes::Poison {}, []));
        self.ctx.add_output(node)
    }
}
