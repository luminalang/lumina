use super::{nodes, AbsoluteIdent, Export, Item, ProjectNode};
use crate::Files;
use cranelift::codegen::entity::SecondaryMap;
use lumina_parser as parser;
use lumina_util::{Identifier, Tr};
use nodes::LuminaNodes;
use rvsdg::{id::AnyNode, Argument, Input, Origin, Output};
use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::Path;

mod expr;
mod pat;
mod record;

/// Builds RVSDG nodes from parsed items.
///
/// Does not resolve and connect dependencies.
pub struct NodeBuilder<'a, 's> {
    node: &'a ProjectNode,
    // project: ParsedProject<'s>,
    ctx: rvsdg::TranslationUnitContext,
    omega: rvsdg::id::Region,

    files: Files,

    // The items contained in this translation unit
    internal: HashMap<AbsoluteIdent, Item>,

    // The items exposed by this translation unit
    external: HashMap<AbsoluteIdent, Export>,

    use_items: Vec<parser::r#use::Declaration<'s>>,

    path: Vec<String>,
}

impl<'a, 's> NodeBuilder<'a, 's> {
    pub fn new(node: &'a ProjectNode) -> Self {
        let ctx = rvsdg::TranslationUnitContext::new();
        let omega = ctx.region;
        Self {
            node,
            files: Files::new(),
            use_items: vec![],
            omega,
            // deps,
            path: vec![],
            ctx,
            internal: HashMap::new(),
            external: HashMap::new(),
        }
    }

    pub fn finish(self) -> rvsdg::TranslationUnitContext {
        self.ctx
    }

    fn canonicalize_item(&self, name: &str) -> AbsoluteIdent {
        AbsoluteIdent(
            self.path
                .iter()
                .map(|string| string.as_str())
                .chain(std::iter::once(name))
                .collect(),
        )
    }

    pub fn include_project(&mut self, node: &ProjectNode) -> () {
        let src_dir = std::fs::read_dir(&node.src_dir).unwrap();

        for entry in src_dir {
            let entry = entry.unwrap();
            let path = entry.path();
            if let Err(err) = self.include_entry(&path) {
                let err = lumina_util::Error::error("general file error").with_text(format!(
                    "Could not read {}: {}",
                    entry.path().display(),
                    err
                ));

                eprintln!("{err}");
            }
        }
    }

    fn include_entry(&mut self, path: &Path) -> Result<(), std::io::Error> {
        let Some(file_name) = path.file_name() else {
            return Ok(());
        };

        if path.extension() == Some(&OsStr::new("lm")) && path.is_file() {
            if file_name != OsStr::new("lib") && file_name != OsStr::new("main") {
                let utf8 = file_name.to_str().expect("filename is not valid UTF-8");
                self.path.push(utf8.to_string());
            }

            let file_key = self.files.open(path)?;

            unsafe {
                let src = self.files.get_unsafe(file_key);
                self.include_file(&src);
            }

            return Ok(());
        }

        if path.is_dir() {
            todo!();
        }

        Ok(())
    }

    fn include_file(&mut self, src: &'s str) {
        // We probably don't need to store the items anywhere intermediately.
        //
        // We can just create the nodes and then connect them later.
        //
        // Also; I think we're gonna start with only incrementally storing non-monomorphised RVSDG.
        let mut parser = parser::Parser::new(src);

        while let Some((span, item)) = parser.item() {
            match item {
                parser::Declaration::ModuleAttribute(span, trs) => todo!(),
                parser::Declaration::Function(declaration) => self.include_func(declaration),
                parser::Declaration::Alias(declaration) => todo!(),
                parser::Declaration::Type(declaration) => todo!(),
                parser::Declaration::Impl(declaration) => todo!(),
                parser::Declaration::Use(declaration) => self.include_use(declaration),
                parser::Declaration::Val(declaration) => todo!(),
                parser::Declaration::Failure => todo!(),
            }
        }
    }

    fn include_func(&mut self, declaration: parser::func::Declaration<'s>) {
        let mut flower = FuncNodeLower::new(&declaration.header, &mut self.ctx);

        flower.func_params(&declaration.header.params);

        if let Some(body) = declaration.body.as_ref() {
            flower.body(body);
        }
    }

    fn include_use(&mut self, declaration: parser::r#use::Declaration<'s>) {
        self.use_items.push(declaration);
    }

    // Add an external dependency for the translation unit and expose it as an item
    // fn add_ext_as_item(&mut self, abs: AbsoluteIdent, ext: External<'s>) -> rvsdg::Argument {
    //     // let arg = self.externals.add(&mut self.ctx, ext);
    //     let item = Item::External(arg.id);
    //     self.internal.insert(abs, item);
    //     arg
    // }
}

pub(super) struct FuncNodeLower<'v, 's> {
    header: &'v parser::func::Header<'s>,
    ctx: &'v mut rvsdg::TranslationUnitContext,
    // externals: &'v mut SecondaryMap<rvsdg::id::Argument, External<'s>>,
    scopes: Scopes<'s>,
    unresolved_deps: SecondaryMap<rvsdg::id::Input, Option<Identifier<'s>>>,
    where_binds: Vec<(Tr<&'s str>, Output<rvsdg::nodes::Lambda>)>,

    type_ascriptions: SecondaryMap<rvsdg::id::AnyNode, Vec<parser::Type<'s>>>,

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
            where_binds: vec![],
            type_ascriptions: SecondaryMap::new(),
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

        // Declare the where bindings as nodes
        for where_bind in &body.where_binds {
            let node = self.ctx.add_lambda_node();
            let region = self.ctx.region(node.node.id);
            self.ctx.in_region(region, |ctx| ctx.add_result());
            self.where_binds.push((where_bind.header.name, node));
        }

        let region = self.ctx.region(self.func_node.id);
        self.ctx.switch_region(region);
        let result = self.ctx.add_result();

        let expr_node = self.expr(&body.expr);
        self.ctx.add_symbol(expr_node.node.id, "entry");
        self.ctx.connect(expr_node, result);
    }

    pub fn func_params(&mut self, patterns: &[Tr<parser::Pattern<'s>>]) {
        for pat in patterns {
            let arg = self.ctx.add_argument();

            // IDEA: What if we create a complex nodes that represents the full match.
            //
            // That way; It'll be much easier to know where to check exhaustivity later.
            //
            // Actually; I suppose each pattern could be a complex node?
            //
            // {x, y}
            //
            // Could have the record as a complex node?
            //
            // But what would the outputs even be?
            //
            // oh ye the outputs would be the fields

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

    fn lambda(
        &mut self,
        patterns: &[Tr<parser::Pattern<'s>>],
        expr: Tr<&parser::Expr<'s>>,
    ) -> Output<rvsdg::nodes::Lambda> {
        let ((), cap_input, lambda) = self.in_lambda(|this| {
            let expr = this.expr(&expr);
            let result = this.ctx.add_result();
            this.ctx.connect(expr, result);
        });

        // TODO: should this be a lambda or a closure node?
        //
        // Depends on how we're gonna handle closures I suppose.
        //
        // Actually, it can just be an input for the closure can't it?
        //
        // That's convenient
        todo!();
    }

    fn apply<O>(&mut self, f: O, params: &[Tr<parser::Expr<'s>>]) -> Output<rvsdg::nodes::Apply>
    where
        O: Into<Origin>,
    {
        let input = self.ctx.add_apply_node();
        let output = self.ctx.add_output(input.node);
        self.ctx.connect(f, input);
        self.apply_params(input.node, params);
        output
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

    fn resolve_local(&mut self, name: &str) -> Option<rvsdg::Origin> {
        self.resolve(name).or_else(|| self.resolve_where_bind(name))
    }

    fn resolve_where_bind(&self, name: &str) -> Option<rvsdg::Origin> {
        self.where_binds
            .iter()
            .find_map(|(n, o)| (**n == name).then_some(o.downcast()))
            .map(rvsdg::Origin::from)
    }

    fn in_lambda<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> T,
    ) -> (T, Input<rvsdg::nodes::Lambda>, Output<rvsdg::nodes::Lambda>) {
        let lambda_output = self.ctx.add_lambda_node();
        let capture_input = self.ctx.add_input(lambda_output.node);

        let region = self.ctx.region(lambda_output.node.id);
        let previous = self.ctx.region;
        self.ctx.switch_region(region);

        let capture_argument = self.ctx.add_argument();

        self.scopes.scopes.push(Scope::Lambda {
            capture_map: vec![],
            captures: capture_argument,
            node: lambda_output.node,
        });
        self.scopes.scopes.push(Scope::Plain(vec![]));

        let t = f(self);
        self.ctx.switch_region(previous);

        let capture_map = self.scopes.pop_lambda();
        for (origin, accessor) in capture_map {
            todo!();
        }

        (t, capture_input, lambda_output)
    }

    fn resolve(&mut self, name: &str) -> Option<rvsdg::Origin> {
        Self::resolve_from_scope(self.ctx, self.scopes.scopes.as_mut_slice(), name)
    }

    fn resolve_from_scope(
        ctx: &mut rvsdg::TranslationUnitContext,
        scopes: &mut [Scope<'s>],
        name: &str,
    ) -> Option<rvsdg::Origin> {
        match scopes.split_first_mut().unwrap() {
            (Scope::Plain(items), upper) => items
                .iter()
                .find_map(|&(n, origin)| (n == name).then_some(origin))
                .or_else(|| Self::resolve_from_scope(ctx, upper, name)),

            (Scope::Lambda { captures, capture_map, .. }, upper) => {
                Self::resolve_from_scope(ctx, upper, name).map(|origin| {
                    match capture_map.iter().find(|(o, _)| *o == origin) {
                        Some((_, output)) => Origin::from(*output),
                        None => {
                            // Create a node that fetches the correct capture from the capture
                            // record.
                            let i = capture_map.len();
                            let output = ctx.add_accessor_node(*captures, i);
                            capture_map.push((origin, output));
                            output.into()
                        }
                    }
                })
            }
        }
    }
}

struct Scopes<'s> {
    scopes: Vec<Scope<'s>>,
}

enum Scope<'s> {
    Plain(Vec<(&'s str, rvsdg::Origin)>),

    Lambda {
        // The lambda node
        node: rvsdg::id::Node<rvsdg::nodes::Lambda>,
        // The capture tuple input
        captures: Argument,
        // A mapping of captures of upper scope to implicit capture accessor
        capture_map: Vec<(rvsdg::Origin, Output<nodes::FieldAccessor>)>,
    },
}

impl<'s> Scopes<'s> {
    fn new() -> Self {
        Self { scopes: vec![Scope::Plain(vec![])] }
    }

    fn declare(&mut self, name: &'s str, out: rvsdg::Origin) {
        match self.scopes.last_mut().unwrap() {
            Scope::Plain(items) => items.push((name, out)),
            Scope::Lambda { .. } => {
                unreachable!("Lambda is missing a plain scope")
            }
        }
    }

    fn pop_lambda(&mut self) -> Vec<(rvsdg::Origin, Output<nodes::FieldAccessor>)> {
        assert!(matches!(self.scopes.pop(), Some(Scope::Plain(_))));
        match self.scopes.pop() {
            Some(Scope::Lambda { capture_map, .. }) => capture_map,
            _ => panic!("not in lambda"),
        }
    }
}

impl<'v, 's> FuncNodeLower<'v, 's> {
    fn poison_node(&mut self) -> Output<nodes::Poison> {
        let node = self.ctx.add_node(|ctx, node| (nodes::Poison {}, []));
        self.ctx.add_output(node)
    }
}
