use super::Index;
use crate::error::Leaf;
use crate::identifier::r#type::Type;
use crate::parser::error::ParseError;
use crate::parser::prelude;
use crate::parser::tokenizer::token::Token;
use crate::parser::tokenizer::Tracked;
use std::collections::HashMap;
use std::path::Path;

pub struct Translator<'a, 'k> {
    parent_func: &'a str,
    index: &'a mut Index,
    values: HashMap<&'k str, ValueSource>,
    values_len: ValueLengths,
}

pub enum ValueSource {
    Parameter(usize, Type),
    Lambda(usize),
    Where(usize),
}

struct ValueLengths {
    p: usize,
    l: usize,
    w: usize,
}

impl<'a, 'k> Translator<'a, 'k> {
    pub fn new(index: &'a mut Index, parent_func: &'a str) -> Self {
        Self {
            parent_func,
            index,
            values: HashMap::new(),
            values_len: ValueLengths { p: 0, l: 0, w: 0 },
        }
    }

    pub fn tag_param(&mut self, k: &'k str, t: Type) {
        self.values
            .insert(k, ValueSource::Parameter(self.values_len.p, t));
        self.values_len.p += 1;
    }
    pub fn tag_lambda(&mut self, k: &'k str) {
        self.values
            .insert(k, ValueSource::Lambda(self.values_len.l));
        self.values_len.l += 1;
    }
    pub fn tag_where(&mut self, k: &'k str) {
        self.values.insert(k, ValueSource::Where(self.values_len.w));
        self.values_len.w += 1;
    }

    pub fn translate_buf(
        &mut self,
        self_mod: &Path,
        tbuf: &mut Vec<Tracked<Token>>,
    ) -> Result<(), ParseError> {
        for mut t in tbuf.iter_mut() {
            use ValueSource::*;
            match &mut t.inner {
                Token::Word(w) => match self.values.get(w.as_str()) {
                    Some(Parameter(id, _type)) => {
                        t.inner = Token::ParamValue(*id);
                        continue;
                    }
                    Some(Lambda(id)) => {
                        t.inner = Token::LambdaValue(*id);
                        continue;
                    }
                    Some(Where(id)) => {
                        t.inner = Token::WhereValue(*id);
                        continue;
                    }
                    None => {
                        let mut spl = w.splitn(2, ':');
                        let first = spl.next().unwrap();
                        let (self_fid, a) = self.index.try_get_file(self_mod).unwrap();
                        let self_module = a.borrow();
                        match spl.next() {
                            None => {
                                // Is it a local function?
                                match self_module.try_get_func(first) {
                                    Some(funcid) => {
                                        t.inner = if first == self.parent_func {
                                            Token::Recurse
                                        } else {
                                            Token::Function(self_fid, funcid)
                                        }
                                    }
                                    None => {
                                        // Must be prelude then
                                        let a =
                                            self.index.get_file(Path::new(prelude::LEAF_PRIM_FILE));
                                        let prelude_mod = a.borrow();
                                        t.inner = match prelude_mod.try_get_func(first) {
                                            Some(funcid) => {
                                                Token::Function(prelude::LEAF_PRIM_FID, funcid)
                                            }
                                            None => {
                                                return Err(ParseError::new(
                                                    self_mod.to_owned(),
                                                    Leaf::FuncNotFound(first.to_owned()),
                                                )
                                                .with_linen(t.position))
                                            }
                                        }
                                    }
                                }
                            }
                            Some(second) => {
                                // Unique handling for the rust bridge
                                if first == "rust" {
                                    if second.get(0..4) == Some("call") {
                                        let bfid = &second[4..].parse().map_err(|_| {
                                            ParseError::new(
                                                self_mod.to_owned(),
                                                Leaf::RustCallNonum,
                                            )
                                            .with_linen(t.position)
                                        })?;
                                        t.inner = Token::BridgedFunction(*bfid);
                                    } else {
                                        return Err(ParseError::new(
                                            self_mod.to_owned(),
                                            Leaf::RustCallInvUse(
                                                first.to_owned(),
                                                second.to_owned(),
                                            ),
                                        )
                                        .with_linen(t.position));
                                    }
                                } else {
                                    let path_mod = match self_module
                                        .get_import(&std::ffi::OsStr::new(first))
                                    {
                                        None => {
                                            return Err(ParseError::new(
                                                self_mod.to_owned(),
                                                Leaf::ModuleNotFound(first.to_owned()),
                                            )
                                            .with_linen(t.position))
                                        }
                                        Some(m) => m,
                                    };
                                    let (fid, tagger) = self.index.try_get_file(path_mod).unwrap();
                                    let funcid = tagger.borrow().get_func(second);
                                    t.inner = Token::Function(fid, funcid);
                                }
                            }
                        };
                    }
                },
                Token::TrackedRuntimeList(tbuf) => self.translate_buf(self_mod, tbuf)?,
                Token::TrackedGroup(tbuf) => self.translate_buf(self_mod, tbuf)?,
                Token::TrackedIfStatement(branches, else_do_buf) => {
                    self.translate_buf(self_mod, else_do_buf)?;
                    for branch in branches {
                        self.translate_buf(self_mod, &mut branch.0)?;
                        self.translate_buf(self_mod, &mut branch.1)?;
                    }
                }
                Token::Lambda(_tbuf) => {
                    return Err(ParseError::new(
                        self_mod.to_owned(),
                        Leaf::Unimplemented(String::from("Lambdas")),
                    )
                    .with_linen(t.position))
                }
                _ => {}
            }
        }
        Ok(())
    }
}
