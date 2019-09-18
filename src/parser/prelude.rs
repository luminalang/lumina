use super::index::Index;
use super::tokenizer::{token::Token, Tokenizer};
use super::FunctionBuilder;
use super::ParseModule;
use crate::evaler::runner::{rustbridge, Entity};
use crate::identifier::r#type::CustomType;
use crate::identifier::r#type::{BaseType, Type};
use std::path::Path;

pub const LEAF_PRIM_FILE: &str = "|!|";
pub const LEAF_PRIM_FID: usize = 0;

pub const STRING_ID: usize = 0;
fn string_type() -> CustomType {
    CustomType {
        name: String::from("string"),
        fields: vec![(
            String::from("inner"),
            Type::List(Box::new(Type::Base(BaseType::Byte))),
        )],
    }
}

pub fn include_all(index: &mut Index) -> ParseModule {
    let (fid, tagger) = index.get_file_or_create(Path::new(LEAF_PRIM_FILE));

    let mut prim_mod = ParseModule::with_fid(fid, Tokenizer::new(Vec::new()));

    let mut tag = tagger.borrow_mut();
    tag.set_type(
        "string",
        vec![(
            String::from("inner"),
            Type::List(Box::new(Type::Base(BaseType::Byte))),
        )],
    );
    prim_mod.types.push(string_type());

    let funcb = rust_str();
    tag.set_func(funcb.header.clone());
    prim_mod.functions.push(funcb);

    prim_mod
}

fn rust_str() -> FunctionBuilder {
    FunctionBuilder::from_raw_ir(
        LEAF_PRIM_FILE,
        "str",
        vec![Token::Finished(Entity::BridgedFunction(
            rustbridge::TO_STRING,
            vec![Entity::ParamValue(0)],
        ))],
        Type::Custom(LEAF_PRIM_FID, STRING_ID, None),
        vec![Type::Generic(0)],
    )
}

/*
fn prelude_raw<'a>() -> &'a str {
    "
    type string
        inner :: list<byte>

    fn new_str from (a -> string)
        rust:call2 from
    "
}
*/
