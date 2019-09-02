use super::index::Index;
use super::tokenizer::token::Token;
use super::FunctionBuilder;
use crate::evaler::function::Function;
use crate::evaler::module;
use crate::evaler::r#type::{CustomType, Value};
use crate::evaler::runner::rustbridge;
use crate::identifier::r#type::{BaseType, Type};
use std::path::Path;

pub const LEAF_PRIM_FILE: &str = "|!|";
pub const LEAF_PRIM_FID: usize = 0;

pub const STRING_ID: usize = 0;
fn string_type() -> CustomType {
    CustomType {
        fields: vec![Type::List(Box::new(Type::Base(BaseType::Byte)))],
    }
}

pub fn include_all(index: &mut Index) {
    let (fid, tagger) = index.get_file_or_create(Path::new(LEAF_PRIM_FILE));

    let mut prim_mod = module::Module {
        fileid: fid,
        types: Vec::with_capacity(1),
        functions: Vec::with_capacity(1),
    };

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
    prim_mod.functions.push(funcb.into());

    unsafe {
        module::push_module(prim_mod);
    };
}

fn rust_str<'a>() -> FunctionBuilder<'a> {
    FunctionBuilder::from_raw_ir(
        LEAF_PRIM_FILE,
        "str",
        vec![
            Token::BridgedFunction(rustbridge::TO_STRING),
            Token::ParamValue(0),
        ],
        Type::Custom(LEAF_PRIM_FID, STRING_ID),
        vec![Type::Generic(0)],
    )
}
