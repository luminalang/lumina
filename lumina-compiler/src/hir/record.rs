//! Records have extra syntactic sugar that's trickier to deal with. Here we try to desugar the
//! difference agnostically across expressions and patterns.

use lumina_parser as parser;
use lumina_util::Tr;

// Intermediate data structure to desugar `a.b.c` syntax
pub struct Field<'s, T> {
    pub name: Tr<&'s str>,
    pub value: FieldValue<'s, T>,
}

pub enum FieldValue<'s, T> {
    Record(Vec<Field<'s, T>>),
    Tail(T),
    Punned,
}

pub fn from_parsed<'e, 's, T>(fields: &'e [parser::Field<'s, T>]) -> Vec<Field<'s, Tr<&'e T>>> {
    fields
        .iter()
        .map(|f| field(&f.field_names, f.bind, f.value.as_ref().map(|v| v.as_ref())))
        .collect()
}

fn field<'s, T>(
    names: &[Tr<&'s str>],
    bind: Option<Tr<&'s str>>,
    value: Option<T>,
) -> Field<'s, T> {
    match names {
        &[name] => Field {
            name,
            value: match value {
                Some(value) => FieldValue::Tail(value),
                None => FieldValue::Punned,
            },
        },
        [name, xs @ ..] => Field {
            name: *name,
            value: FieldValue::Record(vec![field(xs, bind, value)]),
        },
        [] => unreachable!(),
    }
}
