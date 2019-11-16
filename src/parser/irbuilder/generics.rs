use crate::parser::{
    FunctionBuilder, Inlined, ParseError, ParseFault, RawToken, Token, Type, PRELUDE_FID,
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Generics {
    inner: Vec<Type>,
}
impl Generics {
    pub fn decoded(&self, t: &Type) -> Result<Type, ParseFault> {
        match t {
            Type::Generic(n) => match self.inner.get(*n as usize) {
                Some(t) => Ok(t.clone()),
                None => Err(ParseFault::CannotInferType((n + 97) as char)),
            },
            Type::List(box inner) => Ok(Type::List(Box::new(self.decoded(inner)?))),
            _ => Ok(t.clone()),
        }
    }
    pub fn new() -> Self {
        Self::with_capacity(0)
    }
    pub fn with_capacity(cap: usize) -> Self {
        Generics {
            inner: Vec::with_capacity(cap),
        }
    }
    pub fn empty() -> Self {
        Self::new()
    }
    pub fn has_generics(&self) -> bool {
        !self.inner.is_empty()
    }

    pub fn replace_all(&self, func: &mut FunctionBuilder) {
        for param in func.parameter_types.iter_mut() {
            if let Type::Generic(genid) = param {
                *param = self.inner[*genid as usize].clone();
            }
        }
        if let Type::Generic(genid) = func.returns {
            func.returns = self.inner[genid as usize].clone();
        }
    }
}

// TODO: Generic search doesn't work with lists.
pub fn generic_search<'a>(
    from: &'a HashMap<Vec<Type>, usize>,
    find: &[Type],
) -> Option<(usize, Generics)> {
    let mut matches: Vec<(&'a [Type], usize, HashMap<u8, Type>)> = Vec::new();
    'variants: for (params, funcid) in from.iter() {
        if find.len() != params.len() {
            continue;
        }
        let mut generic_map: HashMap<u8, Type> = HashMap::new();
        for (i, param) in params.iter().enumerate() {
            // Type match
            if *param == find[i] {
                continue;
            }

            // Generic match
            if let Some((infered, gen)) = generic_match(param, &find[i]) {
                if let Some(existing_generic) = generic_map.get(&gen) {
                    if *existing_generic != find[i] {
                        continue 'variants;
                    }
                } else {
                    generic_map.insert(gen, infered);
                }
                continue;
            }
            // This one isn't a match. So lets continue with the other variants
            continue 'variants;
        }
        // This variant is compatible!
        matches.push((params, *funcid, generic_map));
    }
    if matches.is_empty() {
        return None;
    }

    Some(get_match_highest_order(matches))
}

fn generic_match(first: &Type, second: &Type) -> Option<(Type, u8)> {
    // Type is the infered type, u8 is the generic
    match first {
        Type::Generic(n) => Some((second.clone(), *n)),
        Type::List(box inner) => {
            if let Type::List(box second_inner) = second {
                generic_match(inner, second_inner)
            } else {
                None
            }
        }
        _ => None,
    }
}

// Gets the variant with the least generics
fn get_match_highest_order(
    mut from: Vec<(&[Type], usize, HashMap<u8, Type>)>,
) -> (usize, Generics) {
    from.sort_by(|a, b| b.2.len().partial_cmp(&a.2.len()).unwrap());
    let selected = from.first().unwrap();
    let funcid = selected.1;
    let mut list = selected
        .2
        .iter()
        .map(|(gen_n, _)| *gen_n)
        .collect::<Vec<u8>>();
    list.sort();
    (
        funcid,
        Generics {
            inner: list
                .iter()
                .map(|n| selected.2[n].clone())
                .collect::<Vec<Type>>(),
        },
    )
}
