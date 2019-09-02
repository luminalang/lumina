use crate::identifier::r#type::{BaseType, Type};
use crate::parser::{list, prelude};
use std::convert::TryFrom;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Float(f32),
    Byte(u8),
    List(Vec<Value>),
    Struct(Type, Vec<Value>),
    Nothing,
}

impl Value {
    #[inline]
    pub fn take(&mut self) -> Value {
        std::mem::replace(self, Value::Nothing)
    }

    pub fn pretty_print(&self) -> String {
        match self {
            Value::Int(n) => n.to_string(),
            Value::Float(n) => n.to_string(),
            Value::Bool(n) => if *n { "true" } else { "false" }.to_owned(),
            Value::Byte(b) => b.to_string(),
            Value::List(values) => {
                let mut buf = String::from("[");
                for v in values.iter() {
                    if buf.len() != 1 {
                        buf.push_str(&", ");
                    }
                    buf.push_str(&v.pretty_print());
                }
                buf.push(']');
                buf
            }
            Value::Struct(_, _) => String::from("<type>"), // TODO
            Value::Nothing => "_".to_owned(),
        }
    }
}

#[macro_export]
macro_rules! assume {
    ($source:expr, $want:path) => {
        if let $want(v) = $source {
            v
        } else {
            panic!(
                "You lied to me, this is a broken assumption. I got {:?}",
                $source
            )
        }
    };
}

impl TryFrom<&[u8]> for Value {
    type Error = ();

    fn try_from(slice: &[u8]) -> Result<Value, Self::Error> {
        match slice.get(0) {
            Some(b'\'') => {
                if slice[1] == b'\\' && slice[2] == b'n' {
                    /*
                    if slice[3] != b'\'' {
                        return Err(()); // TODO: Handle
                    }
                    */
                    return Ok(Value::Byte(b'\n'));
                }

                if slice[2] != b'\'' {
                    return Err(()); // TODO: Handle
                }
                return Ok(Value::Byte(slice[1]));
            }
            Some(b'[') => {
                if *slice.last().unwrap() != b']' {
                    panic!("List missing ending");
                }

                let mut list_buf: Vec<Value> = Vec::new();
                let mut of_type: Option<Type> = None;

                let append = |entity: &[u8]| {
                    let value = match Value::try_from(entity) {
                        // Oh ok this list contains inlined code. We'll handle this list later then
                        Err(_) => return Err(()),
                        Ok(v) => v,
                    };
                    match &of_type {
                        None => of_type = Some((&value).into()),
                        Some(t) => {
                            let got: Type = (&value).into();
                            if got != *t {
                                panic!("ERROR_TODO: List contains mixed types");
                            }
                        }
                    };
                    list_buf.push(value);
                    Ok(())
                };

                list::build_list(slice, append)?;

                let final_list = Ok(Value::List(list_buf));
                return final_list;
            }
            Some(b'"') => {
                if slice.last().copied() != Some(b'"') {
                    return Err(()); // TODO: Handle
                }
                let mut leaf_str: Vec<Value> = Vec::with_capacity(slice.len() - 2);

                let mut iter = slice[1..slice.len() - 1].iter();
                loop {
                    let c = match iter.next() {
                        None => break,
                        Some(c) => *c,
                    };
                    if c == b'\\' {
                        let c2 = match iter.next() {
                            None => break,
                            Some(c) => *c,
                        };
                        // Escape codes
                        let esc = match c2 {
                            b'n' => b'\n',
                            _ => {
                                leaf_str.push(Value::Byte(c));
                                leaf_str.push(Value::Byte(c2));
                                continue;
                            }
                        };
                        leaf_str.push(Value::Byte(esc));
                    } else {
                        leaf_str.push(Value::Byte(c));
                    }
                }

                return Ok(Value::Struct(
                    Type::Custom(prelude::LEAF_PRIM_FID, prelude::STRING_ID),
                    vec![Value::List(leaf_str)],
                ));
            }
            None => return Err(()),
            _ => {}
        }
        let chars = String::from_utf8(slice.to_vec()).unwrap();
        if let Ok(n) = chars.parse::<i32>() {
            return Ok(Value::Int(n));
        }
        if let Ok(n) = chars.parse::<f32>() {
            return Ok(Value::Float(n));
        }
        match chars.as_str() {
            "true" => return Ok(Value::Bool(true)),
            "false" => return Ok(Value::Bool(false)),
            _ => {}
        }

        // Alright nothing we hit it with worked. Can't be an inlined value
        Err(())
    }
}

impl<'t> Into<Type> for &Value {
    fn into(self) -> Type {
        match self {
            Value::Int(_) => Type::Base(BaseType::Int),
            Value::Float(_) => Type::Base(BaseType::Float),
            Value::Bool(_) => Type::Base(BaseType::Bool),
            Value::Byte(_) => Type::Base(BaseType::Byte),
            Value::Nothing => Type::Base(BaseType::Nothing),
            Value::List(list) => Type::List(Box::new(
                list.get(0)
                    .and_then(|v| {
                        let t: Type = v.into();
                        Some(t)
                    })
                    .unwrap_or(Type::Base(BaseType::Nothing)),
            )),
            Value::Struct(of_t, _) => of_t.clone(),
        }
    }
}

#[derive(Debug)]
pub struct CustomType {
    pub fields: Vec<Type>,
}
