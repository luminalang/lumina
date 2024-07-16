use super::*;
use insta;

macro_rules! parse {
    ($src:literal, $parse:expr, $skip_first:expr) => {
        lumina_util::enable_highlighting(false);
        let mut parser = Parser::new($src);
        if $skip_first {
            parser.lexer.next();
        }
        let ast = $parse(&mut parser);
        for err in parser.errors.iter() {
            eprintln!("{:?}", err);
        }

        match ast {
            None => {
                panic!("test yielded parser errors");
            }
            Some(ast) => {
                let mut remaining = vec![];
                loop {
                    let (t, _) = parser.lexer.next();
                    if t == Token::EOF {
                        break;
                    }
                    remaining.push(t);
                }

                if !remaining.is_empty() && remaining.iter().any(|t| *t != T::NewLines) {
                    panic!("leftover tokens: {:?}", remaining);
                }

                if !parser.errors.is_empty() {
                    panic!("abording due to parser errors");
                }

                insta::assert_snapshot!({
                    $src;
                    format!("\n{}", ast)
                })
            }
        }
    };
}

macro_rules! test {
    ($name:ident, $src:literal => $parse:expr) => {
        #[test]
        fn $name() {
            parse!($src, $parse, true);
        }
    };
    ($name:ident, $src:literal, $parse:expr) => {
        #[test]
        fn $name() {
            parse!($src, $parse, false);
        }
    };
}

macro_rules! pattern {
    ($name:ident, $src:literal) => {
        test!($name, $src, Parser::pattern_parameter);
    };
}

macro_rules! r#type {
    ($name:ident, $src:literal) => {
        test!($name, $src, Parser::type_with_params);
    };
}

macro_rules! expr {
    ($name:ident, $src:literal) => {
        test!($name, $src, Parser::expr);
    };
}

macro_rules! declaration {
    ($name:ident, $src:literal) => {
        test!($name, $src, |parser: &mut Parser<'static>| parser
            .declaration()
            .map(|(_, decl)| decl));
    };
}

macro_rules! when {
    ($name:ident, $src:literal) => {
        test!($name, $src => Parser::when);
    };
}

pattern!(pat_simple_list, "[1,2, 3]");
pattern!(pat_nested_list, "[[[], 1], [[2], [[3, 4]], 5]]");
pattern!(pat_simple_punned_record, "{x, y}");
pattern!(pat_simple_annotated_record, "{ point int | x = 1, y = 2 }");
pattern!(pat_simple_infered_record, "{ x = 1, y = 2 }");
pattern!(pat_nested_tuple, "(((x, y), (x, y), ()))");
pattern!(pat_string_literal, "\"hello\\\" world\"");
pattern!(pat_simple_constr, "(Just 20)");
pattern!(pat_nested_constr, "(Just (Just (Pair 10 20)))");
pattern!(pat_int_ranges, "(0..10, 0..20, ..30, 30..)");

r#type!(type_ints, "Con int uint i8 i64 u8 u64");
r#type!(type_floats, "Con f32 f64");
r#type!(type_bool, "bool");
r#type!(type_simple_defined, "Con int");
r#type!(type_simple_closure, "fn(a -> b)");
r#type!(type_simple_closure_ret_sugar, "fn(b)");
r#type!(type_fnptr, "fnptr(a -> b)");
r#type!(
    type_nested_higher_order,
    "fn(fnptr(a, b -> c), fn(d -> e) -> fn(f -> fn(g)))"
);
r#type!(type_simple_tuple, "(a, b)");
r#type!(type_nested_tuple, "((((a, b), c)), d)");
r#type!(type_annotated_trait, "std:Iterator(Item as Option int)");
r#type!(type_ptr, "(*string, *fnptr(*int), ****int)");

expr!(expr_simple_list, "[1,2, 3]");
expr!(expr_nested_list, "[[[], 1], [[2], [[3, 4]], 5]]");
expr!(expr_simple_punned_record, "{x, y}");
expr!(expr_simple_annotated_record, "{ point int | x = 1, y = 2 }");
expr!(expr_simple_infered_record, "{ x = 1, y = 2 }");
expr!(expr_nested_tuple, "(((x, y), (x, y), ()))");
expr!(expr_string_literal, "\"hello\\\" world\"");
expr!(expr_simple_constr, "(Just 20)");
expr!(expr_nested_constr, "(Just (Just (Pair 10 20)))");
expr!(expr_simple_lambda, "\\x y -> x + y");
expr!(expr_complex_lambda, "(\\(x, y) {a, b} -> \\ -> 0) 2");
expr!(
    expr_type_annotated_call,
    "Functor(self as Option):map(a as int, b as Option float)"
);
expr!(expr_multiple_accessors, "a.b.c.d");
expr!(expr_multiple_accessors_as_param, "f a.b.c.d 0");

expr!(expr_pipe_lambda, "1 >> (\\n -> 1)");
expr!(expr_pipe_eval_of_lambda, "1 >> ((\\ -> 0))");
expr!(expr_pipe_eval_of_fn, "1 >> (f 0)");
expr!(expr_pipe_fn, "1 >> f 0");
expr!(expr_pipe_left, "f 0 << f 1 << f 2");
expr!(expr_pipes, "1 >> add 2 >> (\\n -> add n << 3)");

expr!(expr_pass_fn, "map #inc");
expr!(expr_pass_partial, "map #(add 1)");
expr!(expr_pass_lambda, "map #(\\n -> n)");
expr!(expr_pass_expr, "map #1");
expr!(expr_pass_opnum, "#(* 2)");
expr!(expr_deep_set, "{ v | one.two.three.four @ four = 0 }");
expr!(expr_deep_update, "{ v | one.two.three.four @ four = four }");
expr!(expr_field_of_access, "{ v | one = one.two.three }");
expr!(
    expr_if,
    "if (if true then true else false) then true else if true then true else false"
);
expr!(
    expr_match,
    "
  match n
  | 0 ->
    true
  | 1 -> false
  | 2 ->
    true
"
);
expr!(
    expr_match_nested,
    "
match (match 0 | 0 -> true | _ -> false)
 | true ->
   match 0
   | 0 -> 0
     | 1 -> 1
    | _ -> 2
| false ->
     match 0 | false -> 0 | true -> 1
"
);

declaration!(decl_option, "type Option a = Just a | None");
declaration!(decl_point_single_line, "type Point a { x a, y a }");
declaration!(
    decl_point_multi_line,
    "
type Point a {
  x a
  y a
}"
);
declaration!(
    decl_point_multi_line_optional_comma,
    "
type Point a {
  x a,
  y a,
}"
);
declaration!(
    decl_from_trait,
    "
trait From a
  fn from as a -> self"
);
declaration!(
    decl_from_trait_optional_eq,
    "
trait From a =
  fn from as a -> self"
);
declaration!(
    decl_iterator,
    "
trait Iterator
  type Item
   
  fn next as self -> (Item, self)
"
);
declaration!(
    decl_map,
    "
fn map f list as fn(a -> b), [a] -> [b] = 
  match list
  | x : xs -> f x : map #f xs
  | []     -> []
"
);
declaration!(
    decl_impl_into,
    "
impl Into a for b
  fn into b as b -> a =
    From:from b
    
"
);
when!(decl_when_single_line, "when a can Show, n can Add n");
when!(
    decl_when_multi_line,
    "
when
  a can Show
  n can Add n
"
);
when!(
    decl_when_multi_line_optional_comma,
    "
when 
  a can Show,
  n can Add n,
"
);
declaration!(decl_use_simple, "use std:io");
declaration!(decl_use_assign, "use std:io input_output");
declaration!(
    decl_use_expose,
    "use std:io [puts, Target [Stdout, Stdin, Stderr]]"
);
declaration!(
    decl_use_assign_then_expose,
    "use std:io input_output [puts, Target [Stdout, Stdin], Target [..]]"
);
declaration!(
    decl_use_with_annotation,
    "use ext:lumina:parser(Span as MySpan int, Output as This)"
);
declaration!(decl_val, "val LineBuffering = 0");
declaration!(decl_val_annotated, "val LineBuffering as *string = 0");
