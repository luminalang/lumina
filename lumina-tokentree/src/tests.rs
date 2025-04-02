use super::*;
use insta;

macro_rules! tree {
    ($name:ident, $src:literal) => {
        #[test]
        fn $name() {
            lumina_util::test_logger();
            let mut parser = Parser::new($src);
            let entity = parser.next(false);
            insta::assert_snapshot!(entity);
        }
    };
}

tree!(precedence, "1 = 2, 3 . 4 + 4 + 5 * 5, 6 + 6");
tree!(
    nested_match,
    "match 0
| outer0 -> r0
| outer1 ->
  match 1
  | inner0 -> 10
  | inner1 -> 11
| outer2 ->
  match 2
  | inner0 -> 20
  | inner1 -> 21
"
);
