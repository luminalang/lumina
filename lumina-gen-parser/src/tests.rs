use super::*;
use itertools::Itertools;
use tracing::info;

use insta;

macro_rules! parse {
    ($src:literal, $parse:expr, $skip_first:expr) => {
        lumina_util::test_logger();
        lumina_util::enable_highlighting(false);
        info!("parasing: {}", $src);
        let mut parser = Parser::new($src);
        if $skip_first {
            parser.lexer.next();
        }
        let ast = $parse(&mut parser);

        let mut remaining = vec![];
        loop {
            let (t, _) = parser.lexer.next();
            if t == Token::EOF {
                break;
            }
            remaining.push(t);
        }

        if !remaining.is_empty() && remaining.iter().any(|t| *t != Token::NewLines) {
            panic!("leftover tokens: {:?}", remaining);
        }

        let formatted = ast.into_iter().format("\n\n");

        insta::assert_snapshot!({
            $src;
            format!("\n{}", formatted)
        });
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

test!(operator_precedence, "a + b * c * d + e", Parser::everything);
test!(
    delim_precedence,
    "a = match b | p0 -> e0 * e1 . c | p1 -> e1 + e1 . c",
    Parser::everything
);

test!(
    fold_declaration,
    "fn fold f acc list as (fn b a -> b) b [a] -> b =
        match list
        | [x : xs] -> fold #f (f x acc) xs\n| [] -> []",
    Parser::everything
);
