use super::*;
use itertools::Itertools;

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
            panic!("{:?}", err);
        }

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

        if !parser.errors.is_empty() {
            panic!("abording due to parser errors");
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

test!(
    fold_declaration,
    "fn fold as (fn b a -> b) b [a] -> b = 0",
    Parser::everything
);
