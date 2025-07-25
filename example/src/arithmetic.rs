pub mod grammar {
    use rust_sitter::Rule;
    #[derive(PartialEq, Eq, Debug, Rule)]
    #[language]
    pub enum Expression {
        Number(#[leaf(pattern(r"\d+"))] i32),
        #[prec_left(1)]
        Sub(
            Box<Expression>,
            #[leaf("-")] (),
            Box<Expression>,
        ),
        #[prec_left(2)]
        Mul(
            Box<Expression>,
            #[leaf("*")] (),
            Box<Expression>,
        ),
    }

    #[derive(Rule)]
    #[extra]
    struct Whitespace {
        #[leaf(pattern(r"\s"))]
        _whitespace: (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use grammar::Expression;

    #[wasm_bindgen_test::wasm_bindgen_test]
    #[test]
    fn successful_parses() {
        assert_eq!(grammar::Expression::parse("1").unwrap(), Expression::Number(1));

        assert_eq!(grammar::Expression::parse(" 1").unwrap(), Expression::Number(1));

        assert_eq!(
            grammar::Expression::parse("1 - 2").unwrap(),
            Expression::Sub(
                Box::new(Expression::Number(1)),
                (),
                Box::new(Expression::Number(2))
            )
        );

        assert_eq!(
            grammar::Expression::parse("1 - 2 - 3").unwrap(),
            Expression::Sub(
                Box::new(Expression::Sub(
                    Box::new(Expression::Number(1)),
                    (),
                    Box::new(Expression::Number(2))
                )),
                (),
                Box::new(Expression::Number(3))
            )
        );

        assert_eq!(
            grammar::Expression::parse("1 - 2 * 3").unwrap(),
            Expression::Sub(
                Box::new(Expression::Number(1)),
                (),
                Box::new(Expression::Mul(
                    Box::new(Expression::Number(2)),
                    (),
                    Box::new(Expression::Number(3))
                ))
            )
        );

        assert_eq!(
            grammar::Expression::parse("1 * 2 * 3").unwrap(),
            Expression::Mul(
                Box::new(Expression::Mul(
                    Box::new(Expression::Number(1)),
                    (),
                    Box::new(Expression::Number(2))
                )),
                (),
                Box::new(Expression::Number(3))
            )
        );

        assert_eq!(
            grammar::Expression::parse("1 * 2 - 3").unwrap(),
            Expression::Sub(
                Box::new(Expression::Mul(
                    Box::new(Expression::Number(1)),
                    (),
                    Box::new(Expression::Number(2))
                )),
                (),
                Box::new(Expression::Number(3))
            )
        );
    }

    #[test]
    fn failed_parses() {
        insta::assert_debug_snapshot!(grammar::Expression::parse("1 + 2"));
        insta::assert_debug_snapshot!(grammar::Expression::parse("1 - 2 -"));
        insta::assert_debug_snapshot!(grammar::Expression::parse("a1"));
        insta::assert_debug_snapshot!(grammar::Expression::parse("1a"));
    }
}
