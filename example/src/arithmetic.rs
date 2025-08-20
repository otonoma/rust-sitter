pub mod grammar {
    use rust_sitter::Rule;
    #[derive(PartialEq, Eq, Debug, Rule)]
    #[language]
    #[extras(
        // whitespace
        re(r"\s")
    )]
    pub enum Expression {
        Number(#[leaf(pattern(r"\d+"))] i32),
        #[prec_left(1)]
        Sub(Box<Expression>, #[leaf("-")] (), Box<Expression>),
        #[prec_left(2)]
        Mul(Box<Expression>, #[leaf("*")] (), Box<Expression>),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use grammar::Expression;
    use rust_sitter::Language;

    #[wasm_bindgen_test::wasm_bindgen_test]
    #[test]
    fn successful_parses() {
        assert_eq!(
            grammar::Expression::parse("1").into_result().unwrap(),
            Expression::Number(1)
        );

        assert_eq!(
            grammar::Expression::parse(" 1").into_result().unwrap(),
            Expression::Number(1)
        );

        assert_eq!(
            grammar::Expression::parse("1 - 2").into_result().unwrap(),
            Expression::Sub(
                Box::new(Expression::Number(1)),
                (),
                Box::new(Expression::Number(2))
            )
        );

        assert_eq!(
            grammar::Expression::parse("1 - 2 - 3").into_result().unwrap(),
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
            grammar::Expression::parse("1 - 2 * 3").into_result().unwrap(),
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
            grammar::Expression::parse("1 * 2 * 3").into_result().unwrap(),
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
            grammar::Expression::parse("1 * 2 - 3").into_result().unwrap(),
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
