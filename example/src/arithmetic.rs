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
        Let(LetExpression),
        Complex(ComplexExpression),
        Print(PrintExpression),
    }

    #[derive(PartialEq, Eq, Debug, Rule)]
    pub struct LetExpression {
        #[text("let")]
        _let: (),
        pub var: Ident,
        #[text("=")]
        _eq: (),
        pub val: Box<Expression>,
    }

    #[derive(PartialEq, Eq, Debug, Rule)]
    pub enum LogLevel {
        #[leaf("info")]
        Info,
        #[leaf("debug")]
        Debug,
        #[leaf("trace")]
        Trace,
        Custom(CustomLevel),
    }

    #[derive(PartialEq, Eq, Debug, Rule)]
    pub enum Other {
        #[leaf("info")]
        Info,
        #[leaf("debug")]
        Debug,
        #[leaf("trace")]
        Trace,
    }

    #[derive(PartialEq, Eq, Debug, Rule)]
    pub struct CustomLevel {
        #[text("custom")]
        _custom: (),
        #[text("::")]
        _co: (),
        pub value: Other,
    }

    #[derive(PartialEq, Eq, Debug, Rule)]
    pub struct ComplexExpression {
        #[text("log")]
        _log: (),
        #[leaf("optional")]
        optional: Option<()>,
        pub level: LogLevel,
        #[leaf(seq("(", optional(Expression), ")"))]
        pub ex: Option<((), Option<Box<Expression>>, ())>,
        // #[leaf(seq(LogLevel, "(", optional(Expression), ")"))]
        // pub ident_ex: Option<(LogLevel, (), Option<Box<Expression>>, ())>,
        #[leaf(";")]
        _semi: Option<()>,
    }

    #[derive(PartialEq, Eq, Debug, Rule)]
    pub struct PrintExpression {
        #[text("print")]
        _print: (),
        #[text("(")]
        _lparen: (),
        #[sep_by(",")]
        inputs: Vec<Expression>,
        #[text(")")]
        _rparen: (),
    }

    #[derive(PartialEq, Eq, Debug, Rule)]
    pub struct Ident(#[leaf(re(r"[a-zA-Z_][a-zA-Z_0-9]*"))] String);
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
            grammar::Expression::parse("1 - 2 - 3")
                .into_result()
                .unwrap(),
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
            grammar::Expression::parse("1 - 2 * 3")
                .into_result()
                .unwrap(),
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
            grammar::Expression::parse("1 * 2 * 3")
                .into_result()
                .unwrap(),
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
            grammar::Expression::parse("1 * 2 - 3")
                .into_result()
                .unwrap(),
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
