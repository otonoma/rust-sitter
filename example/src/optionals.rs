#[allow(dead_code)]
mod grammar {
    use rust_sitter::Spanned;
    use rust_sitter::Rule;

    #[derive(Debug, Rule)]
    #[language]
    pub struct Language {
        #[leaf(re(r"\d+"))]
        // Not necessary, done automatically.
        // #[rust_sitter::with(|v| v.parse().unwrap())]
        v: Option<i32>,
        #[leaf("_")]
        _s: (),
        t: Spanned<Option<Number>>,
        #[leaf(".")]
        _d: Option<()>,
    }

    #[derive(Debug, Rule)]
    pub struct Number {
        #[leaf(re(r"\d+"))]
        // TODO: We are replacing this entirely with a different defintion.
        // #[with(|v| v.parse().unwrap())]
        v: i32,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rust_sitter::Language;

    #[test]
    fn optional_grammar() {
        insta::assert_debug_snapshot!(grammar::Language::parse("_"));
        insta::assert_debug_snapshot!(grammar::Language::parse("_."));
        insta::assert_debug_snapshot!(grammar::Language::parse("1_"));
        insta::assert_debug_snapshot!(grammar::Language::parse("1_."));
        insta::assert_debug_snapshot!(grammar::Language::parse("1_2"));
        insta::assert_debug_snapshot!(grammar::Language::parse("1_2."));
        insta::assert_debug_snapshot!(grammar::Language::parse("_2"));
        insta::assert_debug_snapshot!(grammar::Language::parse("_2."));
    }
}
