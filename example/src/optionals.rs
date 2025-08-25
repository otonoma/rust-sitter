#[allow(dead_code)]
pub mod grammar {
    use rust_sitter::Rule;
    use rust_sitter::Spanned;

    #[derive(Debug, Rule)]
    #[language]
    pub struct Language {
        v: Option<Number>,
        #[leaf("_")]
        _s: (),
        t: Spanned<Option<Number>>,
        #[leaf(".")]
        _d: Option<()>,
    }

    #[derive(Debug, Rule)]
    pub struct Number {
        #[leaf(re(r"\d+"))]
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
