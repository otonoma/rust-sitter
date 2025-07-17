pub mod grammar {
    use rust_sitter::Rule;

    #[derive(Debug, Rule)]
    #[language]
    #[allow(dead_code)]
    pub struct Words {
        #[leaf("if")]
        keyword: (),
        #[word]
        #[leaf(pattern(r"[a-z_]+"))]
        word: String,
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

    #[test]
    fn words_grammar() {
        insta::assert_debug_snapshot!(grammar::Words::parse("if"));
        insta::assert_debug_snapshot!(grammar::Words::parse("hello"));
        insta::assert_debug_snapshot!(grammar::Words::parse("ifhello"));
        insta::assert_debug_snapshot!(grammar::Words::parse("if hello"));
    }
}
