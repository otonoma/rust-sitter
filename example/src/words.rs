pub mod grammar {
    use rust_sitter::Rule;

    #[derive(Debug, Rule)]
    #[language]
    #[extras(
        re(r"\s")
    )]
    #[allow(dead_code)]
    pub struct Words {
        #[leaf("if")]
        keyword: (),
        #[word]
        #[leaf(pattern(r"[a-z_]+"))]
        word: String,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rust_sitter::Language;

    #[test]
    fn words_grammar() {
        insta::assert_debug_snapshot!(grammar::Words::parse("if"));
        insta::assert_debug_snapshot!(grammar::Words::parse("hello"));
        insta::assert_debug_snapshot!(grammar::Words::parse("ifhello"));
        insta::assert_debug_snapshot!(grammar::Words::parse("if hello"));
    }
}
