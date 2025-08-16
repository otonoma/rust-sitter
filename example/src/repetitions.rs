pub mod grammar {
    use rust_sitter::{Rule, Spanned};

    #[derive(Debug, Rule)]
    #[language]
    #[allow(dead_code)]
    pub struct NumberList {
        #[sep_by1(",")]
        #[leaf(pattern(r"\d+"))]
        numbers: Spanned<Vec<Spanned<i32>>>,
    }

    #[derive(Rule)]
    #[extra]
    struct Whitespace {
        #[leaf(pattern(r"\s"))]
        _whitespace: (),
    }
}

// TODO: Currently not allowed, needs to be fixed.
// pub mod grammar2 {
//     use rust_sitter::{Rule, Spanned};
// 
//     #[derive(Debug, Rule)]
//     #[language]
//     #[allow(dead_code)]
//     pub struct NumberList {
//         #[leaf(pattern(r"\d+"))]
//         numbers: Spanned<Vec<Spanned<i32>>>,
//     }
// 
//     #[derive(Rule)]
//     #[extra]
//     struct Whitespace {
//         #[leaf(pattern(r"\s"))]
//         _whitespace: (),
//     }
// }
// 
// pub mod grammar3 {
//     use rust_sitter::{Rule, Spanned};
// 
//     #[derive(Debug, Rule)]
//     #[language]
//     #[allow(dead_code)]
//     pub struct NumberList {
//         #[sep_by(",")]
//         #[leaf(pattern(r"\d+"))]
//         numbers: Spanned<Vec<Spanned<Option<i32>>>>,
//         #[skip(123)]
//         metadata: u32,
//     }
// 
//     #[derive(Rule)]
//     #[extra]
//     struct Whitespace {
//         #[leaf(pattern(r"\s"))]
//         _whitespace: (),
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;
    use rust_sitter::Language;

    #[test]
    fn repetitions_grammar() {
        // Bug in latest tree-sitter: empty parse on a top-level repeat1 segfaults.
        insta::assert_debug_snapshot!(grammar::NumberList::parse(""));
        insta::assert_debug_snapshot!(grammar::NumberList::parse("1"));
        insta::assert_debug_snapshot!(grammar::NumberList::parse("1, 2"));
    }

    // #[test]
    // fn repetitions_grammar2() {
    //     insta::assert_debug_snapshot!(grammar2::parse(""));
    //     insta::assert_debug_snapshot!(grammar2::parse("1"));
    //     insta::assert_debug_snapshot!(grammar2::parse("1 2"));
    // }

    // #[test]
    // fn repetitions_grammar3() {
    //     insta::assert_debug_snapshot!(grammar3::parse(""));
    //     insta::assert_debug_snapshot!(grammar3::parse("1,"));
    //     insta::assert_debug_snapshot!(grammar3::parse("1, 2"));
    //     insta::assert_debug_snapshot!(grammar3::parse("1,, 2"));
    //     insta::assert_debug_snapshot!(grammar3::parse("1,, 2,"));
    // }
}
