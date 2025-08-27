#[allow(dead_code)]
pub mod grammar {
    use rust_sitter::{Rule, Spanned};
    #[derive(Debug, Rule)]
    #[language]
    #[extras(re(r"\s"))]
    pub enum Repetitions {
        List(NumberList),
        ListRep1(NumberListRep1),
        ListNoSep(NoSepNumberList),
    }

    #[derive(Debug, Rule)]
    pub struct NumberList {
        #[text("list")]
        _list: (),
        #[sep_by(",")]
        #[leaf(Number)]
        numbers: Spanned<Vec<Spanned<Option<i32>>>>,
    }

    #[derive(Debug, Rule)]
    pub struct NumberListRep1 {
        #[text("list1")]
        _list: (),
        #[repeat1]
        #[leaf(Number)]
        numbers: Spanned<Vec<Spanned<i32>>>,
    }

    #[derive(Debug, Rule)]
    pub struct NoSepNumberList {
        #[text("list2")]
        _list: (),
        #[leaf(Number)]
        numbers: Spanned<Vec<Spanned<i32>>>,
    }

    #[derive(Debug, Rule)]
    #[leaf(pattern(r"\d+"))]
    pub struct Number;
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
        insta::assert_debug_snapshot!(grammar::Repetitions::parse("list"));
        insta::assert_debug_snapshot!(grammar::Repetitions::parse("list 1"));
        insta::assert_debug_snapshot!(grammar::Repetitions::parse("list 1, 2"));
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
