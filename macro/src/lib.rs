use syn::{DeriveInput, parse_macro_input};

mod errors;
mod expansion;
// mod grammar;
use expansion::*;

// // TODO: Make a direct grammar function...
// This would allow us to write something like:
// struct Function {
//      name: String,
//      inputs: Vec<Input>,
// }
// grammar! {
//  rule: seq("function", $.ident, "(", repeat($.input), ")") -> |id, inputs| Function { name,
//  inputs: inputs.into() };
//
//  ident: /re/;
//  input: seq($.ident, ":", $.ident);
//
// }
// #[proc_macro]
// pub fn grammar(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//     grammar::parse_grammar_macro(input)
// }

#[proc_macro_derive(
    Rule,
    // Alternatively, we can instead have one helper like `tree(...)` - generally looks cleaner.
    attributes(
        // Helper
        language,
        word,
        leaf,
        text,
        prec,
        prec_left,
        prec_right,
        prec_dynamic,
        // TODO: This will instead be on a derive(Language) as well as others like conflicts,
        // externals, inline, word, supertypes, etc. to fill out the full grammar specification.
        extras,
        with,
        transform,
        sep_by,
        // Helper!
        sep_by1,
        repeat1,
        skip,
    )
)]
pub fn derive_rule(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand_rule(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{Read, Write};
    use std::process::Command;

    use quote::ToTokens;
    use syn::{ItemMod, Result, parse_quote};
    use tempfile::tempdir;

    use crate::expand_rule;

    // Allows expanding multiple rules at once.
    fn expand_grammar(input: ItemMod) -> ItemMod {
        let (_, items) = input.content.unwrap();
        let mut output = vec![];
        for item in items {
            let stream = item.to_token_stream();
            // This might not actually work...
            if let Ok(parsed) = syn::parse2(stream.clone()) {
                let result = expand_rule(parsed).unwrap();
                output.push(result);
            } else {
                output.push(stream);
            }
        }
        let mod_name = input.ident;

        parse_quote! {
            mod #mod_name {
                #(#output)*
            }
        }
    }
    fn rustfmt_code(code: &str) -> String {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("temp.rs");
        let mut file = File::create(file_path.clone()).unwrap();

        writeln!(file, "{code}").unwrap();
        drop(file);

        Command::new("rustfmt")
            .arg(file_path.to_str().unwrap())
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        let mut file = File::open(file_path).unwrap();
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();
        drop(file);
        dir.close().unwrap();
        data
    }

    #[test]
    fn enum_transformed_fields() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    use rust_sitter::Rule;
                    #[derive(Rule)]
                    #[language]
                    pub enum Expression {
                        Number(
                            #[leaf(re(r"\d+"))]
                            i32
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn enum_recursive() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    #[derive(rust_sitter::Rule)]
                    #[language]
                    pub enum Expression {
                        Number(
                            #[leaf(re(r"\d+"))]
                            i32
                        ),
                        Neg(
                            #[leaf("-")]
                            (),
                            Box<Expression>
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn enum_prec_left() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    #[derive(rust_sitter::Rule)]
                    #[language]
                    pub enum Expression {
                        Number(
                            #[leaf(pattern(r"\d+"))]
                            i32
                        ),
                        #[prec_left(1)]
                        Sub(
                            Box<Expression>,
                            #[leaf("-")]
                            (),
                            Box<Expression>
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn struct_extra() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    #[derive(rust_sitter::Rule)]
                    #[language]
                    pub enum Expression {
                        Number(
                            #[leaf(re(r"\d+"))] i32,
                        ),
                    }

                    #[derive(Rule)]
                    #[extra]
                    struct Whitespace {
                        #[leaf(pattern(r"\s"))]
                        _whitespace: (),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn grammar_unboxed_field() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    #[derive(rust_sitter::Rule)]
                    #[language]
                    pub struct Language {
                        e: Expression,
                    }

                    #[derive(rust_sitter::Rule)]
                    pub enum Expression {
                        Number(
                            #[leaf(re(r"\d+"))]
                            i32
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn struct_repeat() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    #[derive(rust_sitter::Rule)]
                    #[language]
                    pub struct NumberList {
                        numbers: Vec<Number>,
                    }

                    #[derive(rust_sitter::Rule)]
                    pub struct Number {
                        #[leaf(re(r"\d+"))]
                        v: i32
                    }

                    #[derive(rust_sitter::Rule)]
                    #[extra]
                    struct Whitespace {
                        #[leaf(pattern(r"\s"))]
                        _whitespace: (),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn struct_optional() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    #[derive(rust_sitter::Rule)]
                    #[language]
                    pub struct Language {
                        #[leaf(re(r"\d+"))]
                        v: Option<i32>,
                        t: Option<Number>,
                    }

                    #[derive(rust_sitter::Rule)]
                    pub struct Number {
                        #[leaf(re(r"\d+"))]
                        v: i32
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn enum_with_unamed_vector() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    #[derive(rust_sitter::Rule)]
                    pub struct Number {
                            #[leaf(re(r"\d+"))]
                            value: u32
                    }

                    #[derive(rust_sitter::Rule)]
                    #[language]
                    pub enum Expr {
                        Numbers(
                            #[repeat1]
                            Vec<Number>
                        )
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn enum_with_named_field() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    #[derive(rust_sitter::Rule)]
                    #[language]
                    pub enum Expr {
                        Number(
                                #[leaf(pattern(r"\d+"))]
                                u32
                        ),
                        Neg {
                            #[leaf("!")]
                            _bang: (),
                            value: Box<Expr>,
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }

    #[test]
    fn spanned_in_vec() -> Result<()> {
        insta::assert_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                mod grammar {
                    use rust_sitter::{Rule, Spanned};

                    #[derive(Rule)]
                    #[language]
                    pub struct NumberList {
                        numbers: Vec<Spanned<Number>>,
                    }

                    #[derive(Rule)]
                    pub struct Number {
                        #[leaf(re(r"\d+"))]
                        v: i32
                    }

                    #[derive(Rule)]
                    #[extra]
                    struct Whitespace {
                        #[leaf(pattern(r"\s"))]
                        _whitespace: (),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));

        Ok(())
    }
}
