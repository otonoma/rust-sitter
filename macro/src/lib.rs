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
// grammar! {
//  rule: seq("function", $.ident, "(", repeat($.input), ")") -> |id, inputs| Function { name,
//  inputs: inputs.into() };
//
//  ident: /re/;
//  input: seq($.ident, ":", $.ident);
//
// }
// #[proc_macro]
// pub fn grammar2(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//     grammar::parse_grammar_macro(input)
// }

#[proc_macro_derive(
    Rule,
    // Alternatively, we can instead have one helper like `baum(...)` - generally looks cleaner.
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
        token,
        extra,
        with,
        with_node,
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

// /// Mark a module to be analyzed for a Rust Sitter grammar. Takes a single, unnamed argument, which
// /// specifies the name of the grammar. This name must be unique across all Rust Sitter grammars within
// /// a compilation unit.
// #[proc_macro_attribute]
// pub fn grammar(
//     attr: proc_macro::TokenStream,
//     input: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     let attr_tokens: proc_macro2::TokenStream = attr.into();
//     let module: ItemMod = parse_macro_input!(input);
//     let expanded = derive_rule(syn::parse_quote! {
//         #[rust_sitter::grammar[#attr_tokens]]
//         #module
//     })
//     .map(ToTokens::into_token_stream)
//     .unwrap_or_else(syn::Error::into_compile_error);
//     proc_macro::TokenStream::from(expanded)
// }

// #[proc_macro_attribute]
// /// Marks the top level AST node where parsing should start.
// ///
// /// ## Example
// /// ```ignore
// /// #[rust_sitter::language]
// /// pub struct Code {
// ///     ...
// /// }
// /// ```
// pub fn language(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// This annotation marks a node as extra, which can safely be skipped while parsing.
// /// This is useful for handling whitespace/newlines/comments.
// ///
// /// ## Example
// /// ```ignore
// /// #[rust_sitter::extra]
// /// struct Whitespace {
// ///     #[rust_sitter::leaf(re(r"\s"))]
// ///     _whitespace: (),
// /// }
// /// ```
// pub fn extra(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// Defines a field which matches a specific token in the source string.
// /// The token can be defined by passing one of two arguments
// /// - `text`: a string literal that will be exactly matched
// /// - `pattern`: a regular expression that will be matched against the source string
// ///
// /// If the resulting token needs to be converted into a richer type at runtime,
// /// such as a number, then the `transform` argument can be used to specify a function
// /// that will be called with the token's text.
// ///
// /// The attribute can also be applied to a struct or enum variant with no fields.
// ///
// /// ## Examples
// ///
// /// Using the `leaf` attribute on a field:
// /// ```ignore
// /// Number(
// ///     #[rust_sitter::leaf(re(r"\d+"))]
// ///     u32
// /// )
// /// ```
// ///
// /// Using the attribute on a unit struct or unit enum variant:
// /// ```ignore
// /// #[rust_sitter::leaf("9")]
// /// struct BigDigit;
// ///
// /// enum SmallDigit {
// ///     #[rust_sitter::leaf("0")]
// ///     Zero,
// ///     #[rust_sitter::leaf("1")]
// ///     One,
// /// }
// /// ```
// ///
// pub fn leaf(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// Defines text in the grammar that should be parsed but not explicitly used. No explicit rule is
// /// created and these segments are inlined.
// ///
// /// ## Example
// /// ```ignore
// /// struct Function {
// ///     #[text("function")]
// ///     _function: (),
// ///     name: Ident,
// ///     #[text("(")]
// ///     _lparen: (),
// ///     // ...
// /// }
// /// ```
// pub fn text(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// Defines a field that does not correspond to anything in the input string,
// /// such as some metadata. Takes a single, unnamed argument, which is the value
// /// used to populate the field at runtime.
// ///
// /// ## Example
// /// ```ignore
// /// struct MyNode {
// ///    ...,
// ///    #[rust_sitter::skip(false)]
// ///    node_visited: bool
// /// }
// /// ```
// pub fn skip(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// /// Applies a custom transformation for parsing the input text of a `leaf` node.
// /// Without using `with` the default extractor is applied.
// ///
// /// ## Example
// /// ```ignore
// /// struct CustomInt(
// ///     #[leaf(re(r"\d+"))]
// ///     #[with(plus_one)]
// ///     i32
// /// );
// ///
// /// fn plus_one(s: &str) -> i32 {
// ///     s.parse::<i32>().unwrap() + 1
// /// }
// /// ```
// #[proc_macro_attribute]
// pub fn with(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// /// Alias for `with`.
// #[proc_macro_attribute]
// pub fn transform(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// Defines a precedence level for a non-terminal that has no associativity.
// ///
// /// This annotation takes a single, unnamed parameter, which specifies the precedence level.
// /// This is used to resolve conflicts with other non-terminals, so that the one with the higher
// /// precedence will bind more tightly (appear lower in the parse tree).
// ///
// /// ## Example
// /// ```ignore
// /// #[rust_sitter::prec(1)]
// /// PriorityExpr(Box<Expr>, Box<Expr>)
// /// ```
// pub fn prec(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// Defines a precedence level for a non-terminal that should be left-associative.
// /// For example, with subtraction we expect 1 - 2 - 3 to be parsed as (1 - 2) - 3,
// /// which corresponds to a left-associativity.
// ///
// /// This annotation takes a single, unnamed parameter, which specifies the precedence level.
// /// This is used to resolve conflicts with other non-terminals, so that the one with the higher
// /// precedence will bind more tightly (appear lower in the parse tree).
// ///
// /// ## Example
// /// ```ignore
// /// #[rust_sitter::prec_left(1)]
// /// Subtract(Box<Expr>, Box<Expr>)
// /// ```
// pub fn prec_left(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// Defines a precedence level for a non-terminal that should be right-associative.
// /// For example, with cons we could have 1 :: 2 :: 3 to be parsed as 1 :: (2 :: 3),
// /// which corresponds to a right-associativity.
// ///
// /// This annotation takes a single, unnamed parameter, which specifies the precedence level.
// /// This is used to resolve conflicts with other non-terminals, so that the one with the higher
// /// precedence will bind more tightly (appear lower in the parse tree).
// ///
// /// ## Example
// /// ```ignore
// /// #[rust_sitter::prec_right(1)]
// /// Cons(Box<Expr>, Box<Expr>)
// /// ```
// pub fn prec_right(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// This macro is similar to [`prec`], but the given numerical precedence is applied at runtime instead
// /// of at parser generation time. This is only necessary when handling a conflict dynamically using
// /// [`conflicts`], and when there is a genuine ambiguity: multiple rules correctly
// /// match a given piece of code. In that event, Rust-sitter compares the total dynamic precedence
// /// associated with each rule, and selects the one with the highest total.
// ///
// /// This is similar to dynamic precedence directives in Bison grammars.
// ///
// /// ## Example
// /// ```ignore
// /// #[rust_sitter::prec_dynamic(1)]
// /// Cons(Box<Expr>, Box<Expr>)
// /// ```
// pub fn prec_dynamic(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// Usually, whitespace is optional before each token. This attribute means that the token will only match if there is no whitespace.
// ///
// /// ## Example
// /// ```ignore
// /// struct StringFragment(
// ///     #[rust_sitter::immediate]
// ///     #[rust_sitter::leaf(pattern(r"[^"\\]+"))]
// ///     ()
// /// );
// /// ```
// pub fn immediate(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// Allows the leaf node sequence to be created as a single token.
// ///
// /// ## Example
// /// ```ignore
// /// struct StringFragment(
// ///     #[rust_sitter::token]
// ///     #[rust_sitter::leaf(pattern(r"[^"\\]+"))]
// ///     ()
// /// );
// /// ```
// pub fn token(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// On `Vec<_>` typed fields, specifies a non-terminal that should be parsed in between the elements.
// /// The [`rust_sitter::repeat`] annotation can be used on the field as well.
// ///
// /// This annotation takes a single, unnamed argument, which specifies a field type to parse. This can
// /// either be a reference to another type, or can be defined as a `leaf` field. Generally, the argument
// /// is parsed using the same rules as an unnamed field of an enum variant.
// ///
// /// ## Example
// /// ```ignore
// /// #[rust_sitter::delimited(",")]
// /// numbers: Vec<Number>
// /// ```
// pub fn delimited(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }
//
// #[proc_macro_attribute]
// /// On `Vec<_>` typed fields, specifies additional config for how the repeated elements should
// /// be parsed. In particular, this annotation takes the following named arguments:
// /// - `non_empty` - if this argument is `true`, then there must be at least one element parsed
// ///
// /// ## Example
// /// ```ignore
// /// #[rust_sitter::repeat(non_empty = true)]
// /// numbers: Vec<Number>
// /// ```
// pub fn repeat(
//     _attr: proc_macro::TokenStream,
//     item: proc_macro::TokenStream,
// ) -> proc_macro::TokenStream {
//     item
// }

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{Read, Write};
    use std::process::Command;

    use quote::ToTokens;
    use quote::quote;
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
                output.push(proc_macro2::TokenStream::from(result));
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
