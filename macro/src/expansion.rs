use std::collections::HashSet;

use crate::errors::IteratorExt as _;
use proc_macro2::Span;
use quote::{ToTokens, quote};
use rust_sitter_common::*;
use syn::*;

pub enum ParamOrField {
    Param(Expr),
    Field(FieldValue),
}

impl ToTokens for ParamOrField {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            ParamOrField::Param(expr) => expr.to_tokens(tokens),
            ParamOrField::Field(field) => field.to_tokens(tokens),
        }
    }
}

pub fn expand_rule(input: DeriveInput) -> Result<proc_macro2::TokenStream> {
    // TODO: Allow renaming it.
    let is_language = input
        .attrs
        .iter()
        .any(|a| sitter_attr_matches(a, "language"));
    let ident = input.ident;
    let attrs = input.attrs;
    let (extract, rule) = match input.data {
        Data::Struct(DataStruct { fields, .. }) => {
            let extract_expr =
                gen_struct_or_variant(fields.clone(), None, ident.clone(), attrs.clone())?;

            let extract_impl: Item = syn::parse_quote! {
                impl ::rust_sitter::Extract<#ident> for #ident {
                    type LeafFn<'a> = ();

                    #[allow(non_snake_case)]
                    fn extract<'a>(node: Option<::rust_sitter::tree_sitter::Node>, source: &[u8], last_idx: usize, last_pt: ::rust_sitter::tree_sitter::Point, _leaf_fn: Option<Self::LeafFn<'a>>) -> Self {
                        let node = node.expect("no node found");
                        #extract_expr
                    }
                }
            };
            let ident_str = ident.to_string();
            let rule_impl: Item = syn::parse_quote! {
                impl ::rust_sitter::rule::Rule<#ident> for #ident {
                    fn produce_ast() -> String {
                        String::new()
                    }
                    fn rule_name() -> &'static str {
                        #ident_str
                    }
                }
            };

            (extract_impl, rule_impl)
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let match_cases: Vec<Arm> = variants
                .iter()
                .map(|v| {
                    let variant_path = format!("{}_{}", ident, v.ident);

                    let extract_expr = gen_struct_or_variant(
                        v.fields.clone(),
                        Some(v.ident.clone()),
                        ident.clone(),
                        v.attrs.clone(),
                    )?;
                    Ok(syn::parse_quote! {
                        #variant_path => return #extract_expr
                    })
                })
                .sift::<Vec<Arm>>()?;

            let enum_name = &ident;
            let ident_str = enum_name.to_string();
            let extract_impl: Item = syn::parse_quote! {
                impl ::rust_sitter::Extract<#enum_name> for #enum_name {
                    type LeafFn<'a> = ();

                    #[allow(non_snake_case)]
                    fn extract<'a>(node: Option<::rust_sitter::tree_sitter::Node>, source: &[u8], _last_idx: usize, _last_pt: ::rust_sitter::tree_sitter::Point, _leaf_fn: Option<Self::LeafFn<'a>>) -> Self {
                        let node = node.expect("No node found");

                        let mut cursor = node.walk();
                        assert!(cursor.goto_first_child(), "Could not find a child corresponding to any enum branch");
                        loop {
                            let node = cursor.node();
                            match node.kind() {
                                #(#match_cases),*,
                                _ => if !cursor.goto_next_sibling() {
                                    panic!("Could not find a child corresponding to any enum branch")
                                }
                            }
                        }
                    }
                }
            };

            let rule_impl: Item = syn::parse_quote! {
                impl ::rust_sitter::rule::Rule<#enum_name> for #enum_name {
                    fn produce_ast() -> String {
                        String::new()
                    }

                    fn rule_name() -> &'static str {
                        #ident_str
                    }
                }
            };
            (extract_impl, rule_impl)
        }
        Data::Union(_) => panic!("Union types not supported"),
    };

    // If it is language, then we need to generate the corresponding functions.
    let lang = if is_language {
        let tree_sitter_ident = Ident::new(&format!("tree_sitter_{ident}"), Span::call_site());

        let root_type_docstr = format!("[`{ident}`]");
        // TODO: We can maybe make a trait for `language`. It should also have a `parse` function.
        quote! {

            impl #ident {
                pub fn language() -> ::rust_sitter::tree_sitter::Language {
                    unsafe extern "C" {
                        fn #tree_sitter_ident() -> ::rust_sitter::tree_sitter::Language;
                    }
                    unsafe { #tree_sitter_ident() }
                }
                /// Parse an input string according to the grammar. Returns either any parsing errors that happened, or a
                #[doc = #root_type_docstr]
                /// instance containing the parsed structured data.
                pub fn parse(input: &str) -> core::result::Result<Self, Vec<::rust_sitter::errors::ParseError>> {
                    ::rust_sitter::__private::parse(input, Self::language)
                }
            }
        }
    } else {
        quote! {}
    };

    Ok(quote! {
        #lang
        #extract
        #rule
    })
}

fn gen_field(ident_str: String, leaf: Field) -> Expr {
    let leaf_type = leaf.ty;

    let leaf_attr = leaf
        .attrs
        .iter()
        .find(|attr| sitter_attr_matches(attr, "leaf"));

    let transform = leaf.attrs.iter().find_map(|attr| {
        if sitter_attr_matches(attr, "transform") || sitter_attr_matches(attr, "with") {
            Some((false, attr.parse_args::<Expr>().unwrap()))
        } else if sitter_attr_matches(attr, "with_node") {
            Some((true, attr.parse_args::<Expr>().unwrap()))
        } else {
            None
        }
    });

    if transform.is_some() && leaf_attr.is_none() {
        panic!("Cannot transform non-leaf nodes");
    }

    let text_attr = leaf
        .attrs
        .iter()
        .find(|attr| sitter_attr_matches(attr, "text"));
    if text_attr.is_some() {
        if leaf_attr.is_some() {
            panic!("Cannot use leaf and text at the same time");
        }
        return syn::parse_quote!({
            ::rust_sitter::__private::skip_text(cursor, #ident_str);
        });
    }

    let (leaf_type, closure_expr): (Type, Expr) = match transform {
        Some((is_node, closure)) => {
            let mut non_leaf = HashSet::new();
            // Major hackery...
            if !is_node {
                non_leaf.insert("Spanned");
                non_leaf.insert("Box");
                non_leaf.insert("Option");
                non_leaf.insert("Vec");
            }
            let wrapped_leaf_type = wrap_leaf_type(&leaf_type, &non_leaf);
            let input_type: syn::Type = if is_node {
                syn::parse_quote!(&::rust_sitter::NodeExt<'_>)
            } else {
                syn::parse_quote!(&str)
            };
            (
                wrapped_leaf_type,
                syn::parse_quote!(Some((#closure) as fn(#input_type) -> #leaf_type)),
            )
        }
        None => (leaf_type, syn::parse_quote!(None)),
    };

    syn::parse_quote!({
        ::rust_sitter::__private::extract_field::<#leaf_type,_>(cursor, source, last_idx, last_pt, #ident_str, #closure_expr)
    })
}

fn gen_struct_or_variant(
    fields: Fields,
    variant_ident: Option<Ident>,
    containing_type: Ident,
    container_attrs: Vec<Attribute>,
) -> Result<Expr> {
    let children_parsed = if fields == Fields::Unit {
        let expr = {
            let dummy_field = Field {
                attrs: container_attrs,
                vis: Visibility::Inherited,
                mutability: FieldMutability::None,
                ident: None,
                colon_token: None,
                ty: Type::Verbatim(quote!(())), // unit type.
            };

            gen_field("unit".to_string(), dummy_field)
        };
        vec![ParamOrField::Param(expr)]
    } else {
        fields
            .iter()
            .enumerate()
            .map(|(i, field)| {
                let expr = if let Some(skip_attrs) = field
                    .attrs
                    .iter()
                    .find(|attr| sitter_attr_matches(attr, "skip"))
                {
                    skip_attrs.parse_args::<syn::Expr>()?
                } else {
                    let ident_str = field
                        .ident
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or(format!("{i}"));

                    gen_field(ident_str, field.clone())
                };

                let field = if let Some(field_name) = &field.ident {
                    ParamOrField::Field(FieldValue {
                        attrs: vec![],
                        member: Member::Named(field_name.clone()),
                        colon_token: Some(Token![:](Span::call_site())),
                        expr,
                    })
                } else {
                    ParamOrField::Param(expr)
                };
                Ok(field)
            })
            .sift::<Vec<ParamOrField>>()?
    };

    let construct_name = match variant_ident {
        Some(ident) => quote! {
            #containing_type::#ident
        },
        None => quote! {
            #containing_type
        },
    };

    let construct_expr = {
        match &fields {
            Fields::Unit => {
                let ParamOrField::Param(ref expr) = children_parsed[0] else {
                    unreachable!()
                };

                quote! {
                    {
                        #expr;
                        #construct_name
                    }
                }
            }
            Fields::Named(_) => quote! {
                #construct_name {
                    #(#children_parsed),*
                }
            },
            Fields::Unnamed(_) => quote! {
                #construct_name(
                    #(#children_parsed),*
                )
            },
        }
    };

    Ok(
        syn::parse_quote!(::rust_sitter::__private::extract_struct_or_variant(node, move |cursor, last_idx, last_pt| #construct_expr)),
    )
}

// pub fn expand_grammar(input: ItemMod) -> Result<ItemMod> {
//     let attr = input
//         .attrs
//         .iter()
//         .find(|a| a.path() == &syn::parse_quote!(rust_sitter::grammar))
//         .ok_or_else(|| syn::Error::new(Span::call_site(), "Each grammar must have a name"))?;
//     let grammar_name_expr =
//         attr.parse_args_with(Punctuated::<Expr, Token![,]>::parse_terminated)?;
//     if grammar_name_expr.is_empty() {
//         return Err(syn::Error::new(
//             Span::call_site(),
//             "Expected a string literal grammar name",
//         ));
//     }
//     if grammar_name_expr.len() > 2 {
//         return Err(syn::Error::new(
//             Span::call_site(),
//             "Expected at most two inputs",
//         ));
//     }
//     let grammar_name = if let Expr::Lit(ExprLit {
//         attrs: _,
//         lit: Lit::Str(s),
//     }) = grammar_name_expr.first().unwrap()
//     {
//         s.value()
//     } else {
//         return Err(syn::Error::new(
//             Span::call_site(),
//             "Expected a string literal grammar name",
//         ));
//     };
//
//     let should_parse = if let Some(Expr::Lit(ExprLit {
//         attrs: _,
//         lit: Lit::Bool(b),
//     })) = grammar_name_expr.last()
//     {
//         b.value()
//     } else {
//         false
//     };
//
//     let (brace, new_contents) = input.content.as_ref().ok_or_else(|| {
//         syn::Error::new(
//             Span::call_site(),
//             "Expected the module to have inline contents (`mod my_module { .. }` syntax)",
//         )
//     })?;
//
//     let root_type = new_contents
//         .iter()
//         .find_map(|item| match item {
//             Item::Enum(ItemEnum { ident, attrs, .. })
//             | Item::Struct(ItemStruct { ident, attrs, .. }) => {
//                 if attrs
//                     .iter()
//                     .any(|attr| attr.path() == &syn::parse_quote!(rust_sitter::language))
//                 {
//                     Some(ident.clone())
//                 } else {
//                     None
//                 }
//             }
//             _ => None,
//         })
//         .ok_or_else(|| {
//             syn::Error::new(
//                 Span::call_site(),
//                 "Each parser must have the root type annotated with `#[rust_sitter::language]`",
//             )
//         })?;
//
//     let mut transformed: Vec<Item> = new_contents
//         .iter()
//         .cloned()
//         .map(|c| match c {
//             Item::Enum(mut e) => {
//                     let match_cases: Vec<Arm> = e.variants.iter().map(|v| {
//                         let variant_path = format!("{}_{}", e.ident, v.ident);
//
//                         let extract_expr = gen_struct_or_variant(
//                             v.fields.clone(),
//                             Some(v.ident.clone()),
//                             e.ident.clone(),
//                             v.attrs.clone(),
//                         )?;
//                         Ok(syn::parse_quote! {
//                             #variant_path => return #extract_expr
//                         })
//                     }).sift::<Vec<Arm>>()?;
//
//                     e.attrs.retain(|a| !is_sitter_attr(a));
//                     e.variants.iter_mut().for_each(|v| {
//                         v.attrs.retain(|a| !is_sitter_attr(a));
//                         v.fields.iter_mut().for_each(|f| {
//                             f.attrs.retain(|a| !is_sitter_attr(a));
//                         });
//                     });
//
//                     let enum_name = &e.ident;
//                     let extract_impl: Item = syn::parse_quote! {
//                         impl ::rust_sitter::Extract<#enum_name> for #enum_name {
//                             type LeafFn<'a> = ();
//
//                             #[allow(non_snake_case)]
//                             fn extract<'a>(node: Option<::rust_sitter::tree_sitter::Node>, source: &[u8], _last_idx: usize, _last_pt: ::rust_sitter::tree_sitter::Point, _leaf_fn: Option<Self::LeafFn<'a>>) -> Self {
//                                 let node = node.expect("No node found");
//
//                                 let mut cursor = node.walk();
//                                 assert!(cursor.goto_first_child(), "Could not find a child corresponding to any enum branch");
//                                 loop {
//                                     let node = cursor.node();
//                                     match node.kind() {
//                                         #(#match_cases),*,
//                                         _ => if !cursor.goto_next_sibling() {
//                                             panic!("Could not find a child corresponding to any enum branch")
//                                         }
//                                     }
//                                 }
//                             }
//                         }
//                     };
//                     Ok(vec![Item::Enum(e), extract_impl])
//             }
//
//             Item::Struct(mut s) => {
//                     let struct_name = &s.ident;
//                     let extract_expr = gen_struct_or_variant(
//                         s.fields.clone(),
//                         None,
//                         s.ident.clone(),
//                         s.attrs.clone(),
//                     )?;
//
//                     s.attrs.retain(|a| !is_sitter_attr(a));
//                     s.fields.iter_mut().for_each(|f| {
//                         f.attrs.retain(|a| !is_sitter_attr(a));
//                     });
//
//
//                     let extract_impl: Item = syn::parse_quote! {
//                         impl ::rust_sitter::Extract<#struct_name> for #struct_name {
//                             type LeafFn<'a> = ();
//
//                             #[allow(non_snake_case)]
//                             fn extract<'a>(node: Option<::rust_sitter::tree_sitter::Node>, source: &[u8], last_idx: usize, last_pt: ::rust_sitter::tree_sitter::Point, _leaf_fn: Option<Self::LeafFn<'a>>) -> Self {
//                                 let node = node.expect("no node found");
//                                 #extract_expr
//                             }
//                         }
//                     };
//
//                     Ok(vec![Item::Struct(s), extract_impl])
//             }
//
//             o => Ok(vec![o]),
//         })
//         .sift::<Vec<_>>()?.into_iter().flatten().collect();
//
//     let tree_sitter_ident = Ident::new(&format!("tree_sitter_{grammar_name}"), Span::call_site());
//
//     transformed.push(syn::parse_quote! {
//         unsafe extern "C" {
//             fn #tree_sitter_ident() -> ::rust_sitter::tree_sitter::Language;
//         }
//     });
//
//     transformed.push(syn::parse_quote! {
//         pub fn language() -> ::rust_sitter::tree_sitter::Language {
//             unsafe { #tree_sitter_ident() }
//         }
//     });
//
//     let root_type_docstr = format!("[`{root_type}`]");
//     transformed.push(syn::parse_quote! {
//     /// Parse an input string according to the grammar. Returns either any parsing errors that happened, or a
//     #[doc = #root_type_docstr]
//     /// instance containing the parsed structured data.
//       pub fn parse(input: &str) -> core::result::Result<#root_type, Vec<::rust_sitter::errors::ParseError>> {
//         ::rust_sitter::__private::parse::<#root_type>(input, language)
//       }
//   });
//
//     // Produces the grammar as a JSON constant.
//     if should_parse {
//         let grammars = rust_sitter_common::expansion::generate_grammar(&input).to_string();
//         transformed.push(syn::parse_quote! {
//             pub const GRAMMAR: &str = #grammars;
//         });
//     }
//
//     let mut filtered_attrs = input.attrs;
//     filtered_attrs.retain(|a| !is_sitter_attr(a));
//     Ok(ItemMod {
//         attrs: filtered_attrs,
//         vis: input.vis,
//         unsafety: None,
//         mod_token: input.mod_token,
//         ident: input.ident,
//         content: Some((*brace, transformed)),
//         semi: input.semi,
//     })
// }
