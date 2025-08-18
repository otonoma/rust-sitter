use std::collections::HashSet;

use crate::errors::IteratorExt as _;
use proc_macro2::Span;
use quote::{ToTokens, quote};
use rust_sitter_common::{
    expansion::{ExpansionState, RuleDerive},
    *,
};
use syn::{spanned::Spanned, *};

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
    // At the very beginning, parse out the common rule json. This will pick up all of the errors
    // there at compile time, and allow us to cleanly represent them. This is a lot of extra
    // compilation time but it is the best we can do for now. Probably isn't noticable in general.
    let d = RuleDerive::from_derive_input_known(input.clone())?;
    let mut ctx = ExpansionState::default();
    rust_sitter_common::expansion::process_rule(d, &mut ctx)?;

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
                    fn extract<'a, 'tree>(
                        ctx: &mut ::rust_sitter::extract::ExtractContext<'_>,
                        node: Option<::rust_sitter::tree_sitter::Node<'tree>>,
                        source: &[u8],
                        _leaf_fn: Option<Self::LeafFn<'a>>,
                    ) -> Result<Self, ::rust_sitter::extract::ExtractError<'tree>> {
                        let node = node.ok_or_else(|| {
                            ::rust_sitter::error::ExtractError::missing_node(ctx, stringify!(#ident))
                        })?;

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
                    fn extract<'a, 'tree>(
                        _ctx: &mut ::rust_sitter::extract::ExtractContext<'_>,
                        node: Option<::rust_sitter::tree_sitter::Node<'tree>>,
                        source: &[u8],
                        _leaf_fn: Option<Self::LeafFn<'a>>,
                    ) -> Result<Self, ::rust_sitter::extract::ExtractError<'tree>> {
                        let node = node.ok_or_else(|| {
                            ::rust_sitter::error::ExtractError::missing_node(_ctx, stringify!(#enum_name))
                        })?;

                        let mut cursor = node.walk();
                        assert!(cursor.goto_first_child(), "Could not find a child corresponding to any enum branch");
                        loop {
                            let node = cursor.node();
                            match node.kind() {
                                #(#match_cases),*,
                                k => if !cursor.goto_next_sibling() {
                                    panic!("Could not find a child corresponding to any enum branch: {k}")
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
        Data::Union(_) => return Err(Error::new(ident.span(), "Union types not supported")),
    };

    // If it is language, then we need to generate the corresponding functions.
    let lang = if is_language {
        let tree_sitter_ident = Ident::new(&format!("tree_sitter_{ident}"), Span::call_site());

        let root_type_docstr = format!("[`{ident}`]");
        quote! {
            impl ::rust_sitter::rule::Language for #ident {
                fn produce_grammar() -> String {
                    String::new()
                }

                fn language() -> ::rust_sitter::tree_sitter::Language {
                    unsafe extern "C" {
                        fn #tree_sitter_ident() -> ::rust_sitter::tree_sitter::Language;
                    }
                    unsafe { #tree_sitter_ident() }
                }
                /// Parse an input string according to the grammar. Returns either any parsing errors that happened, or a
                #[doc = #root_type_docstr]
                /// instance containing the parsed structured data.
                fn parse(input: &str) -> ::rust_sitter::ParseResult<Self> {
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

fn gen_field(ident_str: String, leaf: Field) -> Result<Expr> {
    let leaf_type = &leaf.ty;

    let leaf_attr = leaf
        .attrs
        .iter()
        .find(|attr| sitter_attr_matches(attr, "leaf"));

    let transform = leaf.attrs.iter().find_map(|attr| {
        if sitter_attr_matches(attr, "transform") || sitter_attr_matches(attr, "with") {
            Some((false, attr))
        } else if sitter_attr_matches(attr, "with_node") {
            Some((true, attr))
        } else {
            None
        }
    });

    if transform.is_some() && leaf_attr.is_none() {
        return Err(Error::new(leaf.span(), "Cannot transform non-leaf nodes"));
    }

    let text_attr = leaf
        .attrs
        .iter()
        .find(|attr| sitter_attr_matches(attr, "text"));
    if let Some(text_attr) = text_attr {
        if let Some(attr) = leaf_attr {
            return Err(Error::new(
                attr.span(),
                "Cannot use leaf and text at the same time",
            ));
        }
        let text_input = text_attr.parse_args::<TsInput>()?;
        text_input.evaluate()?;
        // TODO: Handle this correctly.
        return Ok(syn::parse_quote!({
            ::rust_sitter::__private::skip_text(state, #ident_str)?;
            Ok::<_, ::rust_sitter::extract::ExtractError>(())
        }));
    }

    // NOTE (JAB, 2025-07-17): We want to use this eventually in the extract generation, so it
    // makes sense to parse it here. Additionally, we get compile time errors at this level instead
    // of at the parser generation phase.
    let leaf_input = leaf_attr.map(|a| a.parse_args::<TsInput>()).transpose()?;
    // But for now, we just evaluate it to make sure it works correctly.
    if let Some(leaf_input) = leaf_input {
        leaf_input.evaluate()?;
    }

    let (leaf_type, closure_expr): (Type, Expr) = match transform {
        Some((is_node, closure)) => {
            let closure = closure.parse_args::<Expr>()?;
            let mut non_leaf = HashSet::new();
            // Major hackery...
            if !is_node {
                non_leaf.insert("Spanned");
                non_leaf.insert("Box");
                non_leaf.insert("Option");
                non_leaf.insert("Vec");
            }
            let wrapped_leaf_type = wrap_leaf_type(leaf_type, &non_leaf);
            let input_type: syn::Type = if is_node {
                syn::parse_quote!(&::rust_sitter::extract::NodeExt<'_>)
            } else {
                syn::parse_quote!(&str)
            };
            (
                wrapped_leaf_type,
                syn::parse_quote!(Some((#closure) as fn(#input_type) -> #leaf_type)),
            )
        }
        None => (leaf_type.clone(), syn::parse_quote!(None)),
    };

    Ok(syn::parse_quote!({
        ::rust_sitter::__private::extract_field::<#leaf_type,_>(state, source, #ident_str, #closure_expr)
    }))
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

            gen_field("unit".to_string(), dummy_field)?
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

                    gen_field(ident_str, field.clone())?
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
                        #expr?;
                        Ok(#construct_name)
                    }
                }
            }
            Fields::Named(_) => quote! {
                Ok(#construct_name {
                    #(#children_parsed?),*
                })
            },
            Fields::Unnamed(_) => quote! {
                Ok(#construct_name(
                    #(#children_parsed?),*
                ))
            },
        }
    };

    Ok(
        syn::parse_quote!(::rust_sitter::__private::extract_struct_or_variant(node, move |state| #construct_expr)),
    )
}
