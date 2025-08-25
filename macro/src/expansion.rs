use std::collections::{HashMap, HashSet};

use crate::errors::IteratorExt as _;
use proc_macro2::Span;
use quote::{ToTokens, quote};
use rust_sitter_common::{
    expansion::{ExpansionState, RuleDerive},
    *,
};
use rust_sitter_types::grammar::{Grammar, RuleDef};
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
    let mut ctx = ExpansionState::new();
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
            let extract_expr = gen_struct_or_variant(
                fields.clone(),
                None,
                ident.clone(),
                attrs.clone(),
                &ctx.grammar,
            )?;

            let extract_impl: Item = syn::parse_quote! {
                impl ::rust_sitter::Extract for #ident {
                    type Output = Self;
                    type LeafFn = ();
                    #[allow(non_snake_case)]
                    fn extract<'tree>(
                        ctx: &mut ::rust_sitter::extract::ExtractContext,
                        node: Option<::rust_sitter::tree_sitter::Node<'tree>>,
                        source: &[u8],
                        _l: Self::LeafFn,
                    ) -> Result<Self, ::rust_sitter::extract::ExtractError<'tree>> {
                        let node = node.ok_or_else(|| {
                            ::rust_sitter::error::ExtractError::missing_node(ctx)
                        })?;
                        #extract_expr
                    }
                }
            };
            let ident_str = ident.to_string();
            let rule_impl: Item = syn::parse_quote! {
                impl ::rust_sitter::rule::Rule for #ident {
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
                        &ctx.grammar,
                    )?;
                    Ok(syn::parse_quote! {
                        #variant_path => return #extract_expr
                    })
                })
                .sift::<Vec<Arm>>()?;

            let enum_name = &ident;
            let ident_str = enum_name.to_string();
            let extract_impl: Item = syn::parse_quote! {
                impl ::rust_sitter::Extract for #enum_name {
                    type Output = Self;
                    type LeafFn = ();
                    #[allow(non_snake_case)]
                    fn extract<'tree>(
                        ctx: &mut ::rust_sitter::extract::ExtractContext,
                        node: Option<::rust_sitter::tree_sitter::Node<'tree>>,
                        source: &[u8],
                        _l: Self::LeafFn,
                    ) -> Result<Self, ::rust_sitter::extract::ExtractError<'tree>> {
                        let node = node.ok_or_else(|| {
                            ::rust_sitter::error::ExtractError::missing_node(ctx)
                        })?;

                        let mut cursor = node.walk();
                        if !cursor.goto_first_child() {
                            return Err(::rust_sitter::error::ExtractError::missing_node(ctx));
                        }
                        loop {
                            let node = cursor.node();
                            match node.kind() {
                                #(#match_cases),*,
                                k => if !cursor.goto_next_sibling() {
                                    return Err(::rust_sitter::error::ExtractError::missing_enum(ctx));
                                }
                            }
                        }
                    }
                }
            };

            let rule_impl: Item = syn::parse_quote! {
                impl ::rust_sitter::rule::Rule for #enum_name {
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

fn gen_field(ident_str: String, leaf: Field, grammar: &RuleDef) -> Result<Expr> {
    let leaf_type = &leaf.ty;

    let leaf_attr = leaf
        .attrs
        .iter()
        .find(|attr| sitter_attr_matches(attr, "leaf"));

    let transform = leaf
        .attrs
        .iter()
        .find(|attr| sitter_attr_matches(attr, "transform") || sitter_attr_matches(attr, "with"));

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

    let leaf_input = leaf_attr.map(|a| a.parse_args::<TsInput>()).transpose()?;
    // But for now, we just evaluate it to make sure it works correctly.
    if let Some(leaf_input) = leaf_input {
        leaf_input.evaluate()?;
    }

    let extractor: Expr = parse_quote! { ::rust_sitter::extract::BaseExtractor::default() };

    let (leaf_type, leaf_fn): (Type, Expr) = match transform {
        Some(closure) => {
            let closure = closure.parse_args::<Expr>()?;
            let mut non_leaf = HashSet::new();
            non_leaf.insert("Spanned");
            non_leaf.insert("Box");
            non_leaf.insert("Option");
            let ty = wrap_leaf_type(leaf_type, &non_leaf);
            (ty, closure)
        }
        None => (leaf_type.clone(), parse_quote! { () }),
    };

    let extract_state = rule_def_to_extract(grammar)?;

    Ok(parse_quote! {
        ::rust_sitter::__private::extract_field::<#leaf_type, _>(#extractor, #leaf_fn, state, #extract_state, source, #ident_str)
    })
}

fn gen_struct_or_variant(
    fields: Fields,
    variant_ident: Option<Ident>,
    containing_type: Ident,
    container_attrs: Vec<Attribute>,
    grammar: &Grammar,
) -> Result<Expr> {
    let path = match &variant_ident {
        Some(v) => format!("{containing_type}_{v}"),
        None => containing_type.to_string(),
    };
    let rule = grammar
        .rules
        .get(&path)
        .expect("Unexpected state, no grammar found");
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

            gen_field("unit".to_owned(), dummy_field, rule)?
        };
        vec![ParamOrField::Param(expr)]
    } else {
        // Parse out the rule into its appropriate sub parts.
        // All top-level rules at this level are guaranteed to be `SEQ` of `FIELD`s. If a field is
        // optional, the optional part comes before the `FIELD` definition, although that may be
        // unnecessary. However, we don't need to check the fields specifically, because they can be
        // determined by the actual field names instead.
        let field_grammars: HashMap<_, _> = match rule.as_seq().expect("Must be a SEQ") {
            RuleDef::SEQ { members } => fields
                .iter()
                .enumerate()
                .zip(members)
                .map(|((i, field), def)| {
                    let ident_str = field
                        .ident
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or(format!("{i}"));
                    (ident_str, def)
                })
                .collect(),
            _ => {
                unreachable!()
            }
        };

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

                    let grammar = field_grammars
                        .get(&ident_str)
                        .expect("Missing ident grammar");
                    gen_field(ident_str, field.clone(), grammar)?
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
        syn::parse_quote!(::rust_sitter::__private::extract_struct_or_variant(stringify!(#construct_name), node, move |state| #construct_expr)),
    )
}

fn rule_def_to_extract(def: &RuleDef) -> Result<proc_macro2::TokenStream> {
    let mut states = vec![];
    // Handle if the top level rule is itself optional.
    let optional = if let Some(def) = def.as_optional() {
        // Don't propogate the optional to all of the inner states.
        rule_def_add_state(def, false, &mut states);
        true
    } else {
        rule_def_add_state(def, false, &mut states);
        false
    };
    let num_states = states.len() as u32;
    let states = states.into_iter().enumerate().map(|(state, value)| {
        let state = state as u32;
        quote! {
            #state => #value,
        }
    });
    Ok(quote! {
        ::rust_sitter::extract::ExtractFieldContext::new(#num_states, #optional, |state| {
            match state {
                #(#states)*
                #num_states => ::rust_sitter::extract::ExtractFieldState::Complete,
                _ => ::rust_sitter::extract::ExtractFieldState::Overflow,
            }
        })
    })
}

fn rule_def_add_state(def: &RuleDef, optional: bool, states: &mut Vec<proc_macro2::TokenStream>) {
    let s = match def {
        RuleDef::SYMBOL { name } => {
            quote! {
                ::rust_sitter::extract::ExtractFieldState::Str(#name, true, #optional)
            }
        }
        RuleDef::STRING { value } => {
            quote! {
                ::rust_sitter::extract::ExtractFieldState::Str(#value, false, #optional)
            }
        }
        RuleDef::BLANK => return,
        // Not sure what we get here, let's just assume the string is enough though.
        RuleDef::PATTERN { .. } => {
            // It is not possible to have these in direct field extractions, actually. A quirk of
            // tree-sitter, they are always set to `.visible = false`. Maybe we can create a PR
            // where PATTERNs can be exposed if they are wrapped in a FIELD.
            return;
        }
        RuleDef::CHOICE { members } => {
            // Special handle the optional case.
            if let Some(value) = def.as_optional() {
                return rule_def_add_state(value, true, states);
            } else {
                // TODO: Need to figure out the optional case now, should only produce one state
                // that can then enumerate on all the values.
                // It could just return all of them as a set/array, _or_ the state function could
                // take in the inputs and do the checking for us instead.
                let strs = members.iter().map(|s| match s {
                    RuleDef::STRING { value } => quote! { (#value, false) },
                    RuleDef::SYMBOL { name } => quote! { (#name, true) },
                    _ => panic!("CHOICE cannot use {s:#?} currently"),
                });
                quote! {
                    ::rust_sitter::extract::ExtractFieldState::Choice(&[#(#strs),*], #optional)
                }
            }
        }
        // TODO: Handle subfields appropriately?
        RuleDef::FIELD { name: _, content } => {
            return rule_def_add_state(content, optional, states);
        }
        RuleDef::SEQ { members } => {
            return members
                .iter()
                .for_each(|def| rule_def_add_state(def, optional, states));
        }
        RuleDef::PREC_DYNAMIC { value: _, content }
        | RuleDef::PREC_LEFT { value: _, content }
        | RuleDef::PREC_RIGHT { value: _, content }
        | RuleDef::PREC { value: _, content }
        | RuleDef::TOKEN { content }
        | RuleDef::IMMEDIATE_TOKEN { content } => {
            return rule_def_add_state(content, optional, states);
        }
        RuleDef::ALIAS { .. } => unreachable!("ALIAS not supported in this context"),
        RuleDef::REPEAT { content: _ } => unreachable!("REPEAT not supported in this context"),
        RuleDef::REPEAT1 { content: _ } => unreachable!("REPEAT1 not supported in this context"),
        RuleDef::RESERVED { .. } => unreachable!("RESERVED not supported in this context"),
    };

    states.push(s);
}
