use std::collections::HashSet;

use super::*;
use serde_json::{Map, Value, json};
use syn::{parse::Parse, punctuated::Punctuated};

#[derive(Debug)]
pub struct RuleDerive {
    pub ident: syn::Ident,
    pub attrs: Vec<Attribute>,
    pub extras: Extras,
    pub data: syn::Data,
}

impl RuleDerive {
    pub fn from_derive_input(d: DeriveInput) -> Option<Self> {
        if d.attrs.iter().any(|a| {
            let Ok(list) = a.meta.require_list() else {
                return false;
            };
            let derives = list
                .parse_args_with(Punctuated::<Path, Token![,]>::parse_terminated)
                .unwrap();
            derives
                .iter()
                .any(|p| p == &parse_quote!(rust_sitter::Rule) || p == &parse_quote!(Rule))
        }) {
            Some(Self::from_derive_input_known(d))
        } else {
            None
        }
    }

    // Used by the proc macro directly.
    pub fn from_derive_input_known(d: DeriveInput) -> Self {
        let extras = Extras::new(&d.attrs);
        Self {
            ident: d.ident,
            attrs: d.attrs,
            extras,
            data: d.data,
        }
    }
}

/// Generate a single grammar per module.
pub fn generate_grammar(root_file: Vec<Item>) -> Value {
    let mut state = ExpansionState::default();
    // for some reason, source_file must be the first key for things to work
    state.rules_map.insert("source_file".to_string(), json!({}));

    for item in root_file {
        process_item(item, &mut state);
    }

    let language = state
        .language_rule
        .expect("Must specify exactly one root with #[language]")
        .to_string();
    state.rules_map.insert(
        "source_file".to_string(),
        state.rules_map.get(&language).unwrap().clone(),
    );
    let word_rule = state.word_rule;
    let rules_map = state.rules_map;
    let extras_list = state.extras;
    json!({
        "name": language,
        "word": word_rule,
        "rules": rules_map,
        "extras": extras_list
    })
}

#[derive(Default)]
struct ExpansionState {
    rules_map: Map<String, Value>,
    word_rule: Option<String>,
    language_rule: Option<Ident>,
    extras: Vec<Value>,
}

impl ExpansionState {
    fn set_language(&mut self, ident: &Ident) {
        if let Some(existing) = &self.language_rule {
            panic!(
                "Language rule already defined as {}:{:?}, found duplicate with {}:{:?}",
                existing,
                existing.span(),
                ident,
                ident.span(),
            );
        }
        self.language_rule = Some(ident.clone());
    }
    fn set_word(&mut self, ident: String) {
        if let Some(existing) = &self.word_rule {
            panic!("Word rule already defined as {existing}, found duplicate with {ident}",);
        }
        self.word_rule = Some(ident);
    }
    fn push_extra(&mut self, ident: &Ident) {
        self.extras.push(json!({
            "type": "SYMBOL",
            "name": ident.to_string(),
        }));
    }
}

fn process_item(item: Item, ctx: &mut ExpansionState) {
    match item {
        Item::Struct(_) | Item::Enum(_) => {
            // Try and convert it to a derive.
            let stream = item.to_token_stream();
            // stream.into_iter
            let input = syn::parse2::<DeriveInput>(stream)
                .map(RuleDerive::from_derive_input)
                .expect("Failed to parse as DeriveInput");
            if let Some(input) = input {
                // Parse the structure now.
                process_rule(input, ctx);
            }
        }
        Item::Mod(m) => {
            // Recursively process this now.
            let (_, items) = m.content.expect("Module must be inlined");
            for item in items {
                process_item(item, ctx);
            }
        }
        _ => {}
    }
}

fn process_rule(input: RuleDerive, ctx: &mut ExpansionState) {
    if input.extras.language {
        ctx.set_language(&input.ident);
    }
    // if input.extras.word {
    //     ctx.set_word(&input.ident);
    // }
    if input.extras.extra {
        ctx.push_extra(&input.ident);
    }

    let ident = input.ident;

    match input.data {
        Data::Struct(DataStruct { fields, .. }) => {
            gen_struct_or_variant(ident.to_string(), &input.attrs, fields.clone(), ctx);
        }
        Data::Enum(DataEnum { variants, .. }) => {
            variants.iter().for_each(|v| {
                gen_struct_or_variant(
                    format!("{}_{}", ident, v.ident),
                    &v.attrs,
                    v.fields.clone(),
                    ctx,
                )
            });

            let mut members: Vec<Value> = vec![];
            variants.iter().for_each(|v| {
                let variant_path = format!("{}_{}", ident.clone(), v.ident);
                members.push(json!({
                    "type": "SYMBOL",
                    "name": variant_path
                }))
            });

            let rule = json!({
                "type": "CHOICE",
                "members": members
            });

            let precs = input.extras;
            if precs.prec_left_param.is_some() || precs.prec_right_param.is_some() {
                panic!(
                    "The attributes `prec_left` and `prec_right` cannot be applied directly to an enum"
                );
            }
            let rule = precs.apply(rule);

            ctx.rules_map.insert(ident.to_string(), rule);
        }
        Data::Union(_) => panic!("Union not supported"),
    }
}

#[derive(Debug)]
pub struct Extras {
    pub prec_param: Option<Expr>,
    pub prec_left_param: Option<Expr>,
    pub prec_right_param: Option<Expr>,
    pub prec_dynamic_param: Option<Expr>,
    pub immediate: bool,
    pub token: bool,
    pub language: bool,
    pub extra: bool,
    pub word: bool,
}

impl Extras {
    fn new(attrs: &[Attribute]) -> Self {
        let prec_attr = attrs.iter().find(|attr| sitter_attr_matches(attr, "prec"));

        let prec_param = prec_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

        let prec_left_attr = attrs
            .iter()
            .find(|attr| sitter_attr_matches(attr, "prec_left"));

        let prec_left_param = prec_left_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

        let prec_right_attr = attrs
            .iter()
            .find(|attr| sitter_attr_matches(attr, "prec_right"));

        let prec_right_param = prec_right_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

        let prec_dynamic_attr = attrs
            .iter()
            .find(|attr| sitter_attr_matches(attr, "prec_dynamic"));

        let prec_dynamic_param =
            prec_dynamic_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

        let immediate = attrs
            .iter()
            .any(|attr| sitter_attr_matches(attr, "immediate"));

        let token = attrs.iter().any(|attr| sitter_attr_matches(attr, "token"));
        let extra = attrs.iter().any(|attr| sitter_attr_matches(attr, "extra"));
        let language = attrs.iter().any(|a| sitter_attr_matches(a, "language"));
        let word = attrs.iter().any(|a| sitter_attr_matches(a, "word"));

        Self {
            prec_param,
            prec_left_param,
            prec_right_param,
            prec_dynamic_param,
            immediate,
            token,
            extra,
            word,
            language,
        }
    }

    fn apply(&self, rule: serde_json::Value) -> serde_json::Value {
        let Self {
            prec_param,
            prec_left_param,
            prec_right_param,
            prec_dynamic_param,
            immediate,
            token,
            ..
        } = self;

        let rule = if let Some(Expr::Lit(lit)) = prec_param {
            if prec_left_param.is_some() || prec_right_param.is_some() {
                panic!("only one of prec, prec_left, and prec_right can be specified");
            }

            if let Lit::Int(i) = &lit.lit {
                json!({
                    "type": "PREC",
                    "value": i.base10_parse::<u32>().unwrap(),
                    "content": rule
                })
            } else {
                panic!("Expected integer literal for precedence");
            }
        } else if let Some(Expr::Lit(lit)) = prec_left_param {
            if prec_right_param.is_some() {
                panic!("only one of prec, prec_left, and prec_right can be specified");
            }

            let value = if let Lit::Int(i) = &lit.lit {
                i.base10_parse::<u32>().unwrap()
            } else {
                0
            };
            json!({
                "type": "PREC_LEFT",
                "value": value,
                "content": rule
            })
        } else if let Some(Expr::Lit(lit)) = prec_right_param {
            let value = if let Lit::Int(i) = &lit.lit {
                i.base10_parse::<u32>().unwrap()
            } else {
                0
            };
            json!({
                "type": "PREC_RIGHT",
                "value": value,
                "content": rule
            })
        } else if let Some(Expr::Lit(lit)) = prec_dynamic_param {
            if let Lit::Int(i) = &lit.lit {
                json!({
                    "type": "PREC_DYNAMIC",
                    "value": i.base10_parse::<u32>().unwrap(),
                    "content": rule
                })
            } else {
                panic!("Expected integer literal for dynamic precedence");
            }
        } else {
            rule
        };

        if *immediate && *token {
            panic!("Cannot be immediate and token");
        }

        if *immediate {
            json!({
                "type": "IMMEDIATE_TOKEN",
                "content": rule
            })
        } else if *token {
            json!({
                "type": "TOKEN",
                "content": rule,
            })
        } else {
            rule
        }
    }
}

fn gen_field(
    path: String,
    leaf_type: Type,
    attrs: Vec<Attribute>,
    ctx: &mut ExpansionState,
) -> (Value, bool) {
    let precs = Extras::new(&attrs);

    if precs.word {
        // TODO: We don't want to allow this, but because we generate a dummy `_unit` field
        // currently, we have to. Super dumb, but we can fix it later.
        ctx.set_word(path.clone());
        // panic!("Cannot specify word on a field");
    }
    if precs.language {
        panic!("Cannot specify language on a field");
    }
    let leaf_attr = attrs.iter().find(|attr| sitter_attr_matches(attr, "leaf"));

    let text_attr = attrs.iter().find(|attr| sitter_attr_matches(attr, "text"));

    if leaf_attr.is_some() && text_attr.is_some() {
        panic!("Cannot specify leaf and text at the same time");
    }

    let mut skip_over = HashSet::new();
    skip_over.insert("Spanned");
    skip_over.insert("Box");

    let (inner_type_vec, is_vec) = try_extract_inner_type(&leaf_type, "Vec", &skip_over);
    let (inner_type_option, is_option) = try_extract_inner_type(&leaf_type, "Option", &skip_over);

    if let Some(text) = text_attr {
        let input: TsInput = text.parse_args().unwrap();
        // text is only used to parse a bunch of tokens which are then not used directly. As such,
        // the type is required to be `()` or else it will fail to compile.
        match &leaf_type {
            Type::Tuple(t) if t.elems.is_empty() => {}
            _ => panic!("Unexpected type `()` is required for rust_sitter::text"),
        }
        return (precs.apply(input.evaluate().unwrap()), false);
    }

    let leaf_input = leaf_attr.and_then(|a| a.parse_args::<TsInput>().ok());

    if !is_vec && !is_option {
        if let Some(input) = leaf_input {
            ctx.rules_map
                .insert(path.clone(), precs.apply(input.evaluate().unwrap()));

            (
                json!({
                    "type": "SYMBOL",
                    "name": path
                }),
                is_option,
            )
        } else {
            let symbol_name = if let Type::Path(p) = filter_inner_type(&leaf_type, &skip_over) {
                if p.path.segments.len() == 1 {
                    p.path.segments[0].ident.to_string()
                } else {
                    panic!("Expected a single segment path");
                }
            } else {
                panic!("Expected a path");
            };

            (
                precs.apply(json!({
                    "type": "SYMBOL",
                    "name": symbol_name,
                })),
                false,
            )
        }
    } else if is_vec {
        let (field_json, field_optional) = gen_field(
            path.clone(),
            inner_type_vec,
            leaf_attr.iter().cloned().cloned().collect(),
            ctx,
        );

        let (delimited_param, repeat_non_empty) = attrs
            .iter()
            .find_map(|attr| {
                if sitter_attr_matches(attr, "sep_by") {
                    Some((Some(attr.parse_args::<TsInput>().unwrap()), false))
                } else if sitter_attr_matches(attr, "sep_by1") {
                    Some((Some(attr.parse_args::<TsInput>().unwrap()), true))
                } else if sitter_attr_matches(attr, "repeat1") {
                    Some((None, true))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| (None, false));

        // NOTE (JAB): All of this is pretty ugly, I think we can flatten some of these types
        // without losing anything.
        let delimiter_json = delimited_param.as_ref().map(|_| {
            gen_field(
                format!("{path}_vec_delimiter"),
                parse_quote!(()),
                vec![parse_quote!(#[text(#delimited_param)])],
                ctx,
            )
        });

        let field_rule_non_optional = json!({
            "type": "FIELD",
            "name": format!("{path}_vec_element"),
            "content": field_json
        });

        let field_rule = if field_optional {
            json!({
                "type": "CHOICE",
                "members": [
                    {
                        "type": "BLANK"
                    },
                    field_rule_non_optional
                ]
            })
        } else {
            field_rule_non_optional
        };

        let vec_contents = if let Some((delimiter_json, delimiter_optional)) = delimiter_json {
            let delim_made_optional = if delimiter_optional {
                json!({
                    "type": "CHOICE",
                    "members": [
                        {
                            "type": "BLANK"
                        },
                        delimiter_json
                    ]
                })
            } else {
                delimiter_json
            };

            json!({
                "type": "SEQ",
                "members": [
                    field_rule,
                    {
                        "type": if field_optional {
                            "REPEAT1"
                        } else {
                            "REPEAT"
                        },
                        "content": {
                            "type": "SEQ",
                            "members": [
                                delim_made_optional,
                                field_rule,
                            ]
                        }
                    }
                ]
            })
        } else {
            json!({
                "type": "REPEAT1",
                "content": field_rule
            })
        };

        let vec_contents = precs.apply(vec_contents);

        let contents_ident = format!("{path}_vec_contents");
        ctx.rules_map.insert(contents_ident.clone(), vec_contents);

        (
            json!({
                "type": "SYMBOL",
                "name": contents_ident,
            }),
            !repeat_non_empty,
        )
    } else {
        // is_option
        let (field_json, field_optional) = gen_field(path, inner_type_option, attrs, ctx);

        if field_optional {
            panic!("Option<Option<_>> is not supported");
        }

        (precs.apply(field_json), true)
    }
}

fn gen_struct_or_variant(
    path: String,
    attrs: &[Attribute],
    fields: Fields,
    ctx: &mut ExpansionState,
) {
    fn gen_field_optional(
        path: &str,
        field: &Field,
        ctx: &mut ExpansionState,
        ident_str: String,
    ) -> Value {
        // Produce a cleaner grammar: fields with `_` are hidden fields.
        let path = if ident_str.starts_with("_") {
            format!("_{path}_{ident_str}")
        } else {
            format!("{path}_{ident_str}")
        };
        let (field_contents, is_option) =
            gen_field(path, field.ty.clone(), field.attrs.clone(), ctx);

        let core = json!({
            "type": "FIELD",
            "name": ident_str,
            "content": field_contents
        });

        if is_option {
            json!({
                "type": "CHOICE",
                "members": [
                    {
                        "type": "BLANK"
                    },
                    core
                ]
            })
        } else {
            core
        }
    }

    let children = fields
        .iter()
        .enumerate()
        .filter_map(|(i, field)| {
            if field
                .attrs
                .iter()
                .any(|attr| sitter_attr_matches(attr, "skip"))
            {
                None
            } else {
                let ident_str = field
                    .ident
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or(format!("{i}"));

                Some(gen_field_optional(&path, field, ctx, ident_str))
            }
        })
        .collect::<Vec<Value>>();

    let base_rule = match fields {
        Fields::Unit => {
            let dummy_field = Field {
                attrs: attrs.to_owned(),
                vis: Visibility::Inherited,
                mutability: FieldMutability::None,
                ident: None,
                colon_token: None,
                ty: Type::Tuple(TypeTuple {
                    paren_token: Default::default(),
                    elems: Punctuated::new(),
                }),
            };
            gen_field_optional(&path, &dummy_field, ctx, "unit".to_owned())
        }
        _ => json!({
            "type": "SEQ",
            "members": children
        }),
    };

    let precs = Extras::new(attrs);

    ctx.rules_map.insert(path, precs.apply(base_rule));
}
