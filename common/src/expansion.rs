use std::collections::HashSet;

use super::*;
use serde_json::{Map, Value, json};
use syn::{parse::Parse, punctuated::Punctuated};

/// Generates JSON strings defining Tree Sitter grammars for every Rust Sitter
/// grammar found in the given module and recursive submodules.
pub fn generate_grammars(root_file: Vec<Item>) -> Vec<Value> {
    let mut out = vec![];
    root_file
        .iter()
        .for_each(|i| generate_all_grammars(i, &mut out));
    out
}

pub fn generate_grammars_string(root_file: Vec<Item>) -> String {
    serde_json::to_string(&generate_grammars(root_file)).unwrap()
}

fn generate_all_grammars(item: &Item, out: &mut Vec<Value>) {
    if let Item::Mod(m) = item {
        m.content
            .iter()
            .for_each(|(_, items)| items.iter().for_each(|i| generate_all_grammars(i, out)));

        if m.attrs
            .iter()
            .any(|a| a.path() == &parse_quote!(rust_sitter::grammar))
        {
            out.push(generate_grammar(m))
        }
    }
}

#[derive(Debug)]
struct Extras {
    prec_param: Option<Expr>,
    prec_left_param: Option<Expr>,
    prec_right_param: Option<Expr>,
    prec_dynamic_param: Option<Expr>,
    immediate: bool,
    token: bool,
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

        let immediate_attr = attrs
            .iter()
            .find(|attr| sitter_attr_matches(attr, "immediate"));

        let token = attrs.iter().find(|attr| sitter_attr_matches(attr, "token"));

        Self {
            prec_param,
            prec_left_param,
            prec_right_param,
            prec_dynamic_param,
            immediate: immediate_attr.is_some(),
            token: token.is_some(),
        }
    }

    fn apply(self, rule: serde_json::Value) -> serde_json::Value {
        let Self {
            prec_param,
            prec_left_param,
            prec_right_param,
            prec_dynamic_param,
            immediate,
            token,
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

        if immediate && token {
            panic!("Cannot be immediate and token");
        }

        if immediate {
            json!({
                "type": "IMMEDIATE_TOKEN",
                "content": rule
            })
        } else if token {
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
    word_rule: &mut Option<String>,
    out: &mut Map<String, Value>,
) -> (Value, bool) {
    let precs = Extras::new(&attrs);
    let leaf_attr = attrs.iter().find(|attr| sitter_attr_matches(attr, "leaf"));

    let seq_attr = attrs.iter().find(|attr| sitter_attr_matches(attr, "seq"));

    if leaf_attr.is_some() && seq_attr.is_some() {
        panic!("Cannot specify leaf and seq at the same time");
    }

    let mut skip_over = HashSet::new();
    skip_over.insert("Spanned");
    skip_over.insert("Box");

    if precs.prec_left_param.is_some() || precs.prec_right_param.is_some() {
        panic!(
            "The attributes `prec_left` and `prec_right` cannot be applied to a non-struct type"
        );
    }

    let (inner_type_vec, is_vec) = try_extract_inner_type(&leaf_type, "Vec", &skip_over);
    let (inner_type_option, is_option) = try_extract_inner_type(&leaf_type, "Option", &skip_over);

    if let Some(seq) = seq_attr {
        // Handle the seq separately.
        let inputs = seq
            .parse_args_with(Punctuated::<ExprOrCall, Token![,]>::parse_terminated)
            .unwrap();
        let mut members = vec![];
        for input in inputs {
            let (typ, expr) = match input {
                ExprOrCall::Expr(expr) => ("STRING", expr),
                ExprOrCall::Call(call) => {
                    let typ = if call.ident == "pattern" || call.ident == "re" {
                        "PATTERN"
                    } else if call.ident == "text" {
                        "STRING"
                    } else {
                        panic!("Unexpected seq input, expected one of: [pattern, re, text]");
                    };
                    (typ, call.expr)
                }
            };
            if let Expr::Lit(lit) = expr
                && let Lit::Str(s) = lit.lit
            {
                members.push(json!({
                    "type": typ,
                    "value": s.value(),
                }));
            } else {
                panic!("expr in seq must be a literal string");
            }
        }

        // seq is only used to parse a bunch of tokens which are then not used directly. As such,
        // the type is required to be `()` or else it will fail to compile.
        let ty = if is_option {
            &inner_type_option
        } else {
            &leaf_type
        };
        match ty {
            Type::Tuple(t) if t.elems.is_empty() => {}
            _ => panic!("Unexpected type `()` is required for rust_sitter::seq"),
        }
        return (
            precs.apply(json!({
                "type": "SEQ",
                "members": members,
            })),
            is_option,
        );
    }

    if attrs.iter().any(|attr| sitter_attr_matches(attr, "word")) {
        if word_rule.is_some() {
            panic!("Multiple `word` rules specified");
        }

        *word_rule = Some(path.clone());
    }

    let leaf_params = leaf_attr.and_then(|a| {
        a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
            .ok()
    });

    let pattern_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "pattern")
            .map(|p| p.expr.clone())
    });

    let text_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "text")
            .map(|p| p.expr.clone())
    });

    if pattern_param.is_some() && text_param.is_some() {
        panic!("cannot specify text and pattern in the same leaf");
    }

    if !is_vec && !is_option {
        if let Some(Expr::Lit(lit)) = pattern_param {
            if let Lit::Str(s) = &lit.lit {
                out.insert(
                    path.clone(),
                    precs.apply(json!({
                        "type": "PATTERN",
                        "value": s.value(),
                    })),
                );

                (
                    json!({
                        "type": "SYMBOL",
                        "name": path
                    }),
                    is_option,
                )
            } else {
                panic!("Expected string literal for pattern");
            }
        } else if let Some(Expr::Lit(lit)) = text_param {
            if let Lit::Str(s) = &lit.lit {
                out.insert(
                    path.clone(),
                    precs.apply(json!({
                        "type": "STRING",
                        "value": s.value(),
                    })),
                );

                (
                    json!({
                        "type": "SYMBOL",
                        "name": path
                    }),
                    is_option,
                )
            } else {
                panic!("Expected string literal for text");
            }
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
            word_rule,
            out,
        );

        let delimited_attr = attrs
            .iter()
            .find(|attr| sitter_attr_matches(attr, "delimited"));

        let delimited_params =
            delimited_attr.and_then(|a| a.parse_args_with(FieldThenParams::parse).ok());

        let delimiter_json = delimited_params.map(|p| {
            gen_field(
                format!("{path}_vec_delimiter"),
                p.field.ty,
                p.field.attrs,
                word_rule,
                out,
            )
        });

        let repeat_attr = attrs
            .iter()
            .find(|attr| sitter_attr_matches(attr, "repeat"));

        let repeat_params = repeat_attr.and_then(|a| {
            a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
                .ok()
        });

        let repeat_non_empty = repeat_params
            .and_then(|p| {
                p.iter()
                    .find(|param| param.path == "non_empty")
                    .map(|p| p.expr.clone())
            })
            .map(|e| e == syn::parse_quote!(true))
            .unwrap_or(false);

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
        out.insert(contents_ident.clone(), vec_contents);

        (
            json!({
                "type": "SYMBOL",
                "name": contents_ident,
            }),
            !repeat_non_empty,
        )
    } else {
        // is_option
        let (field_json, field_optional) =
            gen_field(path, inner_type_option, attrs, word_rule, out);

        if field_optional {
            panic!("Option<Option<_>> is not supported");
        }

        (precs.apply(field_json), true)
    }
}

fn gen_struct_or_variant(
    path: String,
    attrs: Vec<Attribute>,
    fields: Fields,
    out: &mut Map<String, Value>,
    word_rule: &mut Option<String>,
) {
    fn gen_field_optional(
        path: &str,
        field: &Field,
        word_rule: &mut Option<String>,
        out: &mut Map<String, Value>,
        ident_str: String,
    ) -> Value {
        // Produce a cleaner grammar: fields with `_` are hidden fields.
        let path = if ident_str.starts_with("_") {
            format!("_{path}_{ident_str}")
        } else {
            format!("{path}_{ident_str}")
        };
        let (field_contents, is_option) =
            gen_field(path, field.ty.clone(), field.attrs.clone(), word_rule, out);

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

                Some(gen_field_optional(&path, field, word_rule, out, ident_str))
            }
        })
        .collect::<Vec<Value>>();

    let precs = Extras::new(&attrs);

    let base_rule = match fields {
        Fields::Unit => {
            let dummy_field = Field {
                attrs: attrs.clone(),
                vis: Visibility::Inherited,
                mutability: FieldMutability::None,
                ident: None,
                colon_token: None,
                ty: Type::Tuple(TypeTuple {
                    paren_token: Default::default(),
                    elems: Punctuated::new(),
                }),
            };
            gen_field_optional(&path, &dummy_field, word_rule, out, "unit".to_owned())
        }
        _ => json!({
            "type": "SEQ",
            "members": children
        }),
    };

    let rule = precs.apply(base_rule);

    out.insert(path, rule);
}

pub fn generate_grammar(module: &ItemMod) -> Value {
    let mut rules_map = Map::new();
    // for some reason, source_file must be the first key for things to work
    rules_map.insert("source_file".to_string(), json!({}));

    let mut extras_list = vec![];
    let attr = module
        .attrs
        .iter()
        .find(|a| a.path() == &syn::parse_quote!(rust_sitter::grammar))
        .expect("Each grammar must have a name");
    let grammar_name_expr = attr
        .parse_args_with(Punctuated::<Expr, Token![,]>::parse_terminated)
        .expect("Inputs should be a comma separated list");
    if grammar_name_expr.is_empty() {
        panic!("Expected a string literal for grammar name");
        // return Err(syn::Error::new(
        //     Span::call_site(),
        //     "Expected a string literal grammar name",
        // ));
    }
    if grammar_name_expr.len() > 2 {
        panic!("Expected at most two inputs");
    }
    let grammar_name = if let Expr::Lit(ExprLit {
        attrs: _,
        lit: Lit::Str(s),
    }) = grammar_name_expr.first().unwrap()
    {
        s.value()
    } else {
        panic!("Expected a string literal grammar name");
    };

    let _should_parse = if let Some(Expr::Lit(ExprLit {
        attrs: _,
        lit: Lit::Bool(b),
    })) = grammar_name_expr.last()
    {
        b.value()
    } else {
        false
    };

    let (_, contents) = module.content.as_ref().unwrap();

    let root_type = contents
        .iter()
        .find_map(|item| match item {
            Item::Enum(ItemEnum { ident, attrs, .. })
            | Item::Struct(ItemStruct { ident, attrs, .. }) => {
                if attrs
                    .iter()
                    .any(|attr| attr.path() == &syn::parse_quote!(rust_sitter::language))
                {
                    Some(ident.clone())
                } else {
                    None
                }
            }
            _ => None,
        })
        .expect("Each parser must have the root type annotated with `#[rust_sitter::language]`")
        .to_string();

    // Optionally locate the rule annotated with `#[rust_sitter::word]`.
    let mut word_rule = None;
    contents.iter().for_each(|c| {
        let (symbol, attrs) = match c {
            Item::Enum(e) => {
                e.variants.iter().for_each(|v| {
                    gen_struct_or_variant(
                        format!("{}_{}", e.ident, v.ident),
                        v.attrs.clone(),
                        v.fields.clone(),
                        &mut rules_map,
                        &mut word_rule,
                    )
                });

                let mut members: Vec<Value> = vec![];
                e.variants.iter().for_each(|v| {
                    let variant_path = format!("{}_{}", e.ident.clone(), v.ident);
                    members.push(json!({
                        "type": "SYMBOL",
                        "name": variant_path
                    }))
                });

                let rule = json!({
                    "type": "CHOICE",
                    "members": members
                });

                let precs = Extras::new(&e.attrs);
                if precs.prec_left_param.is_some() || precs.prec_right_param.is_some() {
                    panic!(
                        "The attributes `prec_left` and `prec_right` cannot be applied directly to an enum"
                    );
                }
                let rule = precs.apply(rule);

                rules_map.insert(e.ident.to_string(), rule);

                (e.ident.to_string(), e.attrs.clone())
            }

            Item::Struct(s) => {
                gen_struct_or_variant(
                    s.ident.to_string(),
                    s.attrs.clone(),
                    s.fields.clone(),
                    &mut rules_map,
                    &mut word_rule,
                );

                (s.ident.to_string(), s.attrs.clone())
            }

            _ => return,
        };

        if attrs
            .iter()
            .any(|a| sitter_attr_matches(a, "extra"))
        {
            extras_list.push(json!({
                "type": "SYMBOL",
                "name": symbol
            }));
        }
    });

    rules_map.insert(
        "source_file".to_string(),
        rules_map.get(&root_type).unwrap().clone(),
    );

    json!({
        "name": grammar_name,
        "word": word_rule,
        "rules": rules_map,
        "extras": extras_list
    })
}
