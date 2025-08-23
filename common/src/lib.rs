use proc_macro2::Span;
use quote::ToTokens;
use rust_sitter_types::grammar::RuleDef;
use std::collections::HashSet;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    *,
};

pub mod expansion;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameValueExpr {
    pub path: Ident,
    pub eq_token: Token![=],
    pub expr: Expr,
}

impl Parse for NameValueExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(NameValueExpr {
            path: input.parse()?,
            eq_token: input.parse()?,
            expr: input.parse()?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldThenParams {
    pub field: Field,
    pub comma: Option<Token![,]>,
    pub params: Punctuated<NameValueExpr, Token![,]>,
}

impl Parse for FieldThenParams {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let field = Field::parse_unnamed(input)?;
        let comma: Option<Token![,]> = input.parse()?;
        let params = if comma.is_some() {
            Punctuated::parse_terminated_with(input, NameValueExpr::parse)?
        } else {
            Punctuated::new()
        };

        Ok(FieldThenParams {
            field,
            comma,
            params,
        })
    }
}

/// tree-sitter input parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TsInput {
    pub expr: Expr,
}

impl Parse for TsInput {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            expr: input.parse()?,
        })
    }
}

impl ToTokens for TsInput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.expr.to_tokens(tokens);
    }
}

impl TsInput {
    fn new(expr: &Expr) -> Self {
        Self { expr: expr.clone() }
    }
    pub fn evaluate(&self) -> Result<RuleDef> {
        fn get_str(e: &Expr) -> Result<String> {
            let s = match e {
                Expr::Lit(ExprLit {
                    attrs: _,
                    lit: Lit::Str(f),
                }) => f,
                _ => return Err(syn::Error::new(e.span(), "expected a string")),
            };
            Ok(s.value())
        }
        fn get_arg(p: &Punctuated<Expr, Token![,]>, i: usize, expected: usize) -> Result<&Expr> {
            assert!(i < expected);
            if p.len() != expected {
                // TODO: Fix the span
                return Err(syn::Error::new(p.span(), "Too many arguments"));
            }
            Ok(p.get(i).unwrap())
        }
        let def = match &self.expr {
            Expr::Lit(ExprLit {
                attrs: _,
                lit: Lit::Str(s),
            }) => RuleDef::STRING { value: s.value() },
            Expr::Call(ExprCall {
                attrs: _,
                func,
                paren_token: _,
                args,
            }) => {
                let name = match &**func {
                    Expr::Path(ExprPath {
                        attrs: _,
                        qself: _,
                        path,
                    }) => path.require_ident()?.to_string(),
                    k => return Err(syn::Error::new(k.span(), "Expected path")),
                };
                match name.as_str() {
                    "optional" => {
                        let inner = Self::new(get_arg(args, 0, 1)?);
                        let members = vec![inner.evaluate()?, RuleDef::BLANK];
                        RuleDef::CHOICE { members }
                    }
                    "seq" => {
                        let mut members = vec![];
                        for arg in args {
                            let ts = Self::new(arg);
                            members.push(ts.evaluate()?);
                        }
                        RuleDef::SEQ { members }
                    }
                    "choice" => {
                        let mut members = vec![];
                        for arg in args {
                            let ts = Self::new(arg);
                            members.push(ts.evaluate()?);
                        }
                        RuleDef::CHOICE { members }
                    }
                    "re" | "pattern" => RuleDef::PATTERN {
                        value: get_str(get_arg(args, 0, 1)?)?,
                        flags: None,
                    },
                    "text" => RuleDef::STRING {
                        value: get_str(get_arg(args, 0, 1)?)?,
                    },
                    "token" => {
                        let inner = Self::new(get_arg(args, 0, 1)?);
                        let content = Box::new(inner.evaluate()?);
                        RuleDef::TOKEN { content }
                    }
                    "immediate" => {
                        let inner = Self::new(get_arg(args, 0, 1)?);
                        let content = Box::new(inner.evaluate()?);
                        RuleDef::IMMEDIATE_TOKEN { content }
                    }
                    // nodes can be double wrapped in fields, although I'm not sure what happens
                    // when you ask the cursor for the field name? May not be possible to handle
                    // that in this case.
                    "field" => {
                        let name = get_str(get_arg(args, 0, 2)?)?;
                        let inner = Self::new(get_arg(args, 1, 2)?);
                        let content = Box::new(inner.evaluate()?);
                        RuleDef::FIELD { name, content }
                    }
                    k => {
                        return Err(syn::Error::new(
                            func.span(),
                            format!("Unexpected function call `{k}`"),
                        ));
                    }
                }
            }
            Expr::Path(ExprPath {
                attrs: _,
                qself: _,
                path,
            }) => {
                let ident = path.require_ident()?;
                RuleDef::SYMBOL {
                    name: ident.to_string(),
                }
            }
            k => {
                return Err(syn::Error::new(
                    k.span(),
                    format!("Unexpected input type: {k:?}"),
                ));
            }
        };
        Ok(def)
    }
}

pub fn sitter_attr_matches(attr: &Attribute, name: &str) -> bool {
    let path = attr.path();
    if path.segments.len() == 1 {
        path.segments[0].ident == name
    } else if path.segments.len() == 2 {
        // This is no longer possible, we can clean this up.
        path.segments[0].ident == "rust_sitter" && path.segments[1].ident == name
    } else {
        false
    }
}

pub fn try_extract_inner_type(
    ty: &Type,
    inner_of: &str,
    skip_over: &HashSet<&str>,
) -> (Type, bool) {
    if let Type::Path(p) = &ty {
        let type_segment = p.path.segments.last().unwrap();
        if type_segment.ident == inner_of {
            let leaf_type = if let PathArguments::AngleBracketed(p) = &type_segment.arguments {
                if let GenericArgument::Type(t) = p.args.first().unwrap().clone() {
                    t
                } else {
                    panic!("Argument in angle brackets must be a type")
                }
            } else {
                panic!("Expected angle bracketed path");
            };

            (leaf_type, true)
        } else if skip_over.contains(type_segment.ident.to_string().as_str()) {
            if let PathArguments::AngleBracketed(p) = &type_segment.arguments {
                if let GenericArgument::Type(t) = p.args.first().unwrap().clone() {
                    try_extract_inner_type(&t, inner_of, skip_over)
                } else {
                    panic!("Argument in angle brackets must be a type")
                }
            } else {
                panic!("Expected angle bracketed path");
            }
        } else {
            (ty.clone(), false)
        }
    } else {
        (ty.clone(), false)
    }
}

pub fn filter_inner_type(ty: &Type, skip_over: &HashSet<&str>) -> Type {
    if let Type::Path(p) = &ty {
        let type_segment = p.path.segments.last().unwrap();
        if skip_over.contains(type_segment.ident.to_string().as_str()) {
            if let PathArguments::AngleBracketed(p) = &type_segment.arguments {
                if let GenericArgument::Type(t) = p.args.first().unwrap().clone() {
                    filter_inner_type(&t, skip_over)
                } else {
                    panic!("Argument in angle brackets must be a type")
                }
            } else {
                panic!("Expected angle bracketed path");
            }
        } else {
            ty.clone()
        }
    } else {
        ty.clone()
    }
}

pub fn wrap_leaf_type(ty: &Type, skip_over: &HashSet<&str>) -> Type {
    let mut ty = ty.clone();
    if let Type::Path(p) = &mut ty {
        let type_segment = p.path.segments.last_mut().unwrap();
        if skip_over.contains(type_segment.ident.to_string().as_str()) {
            if let PathArguments::AngleBracketed(args) = &mut type_segment.arguments {
                for a in args.args.iter_mut() {
                    if let syn::GenericArgument::Type(t) = a {
                        *t = wrap_leaf_type(t, skip_over);
                    }
                }

                ty
            } else {
                panic!("Expected angle bracketed path");
            }
        } else {
            parse_quote!(rust_sitter::WithLeaf<#ty, _>)
        }
    } else {
        parse_quote!(rust_sitter::WithLeaf<#ty, _>)
    }
}
