use quote::ToTokens;
use rust_sitter_common::sitter_attr_matches;
use syn::{spanned::Spanned, *};

pub fn expand_extract(input: DeriveInput) -> Result<proc_macro2::TokenStream> {
    let ident = &input.ident;
    // Find the with symbol, or produce an error.
    let with = input
        .attrs
        .iter()
        .find(|a| sitter_attr_matches(a, "with"))
        .ok_or_else(|| Error::new(input.span(), "Missing #[with(...)}"))?;

    let with = with.parse_args::<Expr>()?;

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
                let text = node.utf8_text(source).unwrap();
                let result: ::rust_sitter::extract::__ExtractWithMacro<#ident> = #with(text).into();
                result.extract(ctx)
            }
        }
    };

    Ok(extract_impl.to_token_stream())
}
