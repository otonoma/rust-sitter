//! # DO NOT USE THIS MODULE!
//!
//! This module contains functions for use in the expanded macros produced by rust-sitter.
//! They need to be public so they can be accessed at all (\*cough\* macro hygiene), but
//! they are not intended to actually be called in any other circumstance.

use crate::{
    Extract,
    extract::{ExtractContext, ExtractError, Result},
};

pub fn extract_struct_or_variant<T>(
    node: tree_sitter::Node,
    construct_expr: impl for<'t> Fn(&mut ExtractStructState<'t>) -> Result<'t, T>,
) -> Result<T> {
    let mut parent_cursor = node.walk();
    let mut state = ExtractStructState {
        // cursor: Some(parent_cursor),
        cursor: if parent_cursor.goto_first_child() {
            Some(parent_cursor)
        } else {
            None
        },
        last_idx: node.start_byte(),
        last_pt: node.start_position(),
        // error: ExtractError::empty(),
    };
    construct_expr(&mut state)
}

pub struct ExtractStructState<'tree> {
    cursor: Option<tree_sitter::TreeCursor<'tree>>,
    last_idx: usize,
    last_pt: tree_sitter::Point,
    // TODO: Use this.
    // error: ExtractError,
}

pub fn extract_field<'tree, LT: Extract<T>, T>(
    state: &mut ExtractStructState<'tree>,
    source: &[u8],
    field_name: &str,
    closure_ref: Option<LT::LeafFn<'_>>,
) -> Result<'tree, T> {
    let mut ctx = ExtractContext {
        last_idx: state.last_idx,
        last_pt: state.last_pt,
        field_name,
        node_kind: "",
    };
    if let Some(cursor) = state.cursor.as_mut() {
        loop {
            let n = cursor.node();
            ctx.node_kind = n.kind();
            if n.is_error() {
                // println!("Processing error... {}, {}", n.kind(), field_name);
                // Try and parse it anyway, returning the result if we manage to get it.
                if !cursor.goto_first_child() {
                    state.cursor = None;
                    ctx.last_idx = n.end_byte();
                    ctx.last_pt = n.end_position();
                    return Err(ExtractError::new(n, field_name.to_owned()));
                }
                let n = cursor.node();
                let out = LT::extract(&mut ctx, Some(n), source, closure_ref)?;
                ctx.last_idx = n.end_byte();
                ctx.last_pt = n.end_position();

                return Ok(out);
            } else if let Some(name) = cursor.field_name() {
                if name == field_name {
                    // TODO: Need to keep going if it fails.
                    let out = LT::extract(&mut ctx, Some(n), source, closure_ref)?;

                    if !cursor.goto_next_sibling() {
                        state.cursor = None;
                    };

                    ctx.last_idx = n.end_byte();
                    ctx.last_pt = n.end_position();

                    return Ok(out);
                } else {
                    return LT::extract(&mut ctx, None, source, closure_ref);
                }
            } else {
                state.last_idx = n.end_byte();
                state.last_pt = n.end_position();
            }

            if !cursor.goto_next_sibling() {
                return LT::extract(&mut ctx, None, source, closure_ref);
            }
        }
    } else {
        LT::extract(&mut ctx, None, source, closure_ref)
    }
}

// TODO: Handle errors in this one too.
pub fn skip_text<'tree>(
    state: &mut ExtractStructState<'tree>,
    field_name: &str,
) -> Result<'tree, ()> {
    if let Some(cursor) = state.cursor.as_mut() {
        loop {
            if let Some(name) = cursor.field_name() {
                if name == field_name {
                    if !cursor.goto_next_sibling() {
                        state.cursor = None;
                        return Ok(());
                    }
                } else {
                    return Ok(());
                }
            } else {
                return Ok(());
            }
        }
    }

    Ok(())
}

pub fn parse<T: Extract<T>>(
    input: &str,
    language: impl Fn() -> tree_sitter::Language,
) -> crate::ParseResult<T> {
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&language()).unwrap();
    if matches!(std::env::var("RUST_SITTER_PARSER_LOG").as_deref(), Ok("1")) {
        parser.set_logger(Some(Box::new(|_t, m| log::debug!("parser::{m}"))));
    }
    let tree = parser.parse(input, None).expect("Failed to parse");
    let root_node = tree.root_node();

    println!("{root_node}");

    let mut errors = vec![];
    if root_node.has_error() {
        crate::error::collect_parsing_errors(&root_node, &mut errors);
    }
    let mut ctx = ExtractContext {
        last_pt: Default::default(),
        last_idx: 0,
        field_name: "root",
        node_kind: "source_file",
    };
    let result =
        <T as crate::Extract<_>>::extract(&mut ctx, Some(root_node), input.as_bytes(), None);
    #[allow(clippy::manual_ok_err)]
    let result = match result {
        Err(_e) => {
            // These are actually not really useful yet.
            // e.accumulate_parse_errors(&mut errors);
            None
        }
        Ok(o) => Some(o),
    };
    crate::ParseResult { result, errors }
}
