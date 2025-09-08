//! # DO NOT USE THIS MODULE!
//!
//! This module contains functions for use in the expanded macros produced by rust-sitter.
//! They need to be public so they can be accessed at all (\*cough\* macro hygiene), but
//! they are not intended to actually be called in any other circumstance.

use crate::{
    Extract, ExtractContext, Extractor,
    extract::{ExtractFieldContext, ExtractFieldIterator, Result},
};
use log::{debug, trace};

pub fn extract_struct_or_variant<'tree, T>(
    struct_name: &'static str,
    node: tree_sitter::Node<'tree>,
    construct_expr: impl for<'t> Fn(&mut ExtractStructState<'t>) -> Result<'t, T>,
) -> Result<'tree, T> {
    debug!("extract_struct_or_variant node.kind={}", node.kind());
    trace!("extract_struct_or_variant node={}", node);
    trace!(
        "extract_struct_or_variant node.child_count={}",
        node.child_count()
    );
    let mut parent_cursor = node.walk();
    let has_children = parent_cursor.goto_first_child();
    let mut state = ExtractStructState {
        struct_name,
        cursor: Some(parent_cursor),
        has_children,
        last_idx: node.start_byte(),
        last_pt: node.start_position(),
        // error: ExtractError::empty(),
    };
    construct_expr(&mut state)
}

pub struct ExtractStructState<'tree> {
    struct_name: &'static str,
    cursor: Option<tree_sitter::TreeCursor<'tree>>,
    has_children: bool,
    last_idx: usize,
    last_pt: tree_sitter::Point,
    // TODO: Use this.
    // error: ExtractError,
}

pub fn extract_field<'tree, T: Extract, E: Extractor<T>>(
    extractor: E,
    leaf_fn: T::LeafFn,
    state: &mut ExtractStructState<'tree>,
    field_state: ExtractFieldContext,
    source: &[u8],
    field_name: &'static str,
) -> Result<'tree, T::Output> {
    debug!(
        "extract_field struct_name={} field_name={field_name}",
        state.struct_name
    );
    let mut ctx = ExtractContext {
        last_idx: state.last_idx,
        last_pt: state.last_pt,
        field_name,
        struct_name: state.struct_name,
    };
    if state.has_children {
        if let Some(cursor) = state.cursor.as_mut() {
            trace!("extract_field has_children: {}", cursor.node());
            let mut iter = ExtractFieldIterator::new(
                field_state,
                cursor,
                state.struct_name,
                field_name,
                source,
            );

            // Start the iterator.
            // Iteration requires knowing if there is a valid starting state or not.
            iter.advance_state()?;

            let result = extractor.do_extract_field(&mut ctx, &mut iter, source, leaf_fn)?;
            iter.finalize()?;
            Ok(result)
        } else {
            extractor.do_extract(&mut ctx, None, source, leaf_fn)
        }
    } else if let Some(cursor) = state.cursor.as_mut() {
        let n = cursor.node();
        if !cursor.goto_next_sibling() {
            state.cursor = None;
        }
        extractor.do_extract(&mut ctx, Some(n), source, leaf_fn)
    } else {
        extractor.do_extract(&mut ctx, None, source, leaf_fn)
    }
}

// TODO: Handle errors in this one too.
pub fn skip_text<'tree>(
    state: &mut ExtractStructState<'tree>,
    field_name: &'static str,
) -> Result<'tree, ()> {
    debug!(
        "skip field: {field_name:?}, has cursor: {}",
        state.cursor.is_some()
    );
    if let Some(cursor) = state.cursor.as_mut() {
        debug!(
            "skip field: expects: {field_name:?}, has: {:?}",
            cursor.field_name()
        );
        loop {
            if cursor.node().is_extra() {
                if !cursor.goto_next_sibling() {
                    state.cursor = None;
                    return Ok(());
                }
                continue;
            }
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

pub fn parse<T: crate::Language>(
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

    let mut errors = vec![];
    if root_node.has_error() {
        crate::error::collect_parsing_errors(&root_node, &mut errors);
    }
    let mut ctx = ExtractContext {
        last_pt: Default::default(),
        last_idx: 0,
        field_name: "root",
        struct_name: T::rule_name(),
    };
    let result = <T as crate::Extract>::extract(&mut ctx, Some(root_node), input.as_bytes(), ());
    let result = match result {
        Err(e) => {
            // These are actually not really useful yet.
            e.accumulate_parse_errors(&mut errors);
            None
        }
        Ok(o) => Some(o),
    };
    crate::ParseResult { result, errors }
}
