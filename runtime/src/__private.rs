//! # DO NOT USE THIS MODULE!
//!
//! This module contains functions for use in the expanded macros produced by rust-sitter.
//! They need to be public so they can be accessed at all (\*cough\* macro hygiene), but
//! they are not intended to actually be called in any other circumstance.

use crate::{tree_sitter, Extract};

pub fn extract_struct_or_variant<T>(
    node: tree_sitter::Node,
    construct_expr: impl Fn(&mut Option<tree_sitter::TreeCursor>, &mut usize, &mut tree_sitter::Point) -> T,
) -> T {
    let mut parent_cursor = node.walk();
    construct_expr(
        &mut if parent_cursor.goto_first_child() {
            Some(parent_cursor)
        } else {
            None
        },
        &mut node.start_byte(),
        &mut node.start_position(),
    )
}

pub fn extract_field<LT: Extract<T>, T>(
    cursor_opt: &mut Option<tree_sitter::TreeCursor>,
    source: &[u8],
    last_idx: &mut usize,
    last_pt: &mut tree_sitter::Point,
    field_name: &str,
    closure_ref: Option<LT::LeafFn<'_>>,
) -> T {
    if let Some(cursor) = cursor_opt.as_mut() {
        loop {
            let n = cursor.node();
            if let Some(name) = cursor.field_name() {
                if name == field_name {
                    let out = LT::extract(Some(n), source, *last_idx, *last_pt, closure_ref);

                    if !cursor.goto_next_sibling() {
                        *cursor_opt = None;
                    };

                    *last_idx = n.end_byte();
                    *last_pt = n.end_position();

                    return out;
                } else {
                    return LT::extract(None, source, *last_idx, *last_pt, closure_ref);
                }
            } else {
                *last_idx = n.end_byte();
                *last_pt = n.end_position();
            }

            if !cursor.goto_next_sibling() {
                return LT::extract(None, source, *last_idx, *last_pt, closure_ref);
            }
        }
    } else {
        LT::extract(None, source, *last_idx, *last_pt, closure_ref)
    }
}

pub fn skip_text(cursor_opt: &mut Option<tree_sitter::TreeCursor>, field_name: &str) {
    if let Some(cursor) = cursor_opt.as_mut() {
        loop {
            if let Some(name) = cursor.field_name() {
                if name == field_name {
                    if !cursor.goto_next_sibling() {
                        *cursor_opt = None;
                        return;
                    }
                } else {
                    return;
                }
            } else {
                return;
            }
        }
    }
}

pub fn parse<T: Extract<T>>(
    input: &str,
    language: impl Fn() -> tree_sitter::Language,
) -> core::result::Result<T, Vec<crate::error::ParseError>> {
    let mut parser = crate::tree_sitter::Parser::new();
    parser.set_language(&language()).unwrap();
    let tree = parser.parse(input, None).expect("Failed to parse");
    let root_node = tree.root_node();

    if root_node.has_error() {
        let mut errors = vec![];
        crate::error::collect_parsing_errors(&root_node, input.as_bytes(), &mut errors);

        Err(errors)
    } else {
        Ok(<T as crate::Extract<_>>::extract(
            Some(root_node),
            input.as_bytes(),
            0,
            Default::default(),
            None,
        ))
    }
}
