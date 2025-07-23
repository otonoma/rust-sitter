//! # DO NOT USE THIS MODULE!
//!
//! This module contains functions for use in the expanded macros produced by rust-sitter.
//! They need to be public so they can be accessed at all (\*cough\* macro hygiene), but
//! they are not intended to actually be called in any other circumstance.

use crate::{
    Extract, Span,
    extract::{ExtractError, Result},
    tree_sitter,
};

pub fn extract_struct_or_variant<T>(
    node: tree_sitter::Node,
    construct_expr: impl Fn(&mut ExtractStructState<'_>) -> Result<T>,
) -> Result<T> {
    let mut parent_cursor = node.walk();
    let mut state = ExtractStructState {
        cursor: if parent_cursor.goto_first_child() {
            Some(parent_cursor)
        } else {
            None
        },
        last_idx: node.start_byte(),
        last_pt: node.start_position(),
        error: ExtractError::empty(),
    };
    construct_expr(&mut state)
}

pub struct ExtractStructState<'a> {
    cursor: Option<tree_sitter::TreeCursor<'a>>,
    last_idx: usize,
    last_pt: tree_sitter::Point,
    error: ExtractError,
}

// impl<'a> ExtractStructState<'a> {
//     fn extract_node<LT: Extract<T>, T>(
//         &mut self,
//         node: tree_sitter::Node,
//         source: &[u8],
//         closure_ref: Option<LT::LeafFn<'_>>,
//     ) -> Result<T> {
//     }
// }

// pub struct TryExtractState {
//     pub span: Span,
//     pub err: Option<ExtractError>,
// }

// pub fn try_extract<LT: Extract<T>, T>(
//     err_state: &mut TryExtractState,
//     node: Option<tree_sitter::Node>,
//     source: &[u8],
//     last_idx: usize,
//     last_pt: tree_sitter::Point,
//     leaf_fn: Option<LT::LeafFn<'_>>,
// ) -> Option<T> {
//     // TODO: Double check this.
//     err_state.span.end_byte = last_idx;
//     match LT::extract(node, source, last_idx, last_pt, leaf_fn) {
//         Ok(t) => Some(t),
//         Err(err) => {
//             todo!()
//         }
//     }
// }

pub fn extract_field<LT: Extract<T>, T: std::fmt::Debug>(
    state: &mut ExtractStructState<'_>,
    source: &[u8],
    field_name: &str,
    closure_ref: Option<LT::LeafFn<'_>>,
) -> Result<T> {
    dbg!(field_name);
    if let Some(cursor) = state.cursor.as_mut() {
        loop {
            let n = cursor.node();
            println!("Extracting node from text: {} - {}", n.utf8_text(source).unwrap(), n.to_sexp());
            if n.is_error() {
                println!("Processing error...");
                // Try and parse it anyway, returning the result if we manage to get it.
                if !cursor.goto_first_child() {
                    state.cursor = None;
                    state.last_idx = n.end_byte();
                    state.last_pt = n.end_position();
                    return Err(ExtractError::new(n, field_name.to_owned()));
                }
                let n = cursor.node();
                let out = LT::extract(Some(n), source, state.last_idx, state.last_pt, closure_ref)?;
                // let out = match out {
                //     Ok(out) => {
                //         // ???; I guess this would be only possible in the wrapped type case.
                //         Some(out)
                //     }
                //     Err(e) => {
                //         state.error.merge(e);
                //         None
                //     }
                // };
                // if !cursor.goto_next_sibling() {
                //     state.cursor = None;
                // };

                state.last_idx = n.end_byte();
                state.last_pt = n.end_position();

                return Ok(out);
            } else if let Some(name) = cursor.field_name() {
                if name == field_name {
                    // TODO: Need to keep going if it fails.
                    let out =
                        LT::extract(Some(n), source, state.last_idx, state.last_pt, closure_ref)?;

                    if !cursor.goto_next_sibling() {
                        dbg!(name);
                        state.cursor = None;
                    };

                    state.last_idx = n.end_byte();
                    state.last_pt = n.end_position();

                    return Ok(dbg!(out));
                } else {
                    return LT::extract(None, source, state.last_idx, state.last_pt, closure_ref);
                }
            } else {
                state.last_idx = n.end_byte();
                state.last_pt = n.end_position();
            }

            if !cursor.goto_next_sibling() {
                return LT::extract(None, source, state.last_idx, state.last_pt, closure_ref);
            }
        }
    } else {
        LT::extract(None, source, state.last_idx, state.last_pt, closure_ref)
    }
}

// TODO: Handle errors in this one too.
pub fn skip_text(state: &mut ExtractStructState<'_>, field_name: &str) -> Result<()> {
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
) -> core::result::Result<T, crate::extract::ExtractError> {
    let mut parser = crate::tree_sitter::Parser::new();
    parser.set_language(&language()).unwrap();
    let tree = parser.parse(input, None).expect("Failed to parse");
    let root_node = tree.root_node();

     if root_node.has_error() {
         let mut errors = vec![];
         crate::error::collect_parsing_errors(&root_node, input.as_bytes(), &mut errors);
         for error in errors {
             println!("{error}");
         }
     }
    <T as crate::Extract<_>>::extract(
        Some(root_node),
        input.as_bytes(),
        0,
        Default::default(),
        None,
    )
    // }
}
