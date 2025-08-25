use crate::error::ExtractError;

use super::Result;
use log::{debug, trace};

pub struct ExtractFieldIterator<'cursor, 'tree: 'cursor> {
    pub(crate) cursor: &'cursor mut tree_sitter::TreeCursor<'tree>,
    pub(crate) field_name: &'static str,
    pub(crate) struct_name: &'static str,
    pub(crate) ctx: ExtractFieldContext,
    pub(crate) source: &'cursor [u8],
    pub(crate) current: NodeIterState<'tree>,
}

pub struct ExtractFieldContext {
    state_fn: fn(u32) -> ExtractFieldState,
    state: u32,
    num_states: u32,
    optional: bool,
}

impl ExtractFieldContext {
    pub fn new(num_states: u32, optional: bool, state_fn: fn(u32) -> ExtractFieldState) -> Self {
        Self {
            state_fn,
            state: 0,
            num_states,
            optional,
        }
    }
}

#[derive(Debug)]
pub enum ExtractFieldState {
    // expected string, is_named, is_optional
    Str(&'static str, bool, bool),
    // Current implementation only really supports doing this with a list of strings.
    Choice(&'static [(&'static str, bool)], bool),
    Complete,
    // State went too far.
    Overflow,
}

impl<'cursor, 'tree: 'cursor> ExtractFieldIterator<'cursor, 'tree> {
    fn advance_node(&mut self) {
        loop {
            if self.cursor.node().is_extra() {
                if !self.cursor.goto_next_sibling() {
                    return;
                }
                continue;
            }
            return;
        }
    }

    pub fn advance_state(&mut self) -> Result<'tree, ()> {
        if self.current == NodeIterState::Complete {
            debug!("advance_state: verifying completion");
            self.finalize()?;
            return Ok(());
        }
        self.advance_node();
        let n = self.cursor.node();
        debug!(
            "advance_state: field_name={}, cursor.field_name={:?}, state={}, num_states={}, optional={}, node={}, node.kind={}",
            self.field_name,
            self.cursor.field_name(),
            self.ctx.state,
            self.ctx.num_states,
            self.ctx.optional,
            n,
            n.kind()
        );

        trace!(
            "advance_state: node_string={}",
            n.utf8_text(self.source).unwrap()
        );

        let state = (self.ctx.state_fn)(self.ctx.state);
        self.ctx.state += 1;
        debug!("advance_state: got state={:?}", state);
        match state {
            ExtractFieldState::Str(expected, named, optional) => {
                let cursor_field = self.cursor.field_name();
                let field_name = self.field_name;
                if cursor_field != Some(field_name) {
                    debug!("advance_state: field names didn't match");
                    // Check if we have an optional overall.
                    self.handle_optional_err(|| {
                        format!(
                            "fields didn't match, cursor had: {:?}, expected: {}",
                            cursor_field, field_name
                        )
                    })?;
                    return Ok(());
                }
                if n.kind() == expected && n.is_named() == named {
                    debug!("advance_state: state matched, advancing iteration");
                    // advance the cursor and return the current node.
                    self.cursor.goto_next_sibling();
                    self.current = NodeIterState::Node(Some(n));
                    Ok(())
                } else if optional {
                    debug!("advance_state: state didn't match, but optional, skipping");
                    self.current = NodeIterState::Node(None);
                    Ok(())
                } else {
                    self.handle_optional_err(|| "state didn't match".into())?;
                    Ok(())
                }
            }
            ExtractFieldState::Choice(values, optional) => {
                let cursor_field = self.cursor.field_name();
                let field_name = self.field_name;
                if cursor_field != Some(field_name) {
                    debug!("advance_state: field names didn't match");
                    self.handle_optional_err(|| {
                        format!(
                            "fields didn't match, cursor had: {:?}, expected: {}",
                            cursor_field, field_name
                        )
                    })?;
                    return Ok(());
                }
                for (value, named) in values {
                    if n.kind() == *value && n.is_named() == *named {
                        // Found one.
                        self.cursor.goto_next_sibling();
                        self.current = NodeIterState::Node(Some(n));
                        return Ok(());
                    }
                }
                if optional {
                    self.current = NodeIterState::Node(None);
                    Ok(())
                } else {
                    self.handle_optional_err(|| "none of the choice values matched".into())?;
                    Ok(())
                }
            }
            ExtractFieldState::Complete => {
                debug!("advance_state: got complete state");
                self.current = NodeIterState::Complete;
                Ok(())
            }
            ExtractFieldState::Overflow => {
                self.handle_optional_err(|| "state overflowed".into())?;
                Ok(())
            }
        }
    }

    pub fn next_node(&mut self) -> Result<'tree, Option<tree_sitter::Node<'tree>>> {
        let node = self.current_node();
        self.advance_state()?;
        Ok(node)
    }

    pub fn current_node(&self) -> Option<tree_sitter::Node<'tree>> {
        match self.current {
            NodeIterState::Node(n) => {
                debug!("current_node: {:?}", n.map(|n| n.kind()));
                n
            }
            NodeIterState::Complete => None,
            // TODO: Should error?
            NodeIterState::Start => None,
        }
    }

    pub fn finalize(&self) -> Result<'tree, ()> {
        let state = self.ctx.state;
        let expected = self.ctx.num_states + 1;
        if state != expected {
            return Err(ExtractError::field_extraction(
                self,
                format!("Could not finalize, was in state: {state}, expected: {expected}"),
            ));
        }
        Ok(())
    }
}

// Some helpers.
impl<'cursor, 'tree> ExtractFieldIterator<'cursor, 'tree> {
    fn handle_optional_err<F>(&mut self, f: F) -> Result<'tree, ()>
    where
        F: FnOnce() -> String,
    {
        if self.ctx.optional && self.ctx.state == 1 {
            debug!("advance_state: optional, outputting None");
            self.ctx.state = self.ctx.num_states + 1;
            self.current = NodeIterState::Complete;
            Ok(())
        } else {
            Err(ExtractError::field_extraction(self, f()))
        }
    }

    pub(crate) fn position(&self) -> crate::Position {
        crate::Position::from_node(self.cursor.node())
    }
}

#[derive(Default, Clone, Copy, PartialEq)]
pub(crate) enum NodeIterState<'tree> {
    Node(Option<tree_sitter::Node<'tree>>),
    #[default]
    Start,
    Complete,
}
