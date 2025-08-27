use crate::error::ExtractError;

use super::Result;
use log::{debug, trace};
use tree_sitter::Node;

pub struct ExtractFieldIterator<'cursor, 'tree: 'cursor> {
    pub(crate) cursor: &'cursor mut tree_sitter::TreeCursor<'tree>,
    pub(crate) field_name: &'static str,
    pub(crate) struct_name: &'static str,
    pub(crate) ctx: ExtractFieldContext,
    pub(crate) source: &'cursor [u8],
    pub(crate) current: NodeIterState<'tree>,
    pub(crate) did_advance: bool,
    pub(crate) final_node: Option<Node<'tree>>,
}

pub struct ExtractFieldContext {
    state_fn: fn(u32) -> ExtractFieldState,
    state: u32,
    num_states: u32,
    optional: bool,
}

impl ExtractFieldContext {
    pub fn new(
        num_states: u32,
        optional: bool,
        // repeat_type: RepeatType,
        state_fn: fn(u32) -> ExtractFieldState,
    ) -> Self {
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
    Repeat(&'static str, bool),
    Repeat1,
    Complete,
    // State went too far.
    Overflow,
}

impl<'cursor, 'tree: 'cursor> ExtractFieldIterator<'cursor, 'tree> {
    fn skip_extras(&mut self) {
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

    fn advance_cursor(&mut self) {
        if let NodeIterState::Node(n) = self.current {
            self.final_node = n;
        }
        self.did_advance = self.cursor.goto_next_sibling();
    }

    fn set_complete(&mut self) {
        if let NodeIterState::Node(n) = self.current {
            self.final_node = n;
        }
        self.current = NodeIterState::Complete;
    }

    pub fn advance_state(&mut self) -> Result<'tree, ()> {
        if self.current == NodeIterState::Complete {
            debug!("advance_state: verifying completion");
            self.finalize()?;
            return Ok(());
        }
        self.skip_extras();
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
                    // TODO: It would be generally lovely to clean up this logic throughout.
                    if optional {
                        debug!("advance_state: state didn't match, but optional, skipping");
                        self.current = NodeIterState::Node(None);
                        return Ok(());
                    }

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
                    self.advance_cursor();
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
                    if optional {
                        debug!("advance_state: state didn't match, but optional, skipping");
                        self.current = NodeIterState::Node(None);
                        return Ok(());
                    }
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
                        self.advance_cursor();
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
            ExtractFieldState::Repeat(expected, named) => {
                debug!("advance_state: repeat state: expected={expected}, named={named}");
                if !self.did_advance {
                    // We reached the end of the cursor state, we can advance to the end.
                    self.ctx.state = self.ctx.num_states + 1;
                    self.set_complete();
                    return Ok(());
                }
                // Check if the state matches the repeat and then start over from the beginning. If
                // it doesn't, then we need to advance again and we should hit the complete state
                // after that.
                let cursor_field = self.cursor.field_name();
                let field_name = self.field_name;
                if cursor_field != Some(field_name) {
                    debug!("advance_state: field names didn't match in repeat, completing state");
                    self.ctx.state = self.ctx.num_states + 1;
                    self.set_complete();
                    // Check if we have an optional overall.
                    // self.handle_optional_err(|| {
                    //     format!(
                    //         "fields didn't match, cursor had: {:?}, expected: {}",
                    //         cursor_field, field_name
                    //     )
                    // })?;
                    return Ok(());
                }
                if n.kind() == expected && n.is_named() == named {
                    debug!("advance_state: repeat state matched, resetting iteration");
                    // Advance past the repeat symbol and start over.
                    self.advance_cursor();
                    self.ctx.state = 0;
                    self.advance_state()?;
                    Ok(())
                } else {
                    self.handle_optional_err(|| "state didn't match".into())?;
                    Ok(())
                }
            }
            ExtractFieldState::Repeat1 => {
                debug!("advance_state: repeat1 state");
                if !self.did_advance {
                    self.ctx.state = self.ctx.num_states + 1;
                    self.set_complete();
                    return Ok(());
                }
                let cursor_field = self.cursor.field_name();
                let field_name = self.field_name;
                if cursor_field != Some(field_name) {
                    debug!("advance_state: field names didn't match in repeat, completing state");
                    self.ctx.state = self.ctx.num_states + 1;
                    self.set_complete();
                    // Check if we have an optional overall.
                    // self.handle_optional_err(|| {
                    //     format!(
                    //         "fields didn't match, cursor had: {:?}, expected: {}",
                    //         cursor_field, field_name
                    //     )
                    // })?;
                    Ok(())
                } else {
                    debug!("advance_state: field names matched, triggering repeat");
                    // No repeat symbol in this case, we just are at the next repeat node already.
                    self.ctx.state = 0;
                    self.advance_state()?;
                    Ok(())
                }
            }
            ExtractFieldState::Complete => {
                debug!("advance_state: got complete state");
                self.set_complete();
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

    pub fn is_valid(&self) -> bool {
        matches!(self.current, NodeIterState::Node(_))
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
        if self.ctx.state == 1 && self.ctx.optional {
            debug!("advance_state: optional, outputting None");
            self.ctx.state = self.ctx.num_states + 1;
            self.set_complete();
            Ok(())
        } else {
            Err(ExtractError::field_extraction(self, f()))
        }
    }

    pub(crate) fn new(
        ctx: ExtractFieldContext,
        cursor: &'cursor mut tree_sitter::TreeCursor<'tree>,
        struct_name: &'static str,
        field_name: &'static str,
        source: &'cursor [u8],
    ) -> Self {
        Self {
            cursor,
            final_node: None,
            current: NodeIterState::Start,
            did_advance: false,
            source,
            field_name,
            struct_name,
            ctx,
        }
    }

    pub(crate) fn position(&self) -> crate::Position {
        match self.current {
            NodeIterState::Node(Some(n)) => crate::Position::from_node(n),
            _ => crate::Position::from_node(self.cursor.node()),
        }
    }

    pub(crate) fn final_position(&self) -> crate::Position {
        match self.final_node {
            Some(n) => crate::Position::from_node(n),
            _ => self.position(),
        }
    }
}

#[derive(Default, Clone, Copy, PartialEq)]
pub(crate) enum NodeIterState<'tree> {
    Node(Option<tree_sitter::Node<'tree>>),
    #[default]
    Start,
    Complete,
}
