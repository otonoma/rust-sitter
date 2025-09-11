use log::{debug, trace};
use std::{collections::HashSet, ops::Range};

use crate::{ExtractContext, Point, Position, extract::ExtractFieldIterator};

/// A high level parsing error with useful information extracted already.
#[derive(Debug)]
pub struct ParseError {
    /// Position within the source code of the full node which failed to parse.
    /// This can be used in combination with `error_position` to indicate a greater context of where
    /// an error occurred.
    pub node_position: Position,
    pub error_position: Position,
    /// Possible next tokens that were expected.
    pub lookaheads: Vec<&'static str>,
    pub reason: ParseErrorReason,
}

#[derive(Debug)]
pub enum ParseErrorReason {
    Missing(&'static str),
    Error,
    Extract {
        struct_name: &'static str,
        field_name: &'static str,
        reason: ExtractErrorReason,
    },
}

impl ParseError {
    pub fn is_missing(&self) -> bool {
        matches!(&self.reason, ParseErrorReason::Missing(_))
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} to {}:{}, {}",
            self.error_position.start.line,
            self.error_position.start.column,
            self.error_position.end.line,
            self.error_position.end.column,
            self.reason
        )
    }
}

impl std::fmt::Display for ParseErrorReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorReason::Missing(kind) => write!(f, "missing {kind}"),
            ParseErrorReason::Error => f.write_str("parse error"),
            // ParseErrorReason::FailedExtract { field } => {
            //     write!(f, "failed extraction of field: {field}")
            // }
            ParseErrorReason::Extract {
                struct_name,
                field_name,
                reason,
            } => {
                write!(
                    f,
                    "extraction error for {struct_name}::{field_name}. Reason: {reason}"
                )
            }
        }
    }
}

/// A low level error which just wraps the error node and exposes many fields around it.
#[derive(Debug)]
pub struct NodeError<'a> {
    node: tree_sitter::Node<'a>,
}

impl<'a> NodeError<'a> {
    pub fn to_parse_error(&self) -> ParseError {
        // Handle missing shift.
        let mut node_position = Position::new(self.node_byte_range(), self.point_range());
        let mut error_position = Position::new(
            self.first_error_byte_range(),
            self.first_error_point_range(),
        );
        trace!("error node: {}", self.node);
        trace!("error node: {:?}", self.node);
        trace!("error node parent: {:?}", self.node.parent());
        if self.node.is_missing()
            && let Some(parent) = self.node.parent()
        {
            debug!("attempting missing shift: {}", parent.to_sexp());
            // Find where the missing node is located in the parent, then shift it backwards by
            // removing any extra nodes in its place.
            // let mut c = parent.walk();
            // let idx = parent.children(&mut c)
            //     // defers to pointer equality, which is what we want in this case.
            //     .position(|n| n == self.node)
            //     .unwrap();
            // c.reset(self.node);
            // Doesn't work, the cursor iterator doesn't work correctly.
            // dbg!(self.node.prev_sibling());
            // while dbg!(c.goto_previous_sibling()) && c.node().is_extra() {
            //     debug!("shifting past extra: {}", c.node());
            // }
            // Use parent node for node_position and this node for error_position.
            let mut node = self.node;
            let mut has_shifted = false;
            while let Some(n) = node.prev_sibling() {
                node = n;
                if !has_shifted {
                    has_shifted = node.is_extra();
                }
                debug!("shifting past extra: {}", n);
                if !node.is_extra() {
                    break;
                }
            }

            if has_shifted {
                debug!("shifted to node: {}", node.kind());
                let range = node.byte_range();
                let range = range.end..range.end;
                let new_err = Position::new(
                    range,
                    (node.start_position().into(), node.end_position().into()),
                );
                let new_pos = Position::new(
                    parent.byte_range(),
                    (parent.start_position().into(), parent.end_position().into()),
                );
                debug!("shifted position from {error_position:?} to {new_pos:?}");
                error_position = new_err;
                node_position = new_pos;
            }
        }
        ParseError {
            node_position,
            error_position,
            lookaheads: self.lookahead().map(|l| l.collect()).unwrap_or_default(),
            reason: if self.node.is_missing() {
                ParseErrorReason::Missing(self.node.kind())
            } else {
                ParseErrorReason::Error
            },
        }
    }
    /// Full range of the node which failed to parse.
    pub fn node_byte_range(&self) -> Range<usize> {
        self.node.byte_range()
    }

    /// Byte range of the portion of the text which created the error.
    pub fn error_byte_range(&self) -> Range<usize> {
        self.node.error_byte_range().unwrap()
    }

    pub fn point_range(&self) -> (Point, Point) {
        let start = self.node.start_position();
        let end = self.node.end_position();
        (Point::from_tree_sitter(start), Point::from_tree_sitter(end))
    }

    pub fn error_point_range(&self) -> (Point, Point) {
        let start = self.node.error_start_position().unwrap();
        let end = self.node.error_end_position().unwrap();
        (Point::from_tree_sitter(start), Point::from_tree_sitter(end))
    }

    pub fn first_error_point_range(&self) -> (Point, Point) {
        match self.node.error_child(0) {
            None => self.error_point_range(),
            Some(c) => {
                let start = c.start_position();
                let end = c.end_position();
                (Point::from_tree_sitter(start), Point::from_tree_sitter(end))
            }
        }
    }

    pub fn first_error_byte_range(&self) -> Range<usize> {
        match self.node.error_child(0) {
            None => self.error_byte_range(),
            Some(c) => c.byte_range(),
        }
    }

    pub fn is_missing(&self) -> bool {
        self.node.is_missing()
    }

    pub fn lookahead(
        &self,
        // grammar: Option<&'a crate::grammar::Grammar>,
    ) -> Option<impl Iterator<Item = &'static str>> {
        let (state, reachable, filter) = if self.node.is_missing() {
            // Handle the lookahead appropriately for missing.
            let state = self.node.parse_state();
            (state, None, true)
        } else {
            // Find the endpoint.
            // let (node, ctx) = match self.node.error_child(0) {
            //     Some(c) => (c, self.node.child(0).unwrap()),
            //     None => (self.node, self.node),
            // };
            let node = match self.node.error_child(0) {
                Some(c) => c,
                None => self.node,
            };

            // Find the first context node type and compute reachable set.
            // let reachable = if let Some(grammar) = grammar {
            //     dbg!(grammar.reachable_set(dbg!(ctx.kind())))
            // } else {
            //     None
            // };
            let reachable = None;

            let state = node.parse_state();
            // NOTE: We may want to always filter these.
            (state, reachable, false)
        };

        if state == 0 {
            return None;
        }

        let language = self.node.language().to_owned();
        let it = language.lookahead_iterator(state)?;

        Some(ErrorLookahead {
            it,
            language,
            filter_non_action: filter,
            state,
            reachable,
        })
    }
}

struct ErrorLookahead<'a> {
    it: tree_sitter::LookaheadIterator,
    language: tree_sitter::Language,
    filter_non_action: bool,
    state: u16,
    reachable: Option<HashSet<&'a str>>,
}

impl Iterator for ErrorLookahead<'_> {
    type Item = &'static str;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            self.it.next()?;
            let sym = self.it.current_symbol();
            // skip the end symbol, it isn't useful here.
            if sym == 0 {
                continue;
            }
            if self.filter_non_action && !self.it.has_actions() {
                continue;
            }
            // Maybe we want this to be optional as well?
            // Filter out "extra" nodes.
            if self.state == self.language.next_state(self.state, sym) {
                continue;
            }

            let sym_name = self.it.current_symbol_name();

            if let Some(reachable) = &self.reachable
                && !reachable.contains(sym_name)
            {
                continue;
            }

            return Some(sym_name);
        }
    }
}

#[derive(Debug)]
pub struct ExtractError<'a> {
    inner: Vec<ExtractErrorInner<'a>>,
}

#[derive(Debug)]
struct ExtractErrorInner<'a> {
    /// Span of the node which failed to extract.
    position: crate::Position,
    field_name: &'static str,
    struct_name: &'static str,
    node: Option<tree_sitter::Node<'a>>,
    reason: ExtractErrorReason,
}

impl<'a> ExtractError<'a> {
    pub(crate) fn empty() -> Self {
        Self { inner: vec![] }
    }

    pub(crate) fn prop(self) -> Result<(), Self> {
        if self.inner.is_empty() {
            Ok(())
        } else {
            Err(self)
        }
    }

    pub(crate) fn new(
        struct_name: &'static str,
        field_name: &'static str,
        position: crate::Position,
        reason: ExtractErrorReason,
    ) -> Self {
        Self {
            inner: vec![ExtractErrorInner {
                // TODO: Provide this where possible.
                node: None,
                position,
                field_name,
                struct_name,
                reason,
            }],
        }
    }

    pub(crate) fn new_ctx(
        ctx: &ExtractContext,
        position: crate::Position,
        reason: ExtractErrorReason,
    ) -> Self {
        Self::new(ctx.struct_name, ctx.field_name, position, reason)
    }

    pub(crate) fn merge(&mut self, err: ExtractError<'a>) {
        self.inner.extend(err.inner);
    }

    pub(crate) fn type_conversion(
        ctx: &ExtractContext,
        n: tree_sitter::Node<'_>,
        e: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        let position = crate::Position::from_node(n);
        Self::new(
            ctx.struct_name,
            ctx.field_name,
            position,
            ExtractErrorReason::TypeConversion(Box::new(e)),
        )
    }

    pub(crate) fn field_extraction(
        ctx: &ExtractFieldIterator<'_, '_>,
        msg: impl Into<String>,
    ) -> Self {
        let msg = msg.into();
        log::error!(
            "field_extraction error: {}::{}, msg={}",
            ctx.struct_name,
            ctx.field_name,
            msg
        );
        let position = ctx.position();
        Self::new(
            ctx.struct_name,
            ctx.field_name,
            position,
            ExtractErrorReason::FieldExtraction { message: msg },
        )
    }

    #[allow(dead_code)]
    pub(crate) fn accumulate_parse_errors(self, errors: &mut Vec<ParseError>) {
        for inner in self.inner {
            let err = ParseError {
                node_position: inner.position.clone(),
                error_position: inner.position,
                lookaheads: vec![],
                reason: ParseErrorReason::Extract {
                    struct_name: inner.struct_name,
                    field_name: inner.field_name,
                    reason: inner.reason,
                },
            };
            errors.push(err);
        }
    }

    pub fn missing_node(ctx: &ExtractContext) -> Self {
        let position = crate::Position {
            // TODO: This should be fixed to actually have the full range from the outer node.
            bytes: ctx.last_idx..ctx.last_idx,
            start: Point::from_tree_sitter(ctx.last_pt),
            end: Point::from_tree_sitter(ctx.last_pt),
        };
        Self::new_ctx(ctx, position, ExtractErrorReason::MissingNode)
    }

    pub fn missing_enum(ctx: &ExtractContext) -> Self {
        let position = crate::Position {
            // TODO: This should be fixed to actually have the full range from the outer node.
            bytes: ctx.last_idx..ctx.last_idx,
            start: Point::from_tree_sitter(ctx.last_pt),
            end: Point::from_tree_sitter(ctx.last_pt),
        };
        Self::new_ctx(ctx, position, ExtractErrorReason::MissingEnum)
    }

    pub fn position(&self) -> &Position {
        &self.inner[0].position
    }

    pub fn reason(&self) -> &ExtractErrorReason {
        &self.inner[0].reason
    }
}

#[derive(Debug)]
pub enum ExtractErrorReason {
    FieldExtraction {
        message: String,
    },
    MissingNode,
    MissingEnum,
    /// Parsed OK, but failed to extract to the given type.
    TypeConversion(Box<dyn std::error::Error + Send + Sync + 'static>),
}

impl std::fmt::Display for ExtractErrorReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingNode => write!(f, "missing node in extraction"),
            Self::MissingEnum => write!(f, "missing enum in extraction",),
            Self::FieldExtraction { message } => write!(f, "field extraction failure: {message}"),
            Self::TypeConversion(error) => write!(f, "type conversion: {error}"),
        }
    }
}

impl<'a> IntoIterator for ExtractError<'a> {
    type Item = ExtractError<'a>;
    type IntoIter = ErrorIntoIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        ErrorIntoIter {
            iter: self.inner.into_iter(),
        }
    }
}

pub struct ErrorIntoIter<'a> {
    iter: std::vec::IntoIter<ExtractErrorInner<'a>>,
}

impl<'a> Iterator for ErrorIntoIter<'a> {
    type Item = ExtractError<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        Some(ExtractError {
            inner: vec![self.iter.next()?],
        })
    }
}
/// Given the root node of a Tree Sitter parsing result, accumulates all
/// errors that were emitted.
pub fn collect_parsing_errors(node: &tree_sitter::Node<'_>, errors: &mut Vec<ParseError>) {
    collect_node_errors(*node, |err| errors.push(err.to_parse_error()));
}

pub fn collect_node_errors<'a, F>(node: tree_sitter::Node<'a>, mut f: F)
where
    F: FnMut(NodeError<'a>),
{
    collect_node_errors_(node, &mut f);
    // I couldn't figure out how to get this to compile well.
    fn collect_node_errors_<'a, F>(node: tree_sitter::Node<'a>, f: &mut F)
    where
        F: FnMut(NodeError<'a>),
    {
        if node.is_error() || node.is_missing() {
            f(NodeError { node });
        } else if node.has_error() {
            // A node somewhere down in the tree from here has an error, recursively find it.
            let mut cursor = node.walk();
            node.children(&mut cursor)
                .for_each(|c| collect_node_errors_(c, f));
        }
    }
}
