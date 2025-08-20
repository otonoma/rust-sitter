use std::{collections::HashSet, ops::Range};

use crate::{Point, Position, extract::ExtractContext};

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
    Missing,
    Error,
    FailedExtract {
        field: String,
    },
    MissingNode {
        node_kind: String,
        type_name: &'static str,
    },
    MissingEnum {
        node_kind: String,
        enum_name: &'static str,
    },
    /// Parsed OK, but failed to extract to the given type.
    TypeConversion(Box<dyn std::error::Error + Send + Sync + 'static>),
}

/// A low level error which just wraps the error node and exposes many fields around it.
#[derive(Debug)]
pub struct NodeError<'a> {
    node: tree_sitter::Node<'a>,
}

impl<'a> NodeError<'a> {
    pub fn to_parse_error(&self) -> ParseError {
        ParseError {
            node_position: Position::new(self.node_byte_range(), self.point_range()),
            error_position: Position::new(
                self.first_error_byte_range(),
                self.first_error_point_range(),
            ),
            lookaheads: self.lookahead().map(|l| l.collect()).unwrap_or_default(),
            reason: if self.node.is_missing() {
                ParseErrorReason::Missing
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
            ParseErrorReason::Missing => f.write_str("missing node"),
            ParseErrorReason::Error => f.write_str("parse error"),
            ParseErrorReason::FailedExtract { field } => {
                write!(f, "failed extraction of field: {field}")
            }
            ParseErrorReason::MissingNode {
                node_kind,
                type_name,
            } => write!(
                f,
                "missing node in extraction of type: {type_name}, {node_kind}"
            ),
            ParseErrorReason::MissingEnum {
                node_kind,
                enum_name,
            } => write!(
                f,
                "missing enum in extraction of type: {enum_name}, {node_kind}"
            ),
            ParseErrorReason::TypeConversion(error) => write!(f, "type conversion: {error}"),
        }
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
    reason: ExtractErrorReason<'a>,
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
    pub(crate) fn new(n: tree_sitter::Node<'a>, expected_field: String) -> Self {
        let position = crate::Position::from_node(n);
        Self {
            inner: vec![ExtractErrorInner {
                position,
                reason: ExtractErrorReason::Parse {
                    expected_field,
                    node: n,
                },
            }],
        }
    }
    pub(crate) fn merge(&mut self, err: ExtractError<'a>) {
        self.inner.extend(err.inner);
    }

    pub(crate) fn type_conversion(
        n: tree_sitter::Node<'_>,
        e: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        let position = crate::Position::from_node(n);
        Self {
            inner: vec![ExtractErrorInner {
                position,
                reason: ExtractErrorReason::TypeConversion(Box::new(e)),
            }],
        }
    }

    pub(crate) fn accumulate_parse_errors(self, errors: &mut Vec<ParseError>) {
        for inner in self.inner {
            let err = match inner.reason {
                ExtractErrorReason::TypeConversion(t) => {
                    let reason = ParseErrorReason::TypeConversion(t);
                    ParseError {
                        node_position: inner.position.clone(),
                        error_position: inner.position,
                        reason,
                        lookaheads: vec![],
                    }
                }
                ExtractErrorReason::Parse {
                    expected_field,
                    node,
                } => {
                    let reason = ParseErrorReason::FailedExtract {
                        field: expected_field,
                    };
                    let mut error = NodeError { node }.to_parse_error();
                    error.reason = reason;
                    error
                }
                ExtractErrorReason::MissingNode {
                    node_kind,
                    type_name,
                } => {
                    let reason = ParseErrorReason::MissingNode {
                        node_kind,
                        type_name,
                    };
                    ParseError {
                        node_position: inner.position.clone(),
                        error_position: inner.position,
                        reason,
                        lookaheads: vec![],
                    }
                }
                ExtractErrorReason::MissingEnum {
                    node_kind,
                    enum_name,
                } => {
                    let reason = ParseErrorReason::MissingEnum {
                        node_kind,
                        enum_name,
                    };
                    ParseError {
                        node_position: inner.position.clone(),
                        error_position: inner.position,
                        reason,
                        lookaheads: vec![],
                    }
                }
            };
            errors.push(err);
        }
    }

    pub fn missing_node(ctx: &ExtractContext<'_>, type_name: &'static str) -> Self {
        let position = crate::Position {
            // TODO: This should be fixed to actually have the full range from the outer node.
            bytes: ctx.last_idx..ctx.last_idx,
            start: Point::from_tree_sitter(ctx.last_pt),
            end: Point::from_tree_sitter(ctx.last_pt),
        };
        Self {
            inner: vec![ExtractErrorInner {
                position,
                reason: ExtractErrorReason::MissingNode {
                    node_kind: ctx.node_kind.to_owned(),
                    type_name,
                },
            }],
        }
    }

    pub fn missing_enum(ctx: &ExtractContext<'_>, enum_name: &'static str) -> Self {
        let position = crate::Position {
            // TODO: This should be fixed to actually have the full range from the outer node.
            bytes: ctx.last_idx..ctx.last_idx,
            start: Point::from_tree_sitter(ctx.last_pt),
            end: Point::from_tree_sitter(ctx.last_pt),
        };
        Self {
            inner: vec![ExtractErrorInner {
                position,
                reason: ExtractErrorReason::MissingEnum {
                    node_kind: ctx.node_kind.to_owned(),
                    enum_name,
                },
            }],
        }
    }

    pub fn position(&self) -> &Position {
        &self.inner[0].position
    }

    pub fn reason(&self) -> &ExtractErrorReason<'_> {
        &self.inner[0].reason
    }
}

#[derive(Debug)]
pub enum ExtractErrorReason<'a> {
    /// Failed to parse at the tree-sitter level.
    Parse {
        // Can be &'static?
        expected_field: String,
        node: tree_sitter::Node<'a>,
    },
    MissingNode {
        node_kind: String,
        type_name: &'static str,
    },
    MissingEnum {
        node_kind: String,
        enum_name: &'static str,
    },
    /// Parsed OK, but failed to extract to the given type.
    TypeConversion(Box<dyn std::error::Error + Send + Sync + 'static>),
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
