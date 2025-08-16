pub mod __private;
pub mod error;
pub mod extract;
pub mod grammar;
pub mod rule;

pub use rule::Language;

use extract::ExtractContext;
pub use extract::{Extract, WithLeaf};

use std::ops::Deref;

pub use rust_sitter_macro::*;
pub use tree_sitter;

use tree_sitter::Node;

/// The result of a parse. Parses can return errors and potentially still produce a valid result
/// partial result.
pub struct ParseResult<T> {
    /// The parse result, if it managed to get one. This can `Some` even if there are errors.
    pub result: Option<T>,
    /// All errors that were found during parsing.
    pub errors: Vec<error::ParseError>,
}

impl<T> ParseResult<T> {
    /// Only return the result if there are no errors.
    pub fn into_result(self) -> Result<T, Vec<error::ParseError>> {
        if self.errors.is_empty() {
            // It shouldn't be possible to have an empty result with no parse errors.
            self.result.ok_or_else(Vec::new)
        } else {
            Err(self.errors)
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ParseResult<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseResult")
            .field("result", &self.result)
            .field("errors", &self.errors)
            .finish()
    }
}

pub struct NodeParseResult<'a, T> {
    pub result: Result<T, extract::ExtractError<'a>>,
    pub errors: Vec<error::NodeError<'a>>,
}

/// A wrapper around a value that also contains the span of the value in the source.
#[derive(Clone, Debug)]
pub struct Spanned<T> {
    /// The underlying parsed node.
    pub value: T,
    /// The position where the node is located.
    pub position: Position,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

/// Position in a file, used by errors and `Spanned`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Position {
    /// Byte range.
    pub bytes: core::ops::Range<usize>,
    /// row + column start point.
    pub start: Point,
    /// row + column end point.
    pub end: Point,
}

impl Position {
    fn new(bytes: core::ops::Range<usize>, (start, end): (Point, Point)) -> Self {
        Self { bytes, start, end }
    }

    fn from_node(node: Node<'_>) -> Self {
        let bytes = node.byte_range();
        let start = Point::from_tree_sitter(node.start_position());
        let end = Point::from_tree_sitter(node.end_position());
        Self { bytes, start, end }
    }
}

/// A line and column point in a source parse. These are 1 based to correspond with a text editor
/// line and column. Note, this is a divergence from tree-sitter, which uses a zero-based `Point`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Point {
    pub line: usize,
    pub column: usize,
}

impl Point {
    pub(crate) fn from_tree_sitter(p: tree_sitter::Point) -> Self {
        Self {
            line: p.row + 1,
            column: p.column + 1,
        }
    }
}

impl<T: Extract<U>, U> Extract<Spanned<U>> for Spanned<T> {
    type LeafFn<'a> = T::LeafFn<'a>;
    fn extract<'a, 'tree>(
        ctx: &mut ExtractContext<'_>,
        node: Option<Node<'tree>>,
        source: &[u8],
        leaf_fn: Option<Self::LeafFn<'a>>,
    ) -> extract::Result<'tree, Spanned<U>> {
        Ok(Spanned {
            value: T::extract(ctx, node, source, leaf_fn)?,
            position: node.map(Position::from_node).unwrap_or_else(|| Position {
                bytes: ctx.last_idx..ctx.last_idx,
                start: Point::from_tree_sitter(ctx.last_pt),
                end: Point::from_tree_sitter(ctx.last_pt),
            }),
        })
    }
}
