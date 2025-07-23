pub mod __private;
pub mod error;
pub mod extract;
pub mod rule;

pub use extract::{Extract, WithLeaf};

use std::ops::Deref;

pub use rust_sitter_macro::*;

#[cfg(feature = "tree-sitter-standard")]
pub use tree_sitter_runtime_standard as tree_sitter;

#[cfg(feature = "tree-sitter-c2rust")]
pub use tree_sitter_runtime_c2rust as tree_sitter;

use tree_sitter::Node;

#[derive(Clone, Debug)]
/// A wrapper around a value that also contains the span of the value in the source.
pub struct Spanned<T> {
    /// The underlying parsed node.
    pub value: T,
    /// The span of the node in the source. The first value is the inclusive start
    /// of the span, and the second value is the exclusive end of the span.
    pub byte_span: Span,
    pub line_span: (Point, Point),
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start_byte: usize,
    pub end_byte: usize,
    // Do we need point? I don't think so in reality, because end tools can do the conversion,
    // which tends to be the pattern in other parser tools.
}

impl Span {
    pub fn new(start_byte: usize, end_byte: usize) -> Self {
        Self {
            start_byte,
            end_byte,
        }
    }
}

impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Self::new(start, end)
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
    fn extract<'a>(
        node: Option<Node>,
        source: &[u8],
        last_idx: usize,
        last_pt: tree_sitter::Point,
        leaf_fn: Option<Self::LeafFn<'a>>,
    ) -> extract::Result<Spanned<U>> {
        Ok(Spanned {
            value: T::extract(node, source, last_idx, last_pt, leaf_fn)?,
            byte_span: node
                .map(|n| (n.start_byte(), n.end_byte()))
                .unwrap_or((last_idx, last_idx))
                .into(),
            line_span: node
                .map(|n| {
                    (
                        Point::from_tree_sitter(n.start_position()),
                        Point::from_tree_sitter(n.end_position()),
                    )
                })
                .unwrap_or((
                    Point::from_tree_sitter(last_pt),
                    Point::from_tree_sitter(last_pt),
                )),
        })
    }
}
