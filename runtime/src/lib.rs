pub mod __private;

use std::ops::Deref;

pub use rust_sitter_macro::*;

#[cfg(feature = "tree-sitter-standard")]
pub use tree_sitter_runtime_standard as tree_sitter;

#[cfg(feature = "tree-sitter-c2rust")]
pub use tree_sitter_runtime_c2rust as tree_sitter;

use tree_sitter::Node;

/// Defines the logic used to convert a node in a Tree Sitter tree to
/// the corresponding Rust type.
pub trait Extract<Output> {
    type LeafFn<'a>: Clone;
    fn extract<'a>(
        node: Option<Node>,
        source: &[u8],
        last_idx: usize,
        last_pt: tree_sitter::Point,
        leaf_fn: Option<Self::LeafFn<'a>>,
    ) -> Output;
}

#[derive(Debug, Clone, Copy)]
pub struct NodeExt<'a> {
    pub node: Node<'a>,
    pub source: &'a [u8],
    pub last_idx: usize,
    pub last_pt: tree_sitter::Point,
}

pub trait StrOrNode {
    type Output;
    fn apply(self, source: &[u8], node: Node<'_>, last_idx: usize, last_pt: tree_sitter::Point) -> Self::Output;
}

impl<L> StrOrNode for fn(&str) -> L {
    type Output = L;
    fn apply(self, source: &[u8], node: Node<'_>, _last_idx: usize, _last_pt: tree_sitter::Point) -> L {
        let text = node.utf8_text(source).expect("Could not get text");
        self(text)
    }
}

impl<L> StrOrNode for fn(&NodeExt<'_>) -> L {
    type Output = L;
    fn apply(self, source: &[u8], node: Node<'_>, last_idx: usize, last_pt: tree_sitter::Point) -> L {
        let node = NodeExt {
            node,
            source,
            last_idx,
            last_pt,
        };
        self(&node)
    }
}

pub trait Handler<Input, Output> {
    fn extract(
        self,
        node: Option<Node>,
        source: &[u8],
        last_idx: usize,
        last_pt: tree_sitter::Point,
    ) -> Output;
}

macro_rules! handler_fn {
    ($($t:ident),*) => {
       impl<F, O, $($t: Extract<$t>),*> Handler<($($t),*), O> for F
           where F: FnOnce($($t),*) -> O,
       {
           fn extract(
               self,
                node: Option<Node>,
                source: &[u8],
                last_idx: usize,
                last_pt: tree_sitter::Point,
            ) -> O {
                let node = node.expect("No node found");
                let mut c = node.walk();
                let mut it = node.children(&mut c);
                self(
                    $(
                        $t::extract(it.next(), source, last_idx, last_pt, None)
                    ),*
                )
            }
       }

    };
}

handler_fn!(T1, T2);

/// Map for `#[with(...)]`
pub struct WithLeaf<L, F> {
    _phantom: std::marker::PhantomData<L>,
    _f: std::marker::PhantomData<F>,
}

impl<L: 'static, F> Extract<L> for WithLeaf<L, F>
where
    F: StrOrNode<Output = L> + Clone,
{
    type LeafFn<'a> = F;

    fn extract<'a>(
        node: Option<Node>,
        source: &[u8],
        last_idx: usize,
        last_pt: tree_sitter::Point,
        leaf_fn: Option<Self::LeafFn<'a>>,
    ) -> L {
        let node = node.expect("Expected a node");
        leaf_fn
            .expect("No leaf function on WithLeaf")
            .apply(source, node, last_idx, last_pt)
    }
}

#[derive(Clone)]
pub struct MappedExtract<F, L0, L1> {
    _type: std::marker::PhantomData<F>,
    _prev: std::marker::PhantomData<L0>,
    _curr: std::marker::PhantomData<L1>,
}

#[derive(Clone)]
pub struct MappedLeaf<P, F> {
    prev: Option<P>,
    curr: F,
}

impl<F, L0: 'static, L1: 'static> Extract<L1> for MappedExtract<F, L0, L1>
where
    F: Extract<L0>,
{
    type LeafFn<'a> = MappedLeaf<F::LeafFn<'a>, &'a dyn Fn(L0) -> L1>;
    fn extract<'a>(
        node: Option<Node>,
        source: &[u8],
        last_idx: usize,
        last_pt: tree_sitter::Point,
        leaf_fn: Option<Self::LeafFn<'a>>,
    ) -> L1 {
        let mapped = leaf_fn.unwrap();
        let prev = F::extract(node, source, last_idx, last_pt, mapped.prev);
        (mapped.curr)(prev)
    }
}

// Common implementations for various types.

impl Extract<()> for () {
    type LeafFn<'a> = ();
    fn extract<'a>(
        _node: Option<Node>,
        _source: &[u8],
        _last_idx: usize,
        _last_pt: tree_sitter::Point,
        _leaf_fn: Option<Self::LeafFn<'a>>,
    ) {
    }
}

impl<T: Extract<U>, U> Extract<Option<U>> for Option<T> {
    type LeafFn<'a> = T::LeafFn<'a>;
    fn extract<'a>(
        node: Option<Node>,
        source: &[u8],
        last_idx: usize,
        last_pt: tree_sitter::Point,
        leaf_fn: Option<Self::LeafFn<'a>>,
    ) -> Option<U> {
        node.map(|n| T::extract(Some(n), source, last_idx, last_pt, leaf_fn))
    }
}

impl<T: Extract<U>, U> Extract<Box<U>> for Box<T> {
    type LeafFn<'a> = T::LeafFn<'a>;
    fn extract<'a>(
        node: Option<Node>,
        source: &[u8],
        last_idx: usize,
        last_pt: tree_sitter::Point,
        leaf_fn: Option<Self::LeafFn<'a>>,
    ) -> Box<U> {
        Box::new(T::extract(node, source, last_idx, last_pt, leaf_fn))
    }
}

impl<T: Extract<U>, U> Extract<Vec<U>> for Vec<T> {
    type LeafFn<'a> = T::LeafFn<'a>;
    fn extract<'a>(
        node: Option<Node>,
        source: &[u8],
        mut last_idx: usize,
        mut last_pt: tree_sitter::Point,
        leaf_fn: Option<Self::LeafFn<'a>>,
    ) -> Vec<U> {
        node.map(|node| {
            let mut cursor = node.walk();
            let mut out = vec![];
            if cursor.goto_first_child() {
                loop {
                    let n = cursor.node();
                    if cursor.field_name().is_some() {
                        out.push(T::extract(Some(n), source, last_idx, last_pt, leaf_fn.clone()));
                    }

                    last_idx = n.end_byte();
                    last_pt = n.end_position();

                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
            }

            out
        })
        .unwrap_or_default()
    }
}

macro_rules! extract_from_str {
    ($t:ty) => {
        impl Extract<$t> for $t {
            type LeafFn<'a> = ();
            fn extract<'a>(
                node: Option<Node>,
                source: &[u8],
                _last_idx: usize,
                _last_pt: tree_sitter::Point,
                _leaf_fn: Option<Self::LeafFn<'a>>,
            ) -> Self {
                let node = node.expect("No node found");
                let text = node.utf8_text(source).expect("No text found for node");
                text.parse().expect("Failed to parse type")
            }
        }
    };
}

extract_from_str!(u8);
extract_from_str!(i8);
extract_from_str!(u16);
extract_from_str!(i16);
extract_from_str!(u32);
extract_from_str!(i32);
extract_from_str!(u64);
extract_from_str!(i64);
// NOTE: These two may not work as intended due to rounding issues.
extract_from_str!(f32);
extract_from_str!(f64);
// Sort of silly, but keeps it general.
extract_from_str!(String);

macro_rules! extract_for_tuple {
    ($($t:ident),*) => {
       impl<$($t: Extract<$t>),*> Extract<($($t),*)> for ($($t),*) {
           type LeafFn<'a> = ();
            fn extract<'a>(
                node: Option<Node>,
                source: &[u8],
                last_idx: usize,
                last_pt: tree_sitter::Point,
                _leaf_fn: Option<Self::LeafFn<'a>>,
            ) -> Self {
                let node = node.expect("No node found");
                let mut c = node.walk();
                let mut it = node.children(&mut c);
                (
                    $(
                        $t::extract(it.next(), source, last_idx, last_pt, None)
                    ),*
                )
            }
       }

    };
}

extract_for_tuple!(T1, T2);
extract_for_tuple!(T1, T2, T3);
extract_for_tuple!(T1, T2, T3, T4);
extract_for_tuple!(T1, T2, T3, T4, T5);
extract_for_tuple!(T1, T2, T3, T4, T5, T6);
extract_for_tuple!(T1, T2, T3, T4, T5, T6, T7);
extract_for_tuple!(T1, T2, T3, T4, T5, T6, T7, T8);
// Good enough, can maybe generate all of these with a macro if we are clever enough.

// Would like this to extract optionals specifically if they exist - probably means if a node is
// present then it is true. Might be too magic though.
// impl Extract<bool> for bool {
//     type LeafFn = ();
//     fn extract(
//             node: Option<tree_sitter_runtime_c2rust::Node>,
//             source: &[u8],
//             last_idx: usize,
//             leaf_fn: Option<&Self::LeafFn>,
//         ) -> bool {
//     }
// }

#[derive(Clone, Debug)]
/// A wrapper around a value that also contains the span of the value in the source.
pub struct Spanned<T> {
    /// The underlying parsed node.
    pub value: T,
    /// The span of the node in the source. The first value is the inclusive start
    /// of the span, and the second value is the exclusive end of the span.
    pub byte_span: (usize, usize),
    pub line_span: (Point, Point),
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
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
    fn from_tree_sitter(p: tree_sitter::Point) -> Self {
        Self {
            line: p.row + 1,
            column: p.column + 1,
        }
    }
    const EMPTY: Self = Self { line: 0, column: 0 };
    const fn empty() -> Self {
        Self::EMPTY
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
    ) -> Spanned<U> {
        Spanned {
            value: T::extract(node, source, last_idx, last_pt, leaf_fn),
            byte_span: node
                .map(|n| (n.start_byte(), n.end_byte()))
                .unwrap_or((last_idx, last_idx)),
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
        }
    }
}

pub mod errors {
    #[cfg(feature = "tree-sitter-standard")]
    use tree_sitter_runtime_standard as tree_sitter;

    #[cfg(feature = "tree-sitter-c2rust")]
    use tree_sitter_runtime_c2rust as tree_sitter;

    #[derive(Debug)]
    /// An explanation for an error that occurred during parsing.
    pub enum ParseErrorReason {
        /// The parser did not expect to see some token.
        UnexpectedToken(String),
        /// Tree Sitter failed to parse a specific intermediate node.
        /// The underlying failures are in the vector.
        FailedNode(Vec<ParseError>),
        /// The parser expected a specific token, but it was not found.
        MissingToken(String),
    }

    #[derive(Debug)]
    /// An error that occurred during parsing.
    pub struct ParseError {
        pub reason: ParseErrorReason,
        /// Inclusive start of the error.
        pub start: usize,
        /// Exclusive end of the error.
        pub end: usize,
    }

    /// Given the root node of a Tree Sitter parsing result, accumulates all
    /// errors that were emitted.
    pub fn collect_parsing_errors(
        node: &tree_sitter::Node,
        source: &[u8],
        errors: &mut Vec<ParseError>,
    ) {
        if node.is_error() {
            if node.child(0).is_some() {
                // we managed to parse some children, so collect underlying errors for this node
                let mut inner_errors = vec![];
                let mut cursor = node.walk();
                node.children(&mut cursor)
                    .for_each(|c| collect_parsing_errors(&c, source, &mut inner_errors));

                errors.push(ParseError {
                    reason: ParseErrorReason::FailedNode(inner_errors),
                    start: node.start_byte(),
                    end: node.end_byte(),
                })
            } else {
                let contents = node.utf8_text(source).unwrap();
                if !contents.is_empty() {
                    errors.push(ParseError {
                        reason: ParseErrorReason::UnexpectedToken(contents.to_string()),
                        start: node.start_byte(),
                        end: node.end_byte(),
                    })
                } else {
                    errors.push(ParseError {
                        reason: ParseErrorReason::FailedNode(vec![]),
                        start: node.start_byte(),
                        end: node.end_byte(),
                    })
                }
            }
        } else if node.is_missing() {
            errors.push(ParseError {
                reason: ParseErrorReason::MissingToken(node.kind().to_string()),
                start: node.start_byte(),
                end: node.end_byte(),
            })
        } else if node.has_error() {
            let mut cursor = node.walk();
            node.children(&mut cursor)
                .for_each(|c| collect_parsing_errors(&c, source, errors));
        }
    }
}
