#[cfg(feature = "tree-sitter-standard")]
use tree_sitter_runtime_standard as tree_sitter;

#[cfg(feature = "tree-sitter-c2rust")]
use tree_sitter_runtime_c2rust as tree_sitter;

use crate::Point;

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
    pub start_byte: usize,
    /// Exclusive end of the error.
    pub end_byte: usize,
    pub start_point: Point,
    pub end_point: Point,
    pub text: String,
    pub kind: &'static str,
    pub parent_context: Option<ParentContext>,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Failure to parse node:")?;
        write!(
            f,
            "\t{}:{} - {}:{}",
            self.start_point.line,
            self.start_point.column,
            self.end_point.line,
            self.end_point.column
        )?;
        write!(f, " {}", self.text)?;
        if let Some(parent) = &self.parent_context {
            writeln!(f)?;
            write!(f, "\t(parent node: {})", parent.kind)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ParentContext {
    pub kind: &'static str,
}

/// Given the root node of a Tree Sitter parsing result, accumulates all
/// errors that were emitted.
pub fn collect_parsing_errors(
    node: &tree_sitter::Node,
    source: &[u8],
    errors: &mut Vec<ParseError>,
) {
    let start_byte = node.start_byte();
    let end_byte = node.end_byte();
    let start_point = Point::from_tree_sitter(node.start_position());
    let end_point = Point::from_tree_sitter(node.end_position());
    let kind = node.kind();
    let text = node.utf8_text(source).unwrap().to_owned();
    let mut parent_context = None;
    let reason = if node.is_error() {
        if let Some(p) = node.parent() {
            parent_context = Some(ParentContext {
                kind: p.kind(),
            });
        }
        if node.child(0).is_some() {
            // we managed to parse some children, so collect underlying errors for this node
            let mut inner_errors = vec![];
            let mut cursor = node.walk();
            node.children(&mut cursor)
                .for_each(|c| collect_parsing_errors(&c, source, &mut inner_errors));

            ParseErrorReason::FailedNode(inner_errors)
        } else {
            let contents = node.utf8_text(source).unwrap();
            if !contents.is_empty() {
                ParseErrorReason::UnexpectedToken(contents.to_string())
            } else {
                ParseErrorReason::FailedNode(vec![])
            }
        }
    } else if node.is_missing() {
        ParseErrorReason::MissingToken(node.kind().to_string())
    } else if node.has_error() {
        let mut cursor = node.walk();
        node.children(&mut cursor)
            .for_each(|c| collect_parsing_errors(&c, source, errors));
        return;
    } else {
        return;
    };
    errors.push(ParseError {
        reason,
        start_byte,
        end_byte,
        start_point,
        end_point,
        text,
        kind,
        parent_context,
    });
}
