#[cfg(feature = "tree-sitter-standard")]
use tree_sitter_runtime_standard as tree_sitter;

#[cfg(feature = "tree-sitter-c2rust")]
use tree_sitter_runtime_c2rust as tree_sitter;

use crate::Point;

#[derive(Debug)]
/// An explanation for an error that occurred during parsing.
pub enum ParseErrorReason {
    /// The parser expected a specific token, but it was not found.
    MissingToken(String),
    Lookahead(Vec<&'static str>),
    Unknown,
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
            self.end_point.column,
        )?;
        write!(f, " {}", self.text)?;
        if let Some(parent) = &self.parent_context {
            writeln!(f)?;
            writeln!(f, "\t(parent node: {})", parent.kind)?;
        }
        write!(f, "\treason: ")?;
        match &self.reason {
            ParseErrorReason::MissingToken(tok) => write!(f, "missing token: {tok}"),
            ParseErrorReason::Unknown => write!(f, "unknown"),
            ParseErrorReason::Lookahead(lookahead) => {
                write!(f, "expected one of: {}", lookahead.join(" | "))
            }
        }
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
    if let Some(p) = node.parent() {
        parent_context = Some(ParentContext { kind: p.kind() });
    }
    let reason = if node.is_error() {
        // Narrow down the node range if possible.
        fn walk_node(node: &tree_sitter::Node) {
            let mut children = node.walk();
            dbg!(node);
            dbg!(node.kind());
            for child in node.children(&mut children) {
                walk_node(&child);
            }
        }
        // let q = tree_sitter::Query::new(&node.language(), "(ERROR_INTERNAL) @error").unwrap();
        // let mut qcur = tree_sitter::QueryCursor::new();
        // let mut it = qcur.captures(&q, *node, source);
        // use tree_sitter::StreamingIterator;
        // // NOTE: Instead of just using the first internal error, we should use all of them that are
        // // non-overlapping.
        // let Some((cap, _)) = it.next() else {
        //     panic!("Could not capture ERROR_INTERNAL");
        // };
        // // Should only be one capture since we only have `@error`
        // let error_internal = cap.captures[0].node;
        println!("Error range: {:?}", node.error_range().unwrap());
        let mut err_cur = node.walk();
        for err in node.error_children(&mut err_cur).unwrap() {
            dbg!(err);
        }

        let end = node.error_child(0).unwrap().prev_sibling().unwrap();
        // walk_node(node);
        // dbg!(error_internal.to_sexp());
        // Traverse down to find the next parse state and display it in the error.
        let mut c = end.walk();
        // c.goto_descendant(dbg!(node.descendant_count() - 1));
        // c.goto_first_child();
        // while c.node().child_count() > 0 && c.goto_next_sibling() {}
        // c.goto_previous_sibling();
        // c.goto_first_child();
        // c.goto_next_sibling();
        while c.goto_last_child() {}
        // while c.goto_next_sibling() {}
        // loop {
        //     let mut run = false;
        //     while c.goto_first_child() { run = true ;}
        //     dbg!(c.node());
        //     if c.goto_next_sibling() {run = true ;}
        //     dbg!(c.node());
        //     if !run {
        //         break;
        //     }
        // }
        // dbg!(c.node());
        // dbg!(c.node().next_parse_state());
        let state = dbg!(c.node().next_parse_state());
        // let state = c.node().next_parse_state();
        let state = if state != 0 {
            state
        } else {
            c.node().parse_state()
        };
        dbg!(state);
        if state != 0
            && let Some(mut it) = node.language().lookahead_iterator(state)
        {
            ParseErrorReason::Lookahead(it.iter_names().collect())
        } else {
            ParseErrorReason::Unknown
        }
    } else if node.is_missing() {
        ParseErrorReason::MissingToken(node.kind().to_string())
    } else if node.has_error() {
        // A node somewhere down in the tree from here has an error, recursively find it.
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
