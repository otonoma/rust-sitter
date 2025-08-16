use tree_sitter::Node;

use crate::{Extract, NodeParseResult, ParseResult, extract::ExtractContext};

pub trait Rule<Output>: Extract<Output> {
    // TODO: Use the grammar::RuleDef and grammar::Grammar
    // For this to work as expected we need a #[derive(Language)], or at least a `Language` trait
    // which then has the `parse` function and the `generate_grammar() -> grammar::Grammar`
    // implementation instead of just producing an ast.
    // Since we aren't using any of this yet though, we will leave this alone.
    fn produce_ast() -> String;
    // Maybe Cow instead.
    fn rule_name() -> &'static str;

    /// Extracts directly from a node.
    fn extract_node<'a>(n: Node<'a>, source: &[u8]) -> NodeParseResult<'a, Output>
    where
        Self: Sized,
    {
        let mut ctx = ExtractContext {
            last_pt: n.start_position(),
            last_idx: n.start_byte(),
            node_kind: n.kind(),
            // TODO: ???
            field_name: "",
        };
        // Extract the errors, and try to parse anyway.
        let mut errors = vec![];
        if n.has_error() {
            crate::error::collect_node_errors(n, |e| errors.push(e));
        }
        let result = Self::extract(&mut ctx, Some(n), source, None);
        NodeParseResult { result, errors }
    }
}

pub trait Language: Sized {
    fn produce_grammar() -> String;

    fn language() -> tree_sitter::Language;
    fn parse(input: &str) -> ParseResult<Self>;
}
