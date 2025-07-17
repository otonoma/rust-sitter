
pub trait Rule<Output> {
    // TODO: Consider using serde_json::Value instead. Or just a serialized actual type
    // representing the different constructs...
    fn produce_ast() -> String;
    // Maybe Cow instead.
    fn rule_name() -> &'static str;
}

// ...like this.
// pub enum TreeSitterType {
//     Choice(TreeSitterChoice),
//     Seq(TreeSitterSeq),
// }
