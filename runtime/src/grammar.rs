//! Grammar related functions.
use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

// NOTE: This could be useful for generating the grammar in the first place instead of just
// building json! values directly.

/// Type for the JSON representation of a grammar, mostly copied from `tree_sitter_generate`.
#[derive(Deserialize, Serialize)]
pub struct Grammar {
    pub name: String,
    pub word: Option<String>,
    // NOTE: Use `indexmap` because we need to preserve order.
    // https://docs.rs/indexmap/2.10.0/indexmap/map/struct.IndexMap.html
    pub rules: HashMap<String, RuleDef>,
    pub extras: Vec<RuleDef>,
}

#[derive(Deserialize, Serialize)]
#[serde(tag = "type")]
#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
pub enum RuleDef {
    ALIAS {
        content: Box<RuleDef>,
        named: bool,
        value: String,
    },
    BLANK,
    STRING {
        value: String,
    },
    PATTERN {
        value: String,
        flags: Option<String>,
    },
    SYMBOL {
        name: String,
    },
    CHOICE {
        members: Vec<RuleDef>,
    },
    FIELD {
        name: String,
        content: Box<RuleDef>,
    },
    SEQ {
        members: Vec<RuleDef>,
    },
    REPEAT {
        content: Box<RuleDef>,
    },
    REPEAT1 {
        content: Box<RuleDef>,
    },
    PREC_DYNAMIC {
        value: i32,
        content: Box<RuleDef>,
    },
    PREC_LEFT {
        value: PrecedenceValue,
        content: Box<RuleDef>,
    },
    PREC_RIGHT {
        value: PrecedenceValue,
        content: Box<RuleDef>,
    },
    PREC {
        value: PrecedenceValue,
        content: Box<RuleDef>,
    },
    TOKEN {
        content: Box<RuleDef>,
    },
    IMMEDIATE_TOKEN {
        content: Box<RuleDef>,
    },
    RESERVED {
        context_name: String,
        content: Box<RuleDef>,
    },
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum PrecedenceValue {
    Integer(i32),
    Name(String),
}

impl Grammar {
    /// Starting from `rule_name`, find all symbols (named or anonymous) which can be reached.
    pub fn reachable_set<'a>(&'a self, rule_name: &str) -> Option<HashSet<&'a str>> {
        let mut set = HashSet::new();
        let (name, rule) = self.rules.get_key_value(rule_name)?;
        set.insert(name.as_str());
        self.compute_reachable(rule, &mut set)?;
        Some(set)
    }

    fn compute_reachable<'a>(
        &'a self,
        rule: &'a RuleDef,
        set: &mut HashSet<&'a str>,
    ) -> Option<()> {
        match rule {
            RuleDef::ALIAS {
                content,
                named: _,
                value,
            } => {
                if set.insert(value) {
                    self.compute_reachable(content, set)?;
                }
            }
            RuleDef::BLANK => {}
            RuleDef::STRING { value } => {
                set.insert(value.as_str());
            }
            RuleDef::PATTERN { value: _, flags: _ } => {}
            RuleDef::SYMBOL { name } => {
                // Don't repeat if we have already seen it before.
                if set.insert(name.as_str()) {
                    let rule = self.rules.get(name)?;
                    self.compute_reachable(rule, set)?;
                }
            }
            RuleDef::CHOICE { members } => {
                for member in members {
                    self.compute_reachable(member, set)?;
                }
            }
            RuleDef::FIELD { name: _, content } => self.compute_reachable(content, set)?,
            RuleDef::SEQ { members } => {
                for member in members {
                    self.compute_reachable(member, set)?;
                }
            }
            RuleDef::REPEAT { content } => self.compute_reachable(content, set)?,
            RuleDef::REPEAT1 { content } => self.compute_reachable(content, set)?,
            RuleDef::PREC_DYNAMIC { value: _, content } => self.compute_reachable(content, set)?,
            RuleDef::PREC_LEFT { value: _, content } => self.compute_reachable(content, set)?,
            RuleDef::PREC_RIGHT { value: _, content } => self.compute_reachable(content, set)?,
            RuleDef::PREC { value: _, content } => self.compute_reachable(content, set)?,
            RuleDef::TOKEN { content } => self.compute_reachable(content, set)?,
            RuleDef::IMMEDIATE_TOKEN { content } => self.compute_reachable(content, set)?,
            RuleDef::RESERVED {
                context_name: _,
                content,
            } => self.compute_reachable(content, set)?,
        }

        Some(())
    }
}
