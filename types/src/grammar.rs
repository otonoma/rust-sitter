//! Grammar related functions.
use std::collections::HashSet;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

// NOTE: This could be useful for generating the grammar in the first place instead of just
// building json! values directly.

/// Type for the JSON representation of a grammar, mostly copied from `tree_sitter_generate`.
#[derive(Deserialize, Serialize)]
pub struct Grammar {
    pub name: String,
    pub word: Option<String>,
    pub rules: IndexMap<String, RuleDef>,
    pub extras: Vec<RuleDef>,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
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
        #[serde(skip_serializing_if = "Option::is_none")]
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

impl RuleDef {
    pub fn is_symbol(&self) -> bool {
        matches!(self, RuleDef::SYMBOL { .. })
    }

    pub fn is_blank(&self) -> bool {
        matches!(self, RuleDef::BLANK)
    }

    pub fn optional(rule: RuleDef) -> RuleDef {
        RuleDef::CHOICE {
            members: vec![RuleDef::BLANK, rule],
        }
    }

    pub fn as_optional(&self) -> Option<&RuleDef> {
        match self {
            Self::CHOICE { members } => match members.as_slice() {
                &[ref rule, RuleDef::BLANK] | &[RuleDef::BLANK, ref rule] => Some(rule),
                _ => None,
            },
            Self::PREC { value: _, content }
            | Self::PREC_LEFT { value: _, content }
            | Self::PREC_RIGHT { value: _, content }
            | Self::PREC_DYNAMIC { value: _, content } => content.as_optional(),
            _ => None,
        }
    }

    /// Pull out a sequence, including through precedence unwrapping.
    pub fn as_seq(&self) -> Option<&[RuleDef]> {
        match self {
            Self::SEQ { members } => Some(members),
            Self::PREC { value: _, content }
            | Self::PREC_LEFT { value: _, content }
            | Self::PREC_RIGHT { value: _, content }
            | Self::PREC_DYNAMIC { value: _, content } => content.as_seq(),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum PrecedenceValue {
    Integer(i32),
    Name(String),
}

impl From<i32> for PrecedenceValue {
    fn from(value: i32) -> Self {
        Self::Integer(value)
    }
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
