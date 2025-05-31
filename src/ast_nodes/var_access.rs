use super::expression::Indent;
use super::expression::IndentDisplay;
use std::fmt::{Formatter, Result};

#[derive(Debug, Clone)]
pub struct VarAccessNode {
    pub name: String,
}

impl IndentDisplay for VarAccessNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(f, "{}{}", indent.as_str(), self.name)
    }
}
