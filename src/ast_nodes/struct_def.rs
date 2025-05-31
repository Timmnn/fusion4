use super::expression::Indent;
use super::expression::IndentDisplay;
use std::fmt::{Debug, Formatter, Result};

#[derive(Debug, Clone)]
pub struct StructDefNode {
    pub name: String,
    pub fields: Vec<StructFieldNode>,
}

#[derive(Debug, Clone)]
pub struct StructFieldNode {
    pub name: String,
    pub type_name: String,
}

impl IndentDisplay for StructDefNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(f, "{}{}", indent.as_str(), self.name)
    }
}

#[derive(Debug, Clone)]
pub struct StructFieldAccessNode {
    pub struct_name: String,
    pub field_name: String,
}

impl IndentDisplay for StructFieldAccessNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(
            f,
            "{}{}.{}",
            indent.as_str(),
            self.struct_name,
            self.field_name
        )
    }
}
