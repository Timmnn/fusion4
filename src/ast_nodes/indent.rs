use std::fmt::{Formatter, Result};

#[derive(Debug, Clone, Copy)]
pub struct Indent(pub usize);

impl Indent {
    pub fn new() -> Self {
        Indent(0)
    }

    pub fn increment(&self, increment: usize) -> Self {
        Indent(self.0 + increment)
    }

    // Helper to get indent as string
    pub fn as_str(&self) -> String {
        "| ".repeat(self.0)
    }
}

pub trait IndentDisplay {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result;
}
