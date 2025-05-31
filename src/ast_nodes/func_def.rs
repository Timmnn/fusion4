use super::expression::Indent;
use std::fmt::{Debug, Formatter, Result};

use super::{block::BlockNode, expression::IndentDisplay};

#[derive(Debug, Clone)]
pub struct FuncDefNode {
    pub name: String,
    pub params: Vec<FuncParam>,
    pub body: BlockNode,
    pub return_type: Option<String>,
    pub generic_typing: Option<GenericTypingNode>,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: String,
    pub param_type: String,
}

impl IndentDisplay for FuncParam {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(f, "{}{}: {}", indent.as_str(), self.name, self.param_type)
    }
}

#[derive(Debug, Clone)]
pub struct GenericTypingNode {
    pub types: Vec<String>,
}
