use super::{
    expression::ExpressionNode,
    indent::{Indent, IndentDisplay},
};
use std::fmt::{Debug, Formatter, Result};

#[derive(Debug, Clone)]
pub struct VarDeclNode {
    pub name: String,
    pub value: Box<ExpressionNode>,
    pub var_type: String,
}

#[derive(Debug, Clone)]
pub struct StructInitNode {
    pub name: String,
    pub fields: Vec<StructFieldInitNode>,
}

impl IndentDisplay for StructInitNode {
    fn fmt_with_indent(&self, _f: &mut Formatter<'_>, _indent: Indent) -> Result {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StructFieldInitNode {
    pub name: String,
    pub value: Box<ExpressionNode>,
}

impl IndentDisplay for StructFieldInitNode {
    fn fmt_with_indent(&self, _f: &mut Formatter<'_>, _indent: Indent) -> Result {
        Ok(())
    }
}
