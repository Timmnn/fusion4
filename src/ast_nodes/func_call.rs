use std::fmt;

use super::expression::ExpressionNode;

#[derive(Debug, Clone)]
pub struct FuncCallNode {
    pub name: String,
    pub params: Vec<ExpressionNode>,
    pub generic_params: Vec<String>,
}

impl fmt::Display for FuncCallNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "FuncCall(NAME)")
    }
}
