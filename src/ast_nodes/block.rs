use super::expression::ExpressionNode;
use std::fmt;

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub expressions: Vec<ExpressionNode>,
}

impl fmt::Display for BlockNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Block({:?})", self.expressions)
    }
}
