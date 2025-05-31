use super::program::ProgramNode;

#[derive(Debug)]
pub enum AstNode {
    Program(Box<ProgramNode>),
    Integer(i32),
    Expression(Box<ExpressionNode>),
}

// Implement Display for AstNode
impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstNode::Program(prog) => write!(f, "{}", prog),
            AstNode::Integer(val) => write!(f, "{}", val),
            AstNode::Expression(expr) => write!(f, "{}", expr),
        }
    }
}
