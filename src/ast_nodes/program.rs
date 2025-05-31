use super::expression::ExpressionNode;
use colored::Colorize;
use std::fmt;

#[derive(Debug)]
pub struct ProgramNode {
    pub expressions: Vec<ExpressionNode>,
}

impl fmt::Display for ProgramNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", "Program".black().on_green()).unwrap();
        for stmt in &self.expressions {
            writeln!(f, " {}", stmt).unwrap();
        }
        Ok(())
    }
}
