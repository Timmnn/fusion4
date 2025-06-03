use super::expression::ExpressionNode;
use super::indent::Indent;
use colored::Colorize;
use std::fmt;

#[derive(Debug)]
pub struct ProgramNode {
    pub expressions: Vec<ExpressionNode>,
}

impl fmt::Display for ProgramNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut indent = Indent::new();
        indent = indent.increment(1);

        writeln!(f, "{}", "Program".black().on_green()).unwrap();
        for stmt in &self.expressions {
            writeln!(f, "{}{}", indent.as_str(), stmt).unwrap();
        }
        Ok(())
    }
}
