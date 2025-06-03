use super::{
    block::BlockNode,
    func_call::FuncCallNode,
    func_def::FuncDefNode,
    indent::{Indent, IndentDisplay},
    struct_def::{StructDefNode, StructFieldAccessNode},
    term::{StructInitNode, VarDeclNode},
    var_access::VarAccessNode,
};
use colored::Colorize;
use std::fmt::{Debug, Display, Formatter, Result};

#[derive(Debug, Clone)]
pub struct ExpressionNode {
    pub kind: ExpressionKind,
}

impl IndentDisplay for ExpressionNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(
            f,
            "{}{}",
            indent.as_str(),
            "Expression".on_truecolor(200, 120, 125).black()
        )?;
        self.kind.fmt_with_indent(f, indent.increment(1))
    }
}

impl Display for ExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.fmt_with_indent(f, Indent::new())
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    VarDecl(VarDeclNode),
    //AddExpr(AddExprNode),
    FuncDef(FuncDefNode),
    ReturnExpr(ReturnExprNode),
    CImport(CImportNode),
    FuncCall(FuncCallNode),
    IntLit(i32),
    StrLit(String),
    StructDef(StructDefNode),
    StructFieldAccess(StructFieldAccessNode),
    Import(ImportNode),
    WhileLoop(WhileLoopNode),
    IfStat(IfStatNode),
    BoolExpr(BoolExprNode),
    Reference(Box<ExpressionNode>),
    Deref(Box<ExpressionNode>),
}

#[derive(Debug, Clone)]
pub struct IfStatNode {
    pub condition: Box<ExpressionNode>,
    pub block: BlockNode,
}

impl IndentDisplay for IfStatNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        let _ = writeln!(f, "{}", indent.as_str());
        let _ = write!(f, "{}", indent.as_str());

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct WhileLoopNode {
    pub condition: Box<ExpressionNode>,
    pub block: BlockNode,
}

impl IndentDisplay for WhileLoopNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        let _ = write!(f, "{}{}", indent.as_str(), "");

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ImportNode {
    pub values: Vec<String>,
    pub module: String,
}
impl IndentDisplay for ImportNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        let _ = write!(f, "{}{}", indent.as_str(), self.module);

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum CImportValueType {
    Struct,
    Type,
    Function,
}

#[derive(Debug, Clone)]
pub struct CImportNode {
    pub module: String,
    pub values: Vec<(String, CImportValueType)>,
}
impl IndentDisplay for CImportNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        let _ = write!(f, "{}{}", indent.as_str(), self.module);

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ReturnExprNode {
    pub expression: Box<ExpressionNode>,
}

impl IndentDisplay for ReturnExprNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        self.expression.fmt_with_indent(f, indent.increment(1))?;

        Ok(())
    }
}

impl IndentDisplay for ExpressionKind {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        let string = match self {
            //ExpressionKind::AddExpr(_) => "AddExpr".on_truecolor(100, 149, 237).black(),
            ExpressionKind::BoolExpr(_) => "BoolExpr".on_truecolor(100, 149, 237).black(),
            ExpressionKind::VarDecl(_) => "VarDecl".on_truecolor(100, 150, 200).black(),
            ExpressionKind::FuncDef(_) => "FuncDef".on_truecolor(10, 150, 200).black(),
            ExpressionKind::ReturnExpr(_) => "ReturnExpr".on_truecolor(50, 150, 200).black(),
            ExpressionKind::WhileLoop(_) => "WhileLoop".on_truecolor(50, 150, 200).black(),
            ExpressionKind::Reference(_) => "Reference".on_truecolor(50, 150, 200).black(),
            ExpressionKind::Deref(_) => "Deref".on_truecolor(50, 150, 200).black(),
            ExpressionKind::IfStat(_) => "IfStat".on_truecolor(50, 150, 200).black(),
            ExpressionKind::CImport(node) => format!("CImport({})", node.module)
                .on_truecolor(50, 150, 200)
                .black(),
            ExpressionKind::FuncCall(_) => "FuncCall()".on_truecolor(245, 184, 8).black(),
            ExpressionKind::IntLit(_) => "IntLit()".on_truecolor(25, 67, 1).black(),
            ExpressionKind::StrLit(_) => "StrLit()".on_truecolor(5, 67, 1).black(),
            ExpressionKind::StructDef(_) => "StructDef()".on_truecolor(5, 78, 155).black(),
            ExpressionKind::Import(_) => "Import()".on_truecolor(5, 78, 155).black(),
            ExpressionKind::StructFieldAccess(_) => {
                "StructFieldAccess()".on_truecolor(5, 78, 155).black()
            }
        };

        let indent = indent.increment(1);

        writeln!(f, "{}{}", indent.as_str(), string)?;

        match self {
            //ExpressionKind::AddExpr(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::BoolExpr(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::Reference(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::Deref(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::VarDecl(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::FuncDef(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::WhileLoop(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::IfStat(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::ReturnExpr(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::StructDef(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::CImport(_) => Ok(()),
            ExpressionKind::FuncCall(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::StructFieldAccess(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::Import(node) => node.fmt_with_indent(f, indent.increment(1)),
            ExpressionKind::StrLit(_) => writeln!(
                f,
                "{}{}",
                indent.increment(1).as_str(),
                "StrLit".on_truecolor(200, 120, 125).black()
            ),
            ExpressionKind::IntLit(_) => writeln!(
                f,
                "{}{}",
                indent.increment(1).as_str(),
                "IntLit".on_truecolor(200, 120, 125).black()
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoolExprNode {
    pub lhs: AddExprNode,
    pub comparison: Vec<BoolExprPart>,
}

impl IndentDisplay for BoolExprNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        // Display left part
        writeln!(
            f,
            "{}{}:",
            indent.as_str(),
            "LHS".black().on_truecolor(200, 177, 54)
        )?;
        self.lhs.fmt_with_indent(f, indent.increment(1))?;

        // Display addents if any
        if !self.comparison.is_empty() {
            writeln!(
                f,
                "{}{}",
                indent.as_str(),
                "RHS".on_truecolor(128, 180, 12).black(),
            )?;
            let inner_indent = indent.increment(1);
            for (i, part) in self.comparison.iter().enumerate() {
                writeln!(f, "{}[{}]:", inner_indent.as_str(), i)?;
                part.fmt_with_indent(f, inner_indent.increment(1))?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct BoolExprPart {
    pub operator: BoolOp,
    pub rhs: AddExprNode,
}

impl Display for BoolOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            BoolOp::Equal => write!(f, "=="),
            BoolOp::LessThan => write!(f, "<"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BoolOp {
    Equal,
    LessThan,
}

impl IndentDisplay for BoolExprPart {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(
            f,
            "{}{}({})",
            indent.as_str(),
            "Operator".black().on_truecolor(199, 78, 211),
            self.operator
        )?;
        writeln!(f, "{}Value:", indent.as_str())?;
        self.rhs.fmt_with_indent(f, indent.increment(1))
    }
}

#[derive(Debug, Clone)]
pub struct AddExprNode {
    pub lhs: MulExprNode,
    pub addent: Vec<AddExprPart>,
}

impl IndentDisplay for AddExprNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        // Display left part
        writeln!(
            f,
            "{}{}:",
            indent.as_str(),
            "LHS".black().on_truecolor(200, 177, 54)
        )?;
        self.left.fmt_with_indent(f, indent.increment(1))?;

        // Display addents if any
        if !self.addent.is_empty() {
            writeln!(
                f,
                "{}{}",
                indent.as_str(),
                "RHS".on_truecolor(128, 180, 12).black(),
            )?;
            let inner_indent = indent.increment(1);
            for (i, part) in self.addent.iter().enumerate() {
                writeln!(f, "{}[{}]:", inner_indent.as_str(), i)?;
                part.fmt_with_indent(f, inner_indent.increment(1))?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MulExprPart {
    pub op: MulOp,
    pub value: PrimaryNode,
}

impl IndentDisplay for MulExprPart {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(
            f,
            "{}{}: {}",
            indent.as_str(),
            "Operator".black().on_truecolor(199, 78, 211),
            self.op
        )?;
        writeln!(f, "{}Value:", indent.as_str())?;
        self.value.fmt_with_indent(f, indent.increment(1))
    }
}

#[derive(Debug, Clone)]
pub struct AddExprPart {
    pub op: AddOp,
    pub value: MulExprNode,
}

impl IndentDisplay for AddExprPart {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(
            f,
            "{}{}({})",
            indent.as_str(),
            "Operator".black().on_truecolor(199, 78, 211),
            self.op
        )?;
        writeln!(f, "{}Value:", indent.as_str())?;
        self.value.fmt_with_indent(f, indent.increment(1))
    }
}

#[derive(Debug, Clone)]
pub struct MulExprNode {
    pub left: PrimaryNode,
    pub factor: Vec<MulExprPart>,
}

impl IndentDisplay for MulExprNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(
            f,
            "{}{}",
            indent.as_str(),
            "MulExpr".on_truecolor(255, 165, 0).black()
        )?;

        let inner_indent = indent.increment(1);
        writeln!(
            f,
            "{}{}:",
            inner_indent.as_str(),
            "LHS".black().on_truecolor(200, 177, 54)
        )?;
        self.lhs.fmt_with_indent(f, inner_indent.increment(1))?;

        if !self.factor.is_empty() {
            writeln!(f, "{}Factors:", inner_indent.as_str())?;
            let factors_indent = inner_indent.increment(1);
            for (i, factor) in self.factor.iter().enumerate() {
                writeln!(f, "{}[{}]:", factors_indent.as_str(), i)?;
                factor.fmt_with_indent(f, factors_indent.increment(1))?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct PrimaryNode {
    pub kind: PrimaryKind,
}

impl IndentDisplay for PrimaryNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        writeln!(
            f,
            "{}{}",
            indent.as_str(),
            "Primary".on_truecolor(147, 112, 219).black()
        )?;

        let inner_indent = indent.increment(1);
        match &self.kind {
            PrimaryKind::IntLit(val) => {
                writeln!(
                    f,
                    "{}{}({})",
                    inner_indent.as_str(),
                    "IntLiteral".black().on_truecolor(200, 85, 85),
                    val
                )
            }
            PrimaryKind::StructFieldAccess(node) => {
                writeln!(f, "{}StructFieldAccess({:?})", inner_indent.as_str(), node)
            }
            PrimaryKind::FuncCall(node) => {
                writeln!(f, "{}FuncCall({:?})", inner_indent.as_str(), node)
            }

            PrimaryKind::StructInit(node) => {
                writeln!(f, "{}StructInit({:?})", inner_indent.as_str(), node)
            }
            PrimaryKind::FloatLit(val) => {
                writeln!(f, "{}FloatLiteral({})", inner_indent.as_str(), val)
            }
            PrimaryKind::StrLit(val) => {
                writeln!(f, "{}StringLiteral(\"{}\")", inner_indent.as_str(), val)
            }
            PrimaryKind::VarAccess(expr) => {
                writeln!(f, "{}VarAccess:", inner_indent.as_str())?;
                expr.fmt_with_indent(f, inner_indent.increment(1))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrimaryKind {
    FloatLit(f32),
    StrLit(String),
    VarAccess(VarAccessNode),
    IntLit(i32),
    StructInit(StructInitNode),
    StructFieldAccess(StructFieldAccessNode),
    FuncCall(FuncCallNode),
}

#[derive(Debug, Clone)]
pub enum MulOp {
    Multiply,
    Divide,
}

impl Display for MulOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MulOp::Divide => write!(f, "/"),
            MulOp::Multiply => write!(f, "*"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AddOp {
    Add,
    Subtract, // Fixed typo in the enum variant name
}

impl Display for AddOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            AddOp::Add => write!(f, "+"),
            AddOp::Subtract => write!(f, "-"),
        }
    }
}

// You would need to implement IndentDisplay for other node types as well
// For example:

impl IndentDisplay for VarDeclNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        // Implementation would depend on the actual structure of VarDeclNode
        writeln!(f, "{}Name: {}", indent.as_str(), self.name)?;
        writeln!(f, "{}Value:", indent.as_str())?;
        self.value.fmt_with_indent(f, indent.increment(1))
    }
}

// Stub implementations for the other types
// You would need to replace these with actual implementations

impl IndentDisplay for FuncCallNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        self.params.iter().for_each(|p| {
            let _ = p.fmt_with_indent(f, indent.increment(1));
        });

        Ok(())
    }
}

impl IndentDisplay for BlockNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        self.expressions.iter().for_each(|e| {
            let _ = e.fmt_with_indent(f, indent);
        });

        Ok(())
    }
}

impl IndentDisplay for FuncDefNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: Indent) -> Result {
        self.params.iter().for_each(|p| {
            let _ = p.fmt_with_indent(f, indent);
        });

        let _ = writeln!(f);

        self.body.fmt_with_indent(f, indent.increment(1))
    }
}
