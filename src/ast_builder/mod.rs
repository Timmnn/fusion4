use std::vec;

use crate::ast_nodes::{
    block::BlockNode,
    expression::{
        AddExprNode, AddExprPart, AddOp, CImportNode, CImportValueType, ExpressionKind,
        ExpressionNode, ImportNode, MulExprNode, MulExprPart, MulOp, PrimaryKind, PrimaryNode,
        ReturnExprNode, WhileLoopNode,
    },
    func_call::FuncCallNode,
    func_def::{FuncDefNode, FuncParam, GenericTypingNode},
    program::ProgramNode,
    struct_def::{StructDefNode, StructFieldAccessNode, StructFieldNode},
    term::{StructFieldInitNode, StructInitNode, VarDeclNode},
    var_access::VarAccessNode,
};
use crate::parser::Rule;

type Pair<'a> = pest::iterators::Pair<'a, Rule>;

pub fn build_ast_from_pairs(pair: Pair) -> ProgramNode {
    match pair.as_rule() {
        Rule::program => build_program(pair),
        rule => panic!("Unsupported root rule: {:?}", rule),
    }
}

fn build_program(pair: Pair) -> ProgramNode {
    let expressions = pair
        .into_inner()
        .filter_map(|p| match p.as_rule() {
            Rule::expression => Some(build_expression(p)),
            Rule::EOI => None,
            _ => panic!("Invalid node in program: {:?}", p.as_rule()),
        })
        .collect::<Vec<ExpressionNode>>();

    ProgramNode { expressions }
}

fn build_expression(pair: Pair) -> ExpressionNode {
    let mut inner = pair.into_inner();

    let expr = inner.next().expect("Expression has to have a child node");

    let expression_kind = match expr.as_rule() {
        Rule::var_decl => ExpressionKind::VarDecl(build_var_decl(expr)),
        Rule::add_expr => ExpressionKind::AddExpr(build_add_expr(expr)),
        Rule::func_def => ExpressionKind::FuncDef(build_func_def(expr)),
        Rule::return_expr => ExpressionKind::ReturnExpr(build_return_expr(expr)),
        Rule::c_import => ExpressionKind::CImport(build_c_import(expr)),
        Rule::func_call => ExpressionKind::FuncCall(build_func_call(expr)),
        Rule::int_lit => ExpressionKind::IntLit(expr.as_str().parse().unwrap()),
        Rule::str_lit => ExpressionKind::StrLit(expr.as_str().to_string()),
        Rule::struct_def => ExpressionKind::StructDef(build_struct_def(expr)),
        Rule::struct_field_access => {
            ExpressionKind::StructFieldAccess(build_struct_field_access(expr))
        }
        Rule::import => ExpressionKind::Import(build_import(expr)),
        Rule::while_loop => ExpressionKind::WhileLoop(build_while_loop(expr)),
        _ => panic!("Invalid node in expression: {:?}", expr.as_rule()),
    };

    ExpressionNode {
        kind: expression_kind,
    }
}

fn build_while_loop(pair: Pair) -> WhileLoopNode {
    let mut inner = pair.into_inner();

    let condition = Box::new(build_expression(inner.next().unwrap()));
    let block = build_block(inner.next().unwrap());

    WhileLoopNode { condition, block }
}

fn build_import(pair: Pair) -> ImportNode {
    let inner = pair.into_inner();

    let mut values = vec![];
    let mut name = None;

    for pair in inner {
        match pair.as_rule() {
            Rule::ident => values.push(pair.as_str().to_string()),
            Rule::str_lit => name = Some(pair.as_str().to_string()),
            _ => panic!("Invalid pair in import {:?}", pair.as_rule()),
        }
    }

    ImportNode {
        values,
        module: name.unwrap(),
    }
}

fn build_struct_field_access(pair: Pair) -> StructFieldAccessNode {
    let mut inner = pair.into_inner();
    StructFieldAccessNode {
        struct_name: inner.next().unwrap().as_str().to_string(),
        field_name: inner.next().unwrap().as_str().to_string(),
    }
}

fn build_struct_def(pair: Pair) -> StructDefNode {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_str().to_string();

    let mut fields = vec![];

    let struct_def_content = inner.next().unwrap();

    for field_def in struct_def_content.into_inner() {
        fields.push(build_struct_field_def(field_def));
    }

    StructDefNode { name, fields }
}

fn build_struct_field_def(pair: Pair) -> StructFieldNode {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let type_name = inner.next().unwrap().as_str().to_string();

    StructFieldNode { name, type_name }
}

fn build_c_import(pair: Pair) -> CImportNode {
    let mut inner = pair.into_inner();

    let string = inner.next().unwrap().as_str().to_string();

    let mut values = vec![];
    while inner.len() > 0 {
        println!("dbg {}", inner);

        let name = inner.next().unwrap().as_str().to_string();
        let import_type = inner.next().unwrap();
        values.push((name, CImportValueType::Struct));
    }

    CImportNode {
        module: string[1..(string.len() - 1)].to_string(),
        values,
    }
}
fn build_return_expr(pair: Pair) -> ReturnExprNode {
    let mut inner = pair.into_inner();

    let expr = build_expression(inner.next().unwrap());

    ReturnExprNode {
        expression: Box::new(expr),
    }
}

fn build_add_expr(pair: Pair) -> AddExprNode {
    let mut inner = pair.into_inner();

    let left_pair = inner.next().unwrap();
    let left = build_mul_expr(left_pair);

    let mut addent = vec![];
    while inner.len() > 0 {
        let op_pair = inner.next().unwrap();

        let op = match op_pair.as_rule() {
            Rule::add => AddOp::Add,
            Rule::subtract => AddOp::Subtract,
            rule => panic!("{:?}", rule),
        };

        let value = build_mul_expr(inner.next().unwrap());

        addent.push(AddExprPart { op, value });
    }

    AddExprNode { left, addent }
}

fn build_mul_expr(pair: Pair) -> MulExprNode {
    let mut inner = pair.into_inner();

    let primary_pair = inner.next().unwrap();
    let left = build_primary(primary_pair);

    let mut factor = vec![];
    while inner.len() > 0 {
        let op_pair = inner.next().unwrap();

        let op = match op_pair.as_rule() {
            Rule::multiply => MulOp::Multiply,
            Rule::divide => MulOp::Divide,
            rule => panic!("{:?}", rule),
        };

        let value = build_primary(inner.next().unwrap());

        factor.push(MulExprPart { op, value });
    }

    MulExprNode { left, factor }
}

fn build_primary(pair: Pair) -> PrimaryNode {
    let mut inner = pair.into_inner();

    let primary = inner.next().unwrap();

    let kind = match primary.as_rule() {
        Rule::var_access => PrimaryKind::VarAccess(build_var_access(primary)),
        Rule::int_lit => PrimaryKind::IntLit(primary.as_str().trim().parse().unwrap()),
        Rule::str_lit => PrimaryKind::StrLit(primary.as_str().to_string()),
        Rule::float_lit => PrimaryKind::FloatLit(primary.as_str().parse().unwrap()),
        Rule::struct_init => PrimaryKind::StructInit(build_struct_init(primary)),
        Rule::struct_field_access => {
            PrimaryKind::StructFieldAccess(build_struct_field_access(primary))
        }
        _ => todo!("{:?}", primary),
    };

    PrimaryNode { kind }
}

fn build_struct_init(pair: Pair) -> StructInitNode {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_str().to_string();
    let mut fields = vec![];

    for pair in inner {
        fields.push(build_struct_field_init(pair));
    }

    StructInitNode { name, fields }
}

fn build_struct_field_init(pair: Pair) -> StructFieldInitNode {
    let mut inner = pair.into_inner();
    StructFieldInitNode {
        name: inner.next().unwrap().as_str().to_string(),
        value: Box::new(build_expression(inner.next().unwrap())),
    }
}

fn build_var_access(pair: Pair) -> VarAccessNode {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_str().to_string();

    VarAccessNode { name }
}

fn build_func_call(pair: Pair) -> FuncCallNode {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_str().to_string();

    let mut param_list = None;
    let mut generic_params = None;

    for rule in inner {
        match rule.as_rule() {
            Rule::param_list => param_list = Some(rule),
            Rule::generic_params => generic_params = Some(rule),
            _ => panic!("Invalid rule in func_call {}", rule),
        };
    }

    let params = match param_list {
        None => vec![],
        Some(list) => list
            .into_inner()
            .map(|e| match e.as_rule() {
                Rule::expression => build_expression(e),
                _ => panic!("Invalid Rule {:?} in param_list", e.as_rule()),
            })
            .collect(),
    };

    let generic_params = match generic_params {
        Some(p) => build_generic_params(p),
        None => vec![],
    };

    FuncCallNode {
        name,
        params,
        generic_params,
    }
}

fn build_generic_params(pair: Pair) -> Vec<String> {
    pair.into_inner().map(|p| p.as_str().to_string()).collect()
}

fn build_var_decl(pair: Pair) -> VarDeclNode {
    let mut inner = pair.into_inner();

    let ident = inner.next().unwrap().as_str().to_string();
    let expression = build_expression(inner.next().unwrap());
    let var_type = inner.next().unwrap().as_str().to_string();

    VarDeclNode {
        name: ident,
        value: Box::new(expression),
        var_type,
    }
}

fn build_func_def(pair: Pair) -> FuncDefNode {
    let mut inner = pair.into_inner();

    let name = inner
        .next()
        .expect("Function requires a name")
        .as_str()
        .to_string();
    let mut param_def_list = None;
    let mut return_type = None;
    let mut body = None;
    let mut generic_typing = None;

    for node in inner {
        match node.as_rule() {
            Rule::param_def_list => param_def_list = Some(build_param_def_list(node)),
            Rule::block => body = Some(build_block(node)),
            Rule::return_type => return_type = Some(build_return_type(node)),
            Rule::generic_typing => generic_typing = Some(build_generic_typing(node)),
            _ => panic!("{}", node),
        };
    }

    FuncDefNode {
        name,
        params: param_def_list.unwrap_or(vec![]),
        body: body.unwrap(),
        generic_typing,
        return_type,
    }
}

fn build_generic_typing(pair: Pair) -> GenericTypingNode {
    let inner = pair.into_inner();

    let generic_params = inner
        .map(|p| p.as_str().to_string())
        .collect::<Vec<String>>();

    GenericTypingNode {
        types: generic_params,
    }
}

fn build_block(pair: Pair) -> BlockNode {
    let expressions = pair
        .into_inner()
        .map(|n| match n.as_rule() {
            Rule::expression => build_expression(n),

            _ => panic!("Invalid node in block: {:?}", n.as_rule()),
        })
        .collect();

    BlockNode { expressions }
}

fn build_param_def_list(pair: Pair) -> Vec<FuncParam> {
    let inner = pair.into_inner();

    inner.map(|p| build_field_def(p)).collect()
}

fn build_field_def(pair: Pair) -> FuncParam {
    let mut inner = pair.into_inner();
    FuncParam {
        name: inner.next().unwrap().as_str().to_string(),
        param_type: inner.next().unwrap().as_str().to_string(),
    }
}

fn build_return_type(pair: Pair) -> String {
    pair.as_str().to_string()
}
