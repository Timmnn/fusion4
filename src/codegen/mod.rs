use std::{collections::HashMap, fmt::format};

use crate::ast_nodes::{
    block::BlockNode,
    expression::{
        AddExprNode, AddOp, CImportNode, ExpressionKind, ExpressionNode, MulExprNode, MulOp,
        PrimaryKind, PrimaryNode, ReturnExprNode,
    },
    func_call::FuncCallNode,
    func_def::{FuncDefNode, FuncParam},
    program::ProgramNode,
    struct_def::{StructDefNode, StructFieldAccessNode},
    term::{StructInitNode, VarDeclNode},
};

struct GenericFuncDeclaration {
    node: FuncDefNode,
    generic_params: Vec<String>,
}

struct Context {
    _scope_stack: Vec<String>,
    _current_scope: u32,
    function_declarations: Vec<String>,
    pub generic_function_declarations: HashMap<String, GenericFuncDeclaration>,
    pub main_function_content: String,
    pub struct_definitions: Vec<String>,
    pub imports: Vec<String>,
    pub generic_function_implementations: HashMap<Vec<String>, String>,
}

impl Context {
    pub fn add_function_declaration(&mut self, code: String) {
        self.function_declarations.push(code);
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            _scope_stack: vec![String::from("Global")],
            function_declarations: vec![],
            struct_definitions: vec![],
            generic_function_declarations: HashMap::new(),
            generic_function_implementations: HashMap::new(),
            _current_scope: 0,
            main_function_content: String::from(""),
            imports: vec![],
        }
    }
}

struct CodeGenResult {
    code: String,
}

pub fn gen_code(program: ProgramNode) -> String {
    let mut ctx = Context::default();
    walk_program(program, &mut ctx);
    let main_function = format!("int main(){{{}return 0;}}", ctx.main_function_content);

    let default_type_defs = ["typedef char* string;"];

    format!(
        "{}{}{}{}{}{}",
        default_type_defs.join(""),
        ctx.struct_definitions.join(""),
        ctx.imports.join(";"),
        ctx.function_declarations.join(";"),
        ctx.generic_function_implementations
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .join(";"),
        main_function
    )
}

fn walk_program(program: ProgramNode, ctx: &mut Context) {
    for statement in &program.expressions {
        let code = walk_expression(statement.clone(), ctx);

        ctx.main_function_content += (code.as_str().to_owned() + ";").as_str();
    }
}

fn walk_expression(expr: ExpressionNode, ctx: &mut Context) -> String {
    match expr.kind {
        ExpressionKind::AddExpr(node) => walk_add_expr(node, ctx),

        ExpressionKind::FuncDef(node) => {
            walk_func_def(node, ctx);

            String::from("")
        }
        ExpressionKind::ReturnExpr(node) => walk_return_expr(node, ctx),
        ExpressionKind::CImport(node) => {
            walk_c_import(node, ctx);
            String::from("")
        }
        ExpressionKind::IntLit(val) => val.to_string(),
        ExpressionKind::FuncCall(node) => walk_func_call(node, ctx),
        ExpressionKind::StrLit(str) => walk_str_lit(str, ctx),
        ExpressionKind::StructDef(node) => {
            walk_struct_def(node, ctx);
            String::from("")
        }
        ExpressionKind::StructFieldAccess(node) => walk_struct_field_access(node, ctx),
        ExpressionKind::VarDecl(node) => walk_var_decl(node, ctx),
    }
}

fn walk_var_decl(node: VarDeclNode, ctx: &mut Context) -> String {
    println!("NODEE  {:?}", node);

    let value = walk_expression(*node.value, ctx);

    format!("{} {} = {}", node.var_type, node.name, value)
}

fn walk_struct_field_access(node: StructFieldAccessNode, _ctx: &mut Context) -> String {
    format!("{}.{}", node.struct_name, node.field_name)
}

fn walk_struct_def(node: StructDefNode, ctx: &mut Context) {
    println!("SAAAAAA   {:?}", node);
    ctx.struct_definitions.push(format!(
        "struct _{name} {{ {} }}; typedef struct _{name} {name};",
        node.fields
            .into_iter()
            .map(|field| format!("{} {};", field.type_name, field.name))
            .collect::<Vec<String>>()
            .join(""),
        name = node.name,
    ));
}

fn walk_str_lit(str: String, _ctx: &mut Context) -> String {
    str
}

fn walk_c_import(node: CImportNode, ctx: &mut Context) {
    ctx.imports
        .push(format!("#include {}\n", node.module.as_str()));
}

fn walk_return_expr(ret: ReturnExprNode, ctx: &mut Context) -> String {
    format!("return {};", walk_expression(*ret.expression, ctx))
}

fn walk_add_expr(add: AddExprNode, ctx: &mut Context) -> String {
    let mut left_code = walk_mul_expr_node(add.left, ctx);

    for addent in add.addent {
        left_code += match addent.op {
            AddOp::Add => format!("+{}", walk_mul_expr_node(addent.value, ctx)),
            AddOp::Subtract => format!("-{}", walk_mul_expr_node(addent.value, ctx)),
        }
        .as_str();
    }

    left_code
}

fn walk_mul_expr_node(mul: MulExprNode, ctx: &mut Context) -> String {
    let mut left_code = walk_primary(mul.left, ctx);

    for factor in mul.factor {
        left_code += match factor.op {
            MulOp::Multiply => format!("*{}", walk_primary(factor.value, ctx)),
            MulOp::Divide => format!("/{}", walk_primary(factor.value, ctx)),
        }
        .as_str()
    }

    left_code
}

fn walk_primary(primary: PrimaryNode, ctx: &mut Context) -> String {
    match primary.kind {
        PrimaryKind::IntLit(val) => val.to_string(),
        PrimaryKind::VarAccess(val) => val.name,
        PrimaryKind::FloatLit(val) => val.to_string(),
        PrimaryKind::StructInit(node) => walk_struct_init(node, ctx),
        PrimaryKind::StrLit(val) => walk_str_lit(val, ctx),
        PrimaryKind::StructFieldAccess(node) => walk_struct_field_access(node, ctx),
    }
}

fn walk_struct_init(node: StructInitNode, ctx: &mut Context) -> String {
    format!(
        "({name}){{ {fields} }}",
        name = node.name,
        fields = node
            .fields
            .into_iter()
            .map(|f| format!(".{} = {}", f.name, walk_expression(*f.value, ctx)))
            .collect::<Vec<String>>()
            .join(", ")
    )
}

fn walk_func_call(func_call: FuncCallNode, ctx: &mut Context) -> String {
    println!("FUNCCALL {:?}", func_call);
    let params_code = func_call
        .clone()
        .params
        .into_iter()
        .map(|param| walk_expression(param, ctx))
        .collect::<Vec<String>>()
        .join(", ");

    let mut new_name = func_call.name.clone();

    if let Some(generic_func) = ctx.generic_function_declarations.get(&func_call.name) {
        let node = generic_func.node.clone();

        println!(
            "dbg_e {:?} {:?}",
            generic_func.generic_params, func_call.generic_params
        );

        let params = func_call
            .generic_params
            .iter()
            .enumerate()
            .map(|(i, p)| FuncParam {
                name: generic_func.node.params[i].name.clone(),
                param_type: p.clone(),
            })
            .collect::<Vec<_>>();

        println!("dbg_d {:?}", params);

        let return_type = if let Some(return_type) = node.return_type {
            if let Some(pos) = generic_func
                .generic_params
                .iter()
                .position(|x| *x == return_type)
            {
                func_call.generic_params[pos].clone()
            } else {
                return_type
            }
        } else {
            // Handle the case where return_type is None
            panic!("No return type specified") // or return a default/error
        };

        new_name = format!("{}_{}", node.name, func_call.generic_params.join("_"));

        walk_func_def(
            FuncDefNode {
                name: new_name.clone(),
                params,
                body: node.body,
                return_type: Some(return_type),
                generic_typing: None,
            },
            ctx,
        );
    }

    format!("{}({})", new_name, params_code)
}

fn walk_block(block: BlockNode, ctx: &mut Context) -> CodeGenResult {
    let results: Vec<String> = block
        .expressions
        .into_iter()
        .map(|expr| walk_expression(expr, ctx) + ";\n")
        .collect();

    CodeGenResult {
        code: results.join(""),
    }
}

fn walk_func_def(node: FuncDefNode, ctx: &mut Context) {
    let code = format!(
        "{} {}({}) {{ {} }}",
        match node.clone().return_type {
            Some(ret_type) => ret_type,
            None => "void".to_string(),
        },
        node.name,
        walk_func_def_params(node.clone().params, ctx),
        walk_block(node.clone().body, ctx).code
    )
    .to_string();

    if let Some(generic_typing) = node.clone().generic_typing {
        ctx.generic_function_declarations.insert(
            node.clone().name,
            GenericFuncDeclaration {
                node,
                generic_params: generic_typing.types,
            },
        );
        return;
    }

    ctx.add_function_declaration(code);
}

fn walk_func_def_params(params: Vec<FuncParam>, _ctx: &mut Context) -> String {
    let x = params
        .into_iter()
        .map(|param| format!("{} {}", param.param_type, param.name))
        .collect::<Vec<String>>();

    x.join(", ")
}
