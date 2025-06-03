use std::{collections::HashMap, fs};

use pest::Parser;

use crate::{
    ast_builder::build_ast_from_pairs,
    ast_nodes::{
        block::BlockNode,
        expression::{
            AddExprNode, AddOp, BoolExprNode, BoolOp, CImportNode, CImportValueType,
            ExpressionKind, ExpressionNode, IfStatNode, ImportNode, MulExprNode, MulOp,
            PrimaryKind, PrimaryNode, ReturnExprNode, WhileLoopNode,
        },
        func_call::FuncCallNode,
        func_def::{FuncDefNode, FuncParam},
        program::ProgramNode,
        struct_def::{StructDefNode, StructFieldAccessNode},
        term::{StructInitNode, VarDeclNode},
    },
    parser::{FusionParser, Rule},
};

struct GenericFuncDeclaration {
    node: FuncDefNode,
    generic_params: Vec<String>,
}

#[derive(Debug)]
struct FuncDeclaration {
    pub compiler_name: String,
    pub code: String,
}

struct Context {
    pub scope_stack: Vec<String>,
    pub function_declarations: HashMap<String, FuncDeclaration>,
    pub generic_function_declarations: HashMap<String, GenericFuncDeclaration>,
    pub main_function_content: String,
    pub struct_definitions: Vec<String>,
    pub imports: Vec<String>,
    pub generic_function_implementations: HashMap<Vec<String>, String>,
    pub compiler_prefix: String,
    pub modules: HashMap<String, Context>,
    pub global_variables: Vec<String>,
}

impl Context {
    pub fn new(prefix: String) -> Self {
        Self {
            scope_stack: vec!["Global".to_string()],
            function_declarations: HashMap::new(),
            struct_definitions: vec![],
            generic_function_declarations: HashMap::new(),
            generic_function_implementations: HashMap::new(),
            main_function_content: String::from(""),
            imports: vec![],
            compiler_prefix: prefix,
            modules: HashMap::new(),
            global_variables: vec![],
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            scope_stack: vec!["Global".to_string()],
            function_declarations: HashMap::new(),
            struct_definitions: vec![],
            generic_function_declarations: HashMap::new(),
            generic_function_implementations: HashMap::new(),
            main_function_content: String::from(""),
            imports: vec![],
            compiler_prefix: "".to_string(),
            modules: HashMap::new(),
            global_variables: vec![],
        }
    }
}

pub fn gen_code(program: ProgramNode, compiler_prefix: String) -> String {
    let mut ctx = Context::new(compiler_prefix);
    walk_program(program, &mut ctx);
    let main_function = format!("int main(){{{}return 0;}}", ctx.main_function_content);

    let default_type_defs = ["typedef char* string;"];

    let modules_code = ctx
        .modules
        .values()
        .map(|ctx| {
            format!(
                "{};",
                ctx.function_declarations
                    .values()
                    .map(|f| f.code.clone())
                    .collect::<Vec<String>>()
                    .join(";")
            )
        })
        .collect::<Vec<String>>()
        .join(";");

    format!(
        "{}{}{}{}{}{}{}",
        ctx.imports.join(";"),
        default_type_defs.join(""),
        ctx.global_variables
            .iter()
            .map(|v| format!("{};", v))
            .collect::<Vec<_>>()
            .join(""),
        modules_code,
        ctx.struct_definitions.join(""),
        /*ctx.function_declarations
        .values()
        .map(|f| f.code.clone())
        .collect::<Vec<String>>()
        .join(";"),*/
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
        //ExpressionKind::AddExpr(node) => walk_add_expr(node, ctx),
        ExpressionKind::BoolExpr(node) => walk_bool_expr(node, ctx),
        ExpressionKind::Reference(node) => format!("(void*)&{}", walk_expression(*node, ctx)),
        ExpressionKind::Deref(node) => format!("*{}", walk_expression(*node, ctx)),
        ExpressionKind::FuncDef(node) => {
            walk_func_def(node, ctx);

            String::from("")
        }
        ExpressionKind::ReturnExpr(node) => walk_return_expr(node, ctx),
        ExpressionKind::CImport(node) => {
            walk_c_import(node, ctx);
            String::from("")
        }
        ExpressionKind::WhileLoop(node) => walk_while_loop(node, ctx),
        ExpressionKind::IfStat(node) => walk_if_stat(node, ctx),
        ExpressionKind::Import(node) => {
            walk_import(node, ctx);
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

fn walk_if_stat(node: IfStatNode, ctx: &mut Context) -> String {
    format!(
        "if({}){{ {} }}",
        walk_expression(*node.condition, ctx),
        walk_block(node.block, ctx)
    )
}

fn walk_while_loop(node: WhileLoopNode, ctx: &mut Context) -> String {
    format!(
        "while({}){{ {} }}",
        walk_expression(*node.condition, ctx),
        walk_block(node.block, ctx)
    )
}

fn walk_import(node: ImportNode, ctx: &mut Context) {
    let module_name = node.module.split("/").last().unwrap().replace(".fu\"", "");

    let mut module_ctx = Context::new(format!("{}_{}", module_name, ctx.compiler_prefix));

    let path = node.module.replace("\"", "");

    let contents = fs::read_to_string(path).expect("Should have been able to read the file");

    let mut rules = FusionParser::parse(Rule::program, &contents).unwrap();

    let pair = rules.next().unwrap();

    let ast = match pair.as_rule() {
        Rule::program => build_ast_from_pairs(pair),
        _ => panic!(
            "Top Level Node can only be a program. Not a {}",
            pair.as_str()
        ),
    };

    walk_program(ast, &mut module_ctx);

    for import in node.values {
        let imported = module_ctx.function_declarations.get(&import).unwrap();

        ctx.function_declarations.insert(
            import,
            FuncDeclaration {
                compiler_name: imported.compiler_name.clone(),
                code: imported.code.clone(),
            },
        );
    }

    ctx.modules.insert(node.module, module_ctx);
}

fn walk_var_decl(node: VarDeclNode, ctx: &mut Context) -> String {
    let value = walk_expression(*node.value, ctx);

    let code = format!("{} {} = {}", node.var_type, node.name, value);

    if ctx.scope_stack.last().unwrap() == "Global" {
        ctx.global_variables.push(code);
        return "".to_string();
    }

    code
}

fn walk_struct_field_access(node: StructFieldAccessNode, _ctx: &mut Context) -> String {
    format!("{}.{}", node.struct_name, node.field_name)
}

fn walk_struct_def(node: StructDefNode, ctx: &mut Context) {
    ctx.struct_definitions.push(format!(
        "struct {prefix}{name} {{ {fields} }}; typedef struct {prefix}{name} {name};",
        prefix = ctx.compiler_prefix,
        fields = node
            .fields
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

    for value in node.values {
        match value.1 {
            CImportValueType::Struct => ctx
                .struct_definitions
                .push(create_struct_alias(value.0, ctx)),
            CImportValueType::Type => {}
            CImportValueType::Function => {
                ctx.function_declarations.insert(
                    value.0.clone(),
                    FuncDeclaration {
                        compiler_name: value.0,
                        code: "".to_string(),
                    },
                );
            }
        }
    }
}

fn create_struct_alias(name: String, _ctx: &Context) -> String {
    format!("; typedef struct {name} {name};", name = name,)
}

fn walk_return_expr(ret: ReturnExprNode, ctx: &mut Context) -> String {
    format!("return {};", walk_expression(*ret.expression, ctx))
}

fn walk_bool_expr(node: BoolExprNode, ctx: &mut Context) -> String {
    let mut lhs_code = walk_add_expr(node.lhs, ctx);

    for addent in node.comparison {
        lhs_code += match addent.operator {
            BoolOp::Equal => format!("=={}", walk_add_expr(addent.rhs, ctx)),
            BoolOp::LessThan => format!("<{}", walk_add_expr(addent.rhs, ctx)),
        }
        .as_str();
    }

    lhs_code
}

fn walk_add_expr(add: AddExprNode, ctx: &mut Context) -> String {
    let mut lhs_code = walk_mul_expr(add.left, ctx);

    for addent in add.addent {
        lhs_code += match addent.op {
            AddOp::Add => format!("+{}", walk_mul_expr(addent.value, ctx)),
            AddOp::Subtract => format!("-{}", walk_mul_expr(addent.value, ctx)),
        }
        .as_str();
    }

    lhs_code
}

fn walk_mul_expr(mul: MulExprNode, ctx: &mut Context) -> String {
    let mut lhs_code = walk_primary(mul.left, ctx);

    for factor in mul.factor {
        lhs_code += match factor.op {
            MulOp::Multiply => format!("*{}", walk_primary(factor.value, ctx)),
            MulOp::Divide => format!("/{}", walk_primary(factor.value, ctx)),
        }
        .as_str()
    }

    lhs_code
}

fn walk_primary(primary: PrimaryNode, ctx: &mut Context) -> String {
    match primary.kind {
        PrimaryKind::IntLit(val) => val.to_string(),
        PrimaryKind::FuncCall(node) => walk_func_call(node, ctx),
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
    let params_code = func_call
        .clone()
        .params
        .into_iter()
        .map(|param| walk_expression(param, ctx))
        .collect::<Vec<String>>()
        .join(", ");

    let mut compiler_name = ctx
        .function_declarations
        .get(&func_call.name)
        .unwrap()
        .compiler_name
        .clone();

    if let Some(generic_func) = ctx.generic_function_declarations.get(&func_call.name) {
        let node = generic_func.node.clone();

        let params = func_call
            .generic_params
            .iter()
            .enumerate()
            .map(|(i, p)| FuncParam {
                name: generic_func.node.params[i].name.clone(),
                param_type: p.clone(),
            })
            .collect::<Vec<_>>();

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

        compiler_name = format!("{}_{}", compiler_name, func_call.generic_params.join("_"));

        walk_func_def(
            FuncDefNode {
                name: compiler_name.clone(),
                params,
                body: node.body,
                return_type: Some(return_type),
                generic_typing: None,
            },
            ctx,
        );
    }

    format!("{}({})", compiler_name, params_code)
}

fn walk_block(block: BlockNode, ctx: &mut Context) -> String {
    ctx.scope_stack.push("Block".to_string());
    let results: Vec<String> = block
        .expressions
        .into_iter()
        .map(|expr| walk_expression(expr, ctx) + ";\n")
        .collect();

    ctx.scope_stack.pop();

    results.join("")
}

fn walk_func_def(node: FuncDefNode, ctx: &mut Context) {
    let name = format!("{}{}", ctx.compiler_prefix, node.name);

    let code = format!(
        "{} {}({}) {{ {} }}",
        match node.clone().return_type {
            Some(ret_type) => ret_type,
            None => "void".to_string(),
        },
        name,
        walk_func_def_params(node.clone().params, ctx),
        walk_block(node.clone().body, ctx)
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

    ctx.function_declarations.insert(
        node.name,
        FuncDeclaration {
            compiler_name: name.clone(),
            code: code.clone(),
        },
    );
}

fn walk_func_def_params(params: Vec<FuncParam>, _ctx: &mut Context) -> String {
    let x = params
        .into_iter()
        .map(|param| format!("{} {}", param.param_type, param.name))
        .collect::<Vec<String>>();

    x.join(", ")
}
