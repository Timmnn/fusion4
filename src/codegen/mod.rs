mod context;
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
use context::{
    CImportedFunctionDeclaration, Context, FunctionDeclaration, RegularFunctionDeclaration,
    StructDefinition,
};
use pest::Parser;
use std::fs;

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
                    .map(|f| {
                        match f {
                            FunctionDeclaration::Regular(decl) => decl.c_code.clone(),
                            _ => "".to_string(),
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(";")
            )
        })
        .collect::<Vec<String>>()
        .join(";");

    format!(
        "{}{}{}{}{}{}{}",
        ctx.imports
            .into_iter()
            .map(|i| i + "\n")
            .collect::<Vec<_>>()
            .join(""),
        default_type_defs.join(""),
        ctx.global_variables
            .iter()
            .map(|v| format!("{};", v))
            .collect::<Vec<_>>()
            .join(""),
        modules_code,
        ctx.struct_definitions
            .into_iter()
            .map(|s| s.c_code)
            .collect::<Vec<_>>()
            .join(""),
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

        ctx.main_function_content += (code.c_code.as_str().to_owned() + ";").as_str();
    }
}

struct TypedExpression {
    pub expr_type: String,
    pub c_code: String,
}

fn walk_expression(expr: ExpressionNode, ctx: &mut Context) -> TypedExpression {
    match expr.kind {
        //ExpressionKind::AddExpr(node) => walk_add_expr(node, ctx),
        ExpressionKind::BoolExpr(node) => walk_bool_expr(node, ctx),
        ExpressionKind::Reference(node) => TypedExpression {
            c_code: format!("(void*)&{}", walk_expression(*node, ctx).c_code),
            expr_type: "void*".to_string(),
        },
        ExpressionKind::Deref(node) => {
            let res = walk_expression(*node, ctx);

            TypedExpression {
                c_code: format!("*{}", res.c_code),
                expr_type: res.expr_type,
            }
        }
        ExpressionKind::FuncDef(node) => {
            walk_func_def(node, ctx);

            TypedExpression {
                c_code: "".to_string(),
                expr_type: "()".to_string(),
            }
        }
        ExpressionKind::ReturnExpr(node) => TypedExpression {
            c_code: walk_return_expr(node, ctx),
            expr_type: "()".to_string(),
        },
        ExpressionKind::CImport(node) => {
            walk_c_import(node, ctx);
            TypedExpression {
                c_code: "".to_string(),
                expr_type: "()".to_string(),
            }
        }
        ExpressionKind::WhileLoop(node) => TypedExpression {
            c_code: walk_while_loop(node, ctx),
            expr_type: "()".to_string(),
        },
        ExpressionKind::IfStat(node) => TypedExpression {
            c_code: walk_if_stat(node, ctx),
            expr_type: "()".to_string(),
        },
        ExpressionKind::Import(node) => {
            walk_import(node, ctx);
            TypedExpression {
                c_code: "".to_string(),
                expr_type: "()".to_string(),
            }
        }
        ExpressionKind::IntLit(val) => TypedExpression {
            c_code: val.to_string(),
            expr_type: "int".to_string(),
        },
        ExpressionKind::FuncCall(node) => TypedExpression {
            c_code: walk_func_call(node, ctx),
            expr_type: "todo".to_string(),
        },
        ExpressionKind::StrLit(str) => TypedExpression {
            c_code: walk_str_lit(str, ctx),
            expr_type: "string".to_string(),
        },
        ExpressionKind::StructDef(node) => {
            walk_struct_def(node, ctx);

            TypedExpression {
                c_code: "".to_string(),
                expr_type: "()".to_string(),
            }
        }
        ExpressionKind::StructFieldAccess(node) => TypedExpression {
            c_code: walk_struct_field_access(node, ctx),
            expr_type: "todo".to_string(),
        },
        ExpressionKind::VarDecl(node) => TypedExpression {
            c_code: walk_var_decl(node, ctx),
            expr_type: "todo".to_string(),
        },
    }
}

fn walk_if_stat(node: IfStatNode, ctx: &mut Context) -> String {
    format!(
        "if({}){{ {} }}",
        walk_expression(*node.condition, ctx).c_code,
        walk_block(node.block, ctx)
    )
}

fn walk_while_loop(node: WhileLoopNode, ctx: &mut Context) -> String {
    format!(
        "while({}){{ {} }}",
        walk_expression(*node.condition, ctx).c_code,
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

        let imported = match imported {
            FunctionDeclaration::CImported(x) => x,
            _ => unreachable!(),
        };

        ctx.function_declarations.insert(
            import,
            FunctionDeclaration::Regular(RegularFunctionDeclaration {
                compiler_name: imported.name.clone(),
                c_code: "".to_string(),
            }),
        );
    }

    ctx.modules.insert(node.module, module_ctx);
}

fn walk_var_decl(node: VarDeclNode, ctx: &mut Context) -> String {
    let expr_result = walk_expression(*node.value, ctx);

    let var_type = match node.var_type {
        Some(t) => t,
        None => expr_result.expr_type,
    };

    let code = format!("{} {} = {}", var_type, node.name, expr_result.c_code);

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
    let c_code = format!(
        "struct {prefix}{name} {{ {fields} }}; typedef struct {prefix}{name} {name};",
        prefix = ctx.compiler_prefix,
        fields = node
            .fields
            .clone()
            .into_iter()
            .map(|field| format!("{} {};", field.type_name, field.name))
            .collect::<Vec<String>>()
            .join(""),
        name = node.name,
    );

    ctx.struct_definitions.push(StructDefinition {
        name: node.name,
        c_code,
        fields: node.fields.into_iter().map(|f| f.name).collect::<Vec<_>>(),
    });
}

fn walk_str_lit(str: String, _ctx: &mut Context) -> String {
    str
}

fn walk_c_import(node: CImportNode, ctx: &mut Context) {
    ctx.imports
        .push(format!("#include {}", node.module.as_str()));

    for value in node.values {
        match value.1 {
            CImportValueType::Struct => {
                ctx.struct_definitions.push(StructDefinition {
                    name: value.0.clone(),
                    c_code: create_struct_alias(value.0, ctx),
                    fields: vec![],
                });
            }
            CImportValueType::Type => {}
            CImportValueType::Function => {
                ctx.function_declarations.insert(
                    value.0.clone(),
                    FunctionDeclaration::CImported(CImportedFunctionDeclaration { name: value.0 }),
                );
            }
        }
    }
}

fn create_struct_alias(name: String, _ctx: &Context) -> String {
    format!("; typedef struct {name} {name};", name = name,)
}

fn walk_return_expr(ret: ReturnExprNode, ctx: &mut Context) -> String {
    format!("return {};", walk_expression(*ret.expression, ctx).c_code)
}

fn walk_bool_expr(node: BoolExprNode, ctx: &mut Context) -> TypedExpression {
    let result = walk_add_expr(node.lhs, ctx);

    let mut c_code = result.c_code;

    for addent in node.comparison.clone() {
        c_code += match addent.operator {
            BoolOp::Equal => format!("=={}", walk_add_expr(addent.rhs, ctx).c_code),
            BoolOp::LessThan => format!("<{}", walk_add_expr(addent.rhs, ctx).c_code),
        }
        .as_str();
    }

    TypedExpression {
        c_code,
        expr_type: if node.comparison.is_empty() {
            result.expr_type
        } else {
            "boolean".to_string()
        },
    }
}

fn walk_add_expr(add: AddExprNode, ctx: &mut Context) -> TypedExpression {
    let result = walk_mul_expr(add.lhs, ctx);

    let mut c_code = result.c_code;

    for addent in add.addent.clone() {
        c_code += match addent.op {
            AddOp::Add => format!("+{}", walk_mul_expr(addent.value, ctx).c_code),
            AddOp::Subtract => format!("-{}", walk_mul_expr(addent.value, ctx).c_code),
        }
        .as_str();
    }

    TypedExpression {
        c_code,
        expr_type: if add.addent.is_empty() {
            result.expr_type
        } else {
            "int".to_string()
        },
    }
}

fn walk_mul_expr(mul: MulExprNode, ctx: &mut Context) -> TypedExpression {
    let result = walk_primary(mul.lhs, ctx);

    let mut c_code = result.c_code;

    for factor in mul.rhs.clone() {
        c_code += match factor.op {
            MulOp::Multiply => format!("*{}", walk_primary(factor.value, ctx).c_code),
            MulOp::Divide => format!("/{}", walk_primary(factor.value, ctx).c_code),
        }
        .as_str()
    }

    TypedExpression {
        c_code,
        expr_type: if mul.rhs.is_empty() {
            result.expr_type
        } else {
            "boolean".to_string()
        },
    }
}

fn walk_primary(primary: PrimaryNode, ctx: &mut Context) -> TypedExpression {
    match primary.kind {
        PrimaryKind::IntLit(val) => TypedExpression {
            c_code: val.to_string(),
            expr_type: "int".to_string(),
        },
        PrimaryKind::FuncCall(node) => TypedExpression {
            expr_type: "todo".to_string(),
            c_code: walk_func_call(node, ctx),
        },
        PrimaryKind::VarAccess(node) => TypedExpression {
            expr_type: "todo".to_string(),
            c_code: node.name,
        },
        PrimaryKind::FloatLit(node) => TypedExpression {
            expr_type: "float".to_string(),
            c_code: node.to_string(),
        },

        PrimaryKind::StructInit(node) => TypedExpression {
            expr_type: "todo".to_string(),
            c_code: walk_struct_init(node, ctx),
        },

        PrimaryKind::StrLit(node) => TypedExpression {
            expr_type: "string".to_string(),
            c_code: walk_str_lit(node, ctx),
        },

        PrimaryKind::StructFieldAccess(node) => TypedExpression {
            expr_type: "todo".to_string(),
            c_code: walk_struct_field_access(node, ctx),
        },
    }
}

fn walk_struct_init(node: StructInitNode, ctx: &mut Context) -> String {
    format!(
        "({name}){{ {fields} }}",
        name = node.name,
        fields = node
            .fields
            .into_iter()
            .map(|f| format!(".{} = {}", f.name, walk_expression(*f.value, ctx).c_code))
            .collect::<Vec<String>>()
            .join(", ")
    )
}

fn walk_func_call(func_call: FuncCallNode, ctx: &mut Context) -> String {
    let params_code = func_call
        .clone()
        .params
        .into_iter()
        .map(|param| walk_expression(param, ctx).c_code)
        .collect::<Vec<String>>()
        .join(", ");

    let code = match ctx.function_declarations.get(&func_call.name).unwrap() {
        FunctionDeclaration::Generic(f) => {
            let node = f.ast_node.clone();

            let params = func_call
                .generic_params
                .iter()
                .enumerate()
                .map(|(i, p)| FuncParam {
                    name: node.params[i].name.clone(),
                    param_type: p.clone(),
                })
                .collect::<Vec<_>>();

            let return_type = if let Some(return_type) = node.return_type {
                if let Some(pos) = f.generic_params.iter().position(|x| *x == return_type) {
                    func_call.generic_params[pos].clone()
                } else {
                    return_type
                }
            } else {
                // Handle the case where return_type is None
                panic!("No return type specified") // or return a default/error
            };

            let compiler_name = format!("{}_{}", "todo", func_call.generic_params.join("_"));

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

            "".to_string()
        }
        FunctionDeclaration::Regular(f) => format!("{}({})", f.compiler_name, params_code),
        FunctionDeclaration::CImported(f) => format!("{}({})", f.name, params_code),
    };

    code
}

fn walk_block(block: BlockNode, ctx: &mut Context) -> String {
    ctx.scope_stack.push("Block".to_string());
    let results: Vec<String> = block
        .expressions
        .into_iter()
        .map(|expr| walk_expression(expr, ctx).c_code + ";\n")
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
        walk_func_def_params(node.clone().params, ctx).c_code,
        walk_block(node.clone().body, ctx)
    )
    .to_string();

    if let Some(generic_typing) = node.clone().generic_typing {
        ctx.function_declarations.insert(
            node.clone().name,
            FunctionDeclaration::Generic(context::GenericFunctionDeclaration {
                ast_node: node,
                generic_params: generic_typing.types,
            }),
        );
        return;
    }

    ctx.function_declarations.insert(
        node.name,
        FunctionDeclaration::Regular(RegularFunctionDeclaration {
            compiler_name: name.clone(),
            c_code: code.clone(),
        }),
    );
}

struct WalkFuncDefParamsResult {
    pub c_code: String,
    /// (name, type)
    pub params: Vec<(String, String)>,
}

fn walk_func_def_params(params: Vec<FuncParam>, _ctx: &mut Context) -> WalkFuncDefParamsResult {
    let mut parsed_params = vec![];
    let c_code = params
        .into_iter()
        .map(|param| {
            parsed_params.push((param.name.clone(), param.param_type.clone()));

            format!("{} {}", param.param_type, param.name)
        })
        .collect::<Vec<String>>()
        .join(", ");

    WalkFuncDefParamsResult {
        c_code,
        params: parsed_params,
    }
}
