use crate::ast_nodes::func_def::FuncDefNode;
use std::collections::HashMap;

pub struct RegularFunctionDeclaration {
    // The name given by the compiler (includes prefix to prevent name collisions)
    pub compiler_name: String,
    // The compiled code
    pub c_code: String,
}

pub struct GenericFunctionDeclaration {
    pub ast_node: FuncDefNode,
    pub generic_params: Vec<String>,
}

//TODO: Parse parameters and return type for parsed function to check types before passing the
//code to gcc.
pub struct CImportedFunctionDeclaration {
    pub name: String,
}

pub enum FunctionDeclaration {
    Generic(GenericFunctionDeclaration),
    Regular(RegularFunctionDeclaration),
    CImported(CImportedFunctionDeclaration),
}

pub struct StructDefinition {
    pub name: String,
    pub c_code: String,
    pub fields: Vec<String>,
}

pub struct Context {
    /// Stores every function thats declared by the program. This also includes functions imported
    /// from C. The key is the name defined by the program, while the value includes the name
    /// given to the function by the compiler
    pub function_declarations: HashMap<String, FunctionDeclaration>,
    pub types: Vec<String>,
    pub scope_stack: Vec<String>,
    pub main_function_content: String,
    pub struct_definitions: Vec<StructDefinition>,
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
            generic_function_implementations: HashMap::new(),
            main_function_content: String::from(""),
            imports: vec![],
            types: vec![],
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
            generic_function_implementations: HashMap::new(),
            main_function_content: String::from(""),
            imports: vec![],
            types: vec![],
            compiler_prefix: "".to_string(),
            modules: HashMap::new(),
            global_variables: vec![],
        }
    }
}
