mod ast_builder;

mod ast_nodes;
mod codegen;
mod parser;

use ast_builder::build_ast_from_pairs;
use clap::Parser as ClapParser;
use codegen::gen_code;
use colored::Colorize;
use parser::{FusionParser, Rule};
use pest::Parser;
use sh::sh;
use std::fs;
use std::path::Path;
use std::process::Command;

/// Simple program to parse a function definition
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long)]
    input: String,
    #[arg(long)]
    output: String,
}

fn main() {
    let args = Args::parse();

    let file_name = &args.input;

    let file_path = Path::new(file_name.as_str());

    let file_content = fs::read_to_string(file_path)
        .expect("Failed to read source file.")
        .to_string();
    let mut rules = FusionParser::parse(Rule::program, &file_content).unwrap();

    let pair = rules.next().unwrap();

    println!("{}{}", "Pairs: \n".yellow(), pair);

    let ast = match pair.as_rule() {
        Rule::program => build_ast_from_pairs(pair),
        _ => panic!(
            "Top Level Node can only be a program. Not a {}",
            pair.as_str()
        ),
    };

    println!("{}{}", "AST:\n".purple(), ast);

    let code = gen_code(ast);

    println!(
        "\n\n{}\n--------------------\n{}\n--------------------\n\n",
        "Generated C-Code:".green(),
        code
    );

    let formatter_output = if std::env::consts::OS == "windows" {
        Command::new("powershell")
            .arg("-c")
            .arg(format!("echo '{}' | clang-format", code))
            .output()
            .unwrap()
    } else {
        Command::new("sh")
            .arg("-c")
            .arg(format!("echo '{}' | clang-format", code))
            .output()
            .unwrap()
    };

    let formatted_code = String::from_utf8(formatter_output.stdout).unwrap();

    println!(
        "\n\n{}\n--------------------\n{}\n--------------------\n\n",
        "Formatted C-Code:".on_green().black(),
        formatted_code
    );

    fs::write("output.c", formatted_code).unwrap();

    sh!(gcc "output.c" "-o" {args.output});
    //sh!(rm "output.c");
}
