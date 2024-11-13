use std::{fs, error::Error};

pub mod repl;
mod lexer;
mod ast;
mod parser;
mod evaluator;

use parser::Parser;
use ast::Node;

pub struct Config {
    file_name: String
}

impl Config {
    pub fn build(args: &[String]) -> Result<Config, &'static str> {
        if args.len() != 2 {
            return Err("must supply a file as an argument");
        }
        let file_name = args[1].clone();
        Ok(Config { file_name })
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(&config.file_name)?;
    println!("lexing contents of {}", &config.file_name);
    let program = Parser::new(&contents).parse_program();
    match program {
        Ok(program) => println!("{}", program.to_string()),
        Err(e) => println!("{e}"),
    };
    Ok(())
}
