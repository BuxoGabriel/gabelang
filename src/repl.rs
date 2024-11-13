use std::io::{self, BufRead, Write};
use crate::ast::Node;
use crate::parser::Parser;

pub fn start() {
    print!(">>");
    io::stdout().flush().unwrap();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let program_ast = Parser::new(&line.unwrap()).parse_program();
        let program = match program_ast {
            Ok(program) => program,
            Err(err_msg) => {
                println!("{err_msg}");
                continue;
            }
        };
        match program.eval() {
            Ok(object) => {
                if let Some(val) = object.to_string() {
                    println!("{val}");
                }
            },
            Err(err) => println!("{err}")
        };
    }
}
