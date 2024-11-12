use std::io::{self, BufRead, Write};
use crate::parser::Parser;

pub fn start() {
    print!(">>");
    io::stdout().flush().unwrap();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let program_ast = Parser::new(&line.unwrap()).parse_program();
        match program_ast {
            Ok(program) => {
                for line in program.statements.iter() {
                    println!("{}", line.to_string());
                }
            },
            Err(err_msg) => {
                println!("{err_msg}");
            }
        }
    }
}
