use std::io::{self, BufRead, Write};
use crate::evaluator::GabrEnv;
use crate::parser::Parser;

pub fn start() {
    print!(">>");
    io::stdout().flush().unwrap();
    let stdin = io::stdin();
    let mut env = GabrEnv::new();
    for line in stdin.lock().lines() {
        let program_ast = Parser::new(&line.unwrap()).parse_program();
        let program = match program_ast {
            Ok(program) => program,
            Err(err_msg) => {
                println!("{err_msg}");
                continue;
            }
        };
        if program.len() == 1 {
            match env.eval_statement(&program[0]) {
                Ok(object) => {
                    if object.is_some() {
                        println!("{object}");
                    }
                },
                Err(err) => println!("{err}")
            };
        }
        print!(">>");
        io::stdout().flush().unwrap();
    }
}
