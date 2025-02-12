use std::io::{self, BufRead, Write};
use crate::evaluator::GabrEnv;
use crate::parser::Parser;

/// Starts the gabelang repl in an isolated environment
///
/// The repl runs until forcefully terminated and prints to stdout and takes input from stdin.
pub fn start() {
    println!(r"WELCOME TO
   _____       _          _                   
  / ____|     | |        | |                  
 | |  __  __ _| |__   ___| | __ _ _ __   __ _ 
 | | |_ |/ _` | '_ \ / _ \ |/ _` | '_ \ / _` |
 | |__| | (_| | |_) |  __/ | (_| | | | | (_| |
  \_____|\__,_|_.__/ \___|_|\__,_|_| |_|\__, |
                                         __/ |
                                        |___/ 
GABELANG IS A HIGH LEVEL, INTERPRETTED, GARBAGE COLLECTED, PROGRAMMING LANGUAGE THAT SUPPORTS
ARRAYS,OBJECTS, AND FIRST CLASS AND HIGHER ORDER FUNCTIONS

TRY DECLARING VARIABLES LIKE THIS
>> let i = {{ a: [5], b: 3 * 3 }};

TRY DECLARING FUNCTIONS LIKE THIS
>> fn double_and_abs(num) {{if num > 0 {{return num * 2;}} else {{return num * -2;}}}}

TYPING AN EXPRESSION INSTEAD OF A STATEMENT WILL PRINT THE EVALUATION OF THE STATEMENT
>> double_and_abs(i.b)
18");

    print!(">> ");
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
        print!(">> ");
        io::stdout().flush().unwrap();
    }
}
