use std::{collections::HashMap, error::Error, fs};

mod repl;
mod lexer;
mod ast;
mod parser;
mod evaluator;

use evaluator::GabrEnv;
use parser::Parser;

pub struct Config {
    flags: HashMap<String, String>
}

impl Config {
    pub fn build(args: &[String]) -> Result<Config, &'static str> {
        let flags = Self::parse_flags(args);
        Ok(Config { flags })
    }

    fn parse_flags(args: &[String]) -> HashMap<String, String> {
        let mut map = HashMap::<String, String>::new();
        args.windows(2).for_each(|pair| {
            match pair[0].clone().split_once("--") {
                Some((_, flag)) => {
                    map.insert(flag.to_string(), pair[1].clone());
                },
                None => {},
            }
        });
        map
    }

    fn get_flag(&self, flag: &str) -> Option<String> {
        self.flags.get(flag).map(|f| f.clone())
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if let Some(file_name) = config.get_flag("file") {
        let contents = fs::read_to_string(&file_name)?;
        println!("parsing {}", &file_name);
        let program = Parser::new(&contents).parse_program();
        match program {
            Ok(program) => {
                println!("parsing complete! Running {}", &file_name);
                let mut env = GabrEnv::new();
                let res = env.eval_program(&program)?;
                println!("{res}");
            },
            Err(e) => println!("{e}"),
        };
        return Ok(())
    }
    repl::start();
    Ok(())
}
