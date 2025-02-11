use std::{collections::HashMap, fs, io};

mod ast;
pub mod repl;
pub mod lexer;
pub mod parser;
pub mod evaluator;

use evaluator::GabrEnv;
use parser::Parser;

#[derive(Default)]
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

/// Runs the gabelang interpretter by taking in a config and either running the repl or interpretting a .gabe file based on args
/// 
/// Config can be generated using gabelang::Config::build
/// 
/// # Errors
///
/// run can only generate errors if it is interpretting a file and either the file fails
///
/// # Example
///
/// ```
/// use gabelang::Config;
/// gabelang::run(Config::default()).expect("Application ran into an unexpected error");
/// ```

pub fn run(config: Config) -> io::Result<()> {
    if let Some(file_name) = config.get_flag("file") {
        let contents = fs::read_to_string(&file_name)?;
        println!("parsing {}", &file_name);
        let program = Parser::new(&contents).parse_program();
        match program {
            Ok(program) => {
                println!("parsing complete! Running {}", &file_name);
                let mut env = GabrEnv::new();
                match env.eval_program(&program) {
                    Ok(res) => println!("{res}"),
                    Err(err) => println!("Execution Failed: {err}")
                }
            },
            Err(e) => println!("{e}"),
        };
        return Ok(())
    }
    repl::start();
    Ok(())
}
