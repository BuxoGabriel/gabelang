#![doc(html_favicon_url = "https://buxogabriel.vercel.app/favicon.ico")]
#![doc(html_logo_url = "https://buxogabriel.vercel.app/apple-touch-icon.png")]

use std::{collections::HashMap, fs, io};

mod ast;
/// The repl module contains a start function run an isolated instance of the gabelang repl
pub mod repl;
/// The lexer module contains the Lexer struct which creates an iterator of tokens that may be used
/// by the gabelang parser when given a string 
pub mod lexer;
/// The parser module contains the Parser struct which contains a lexer and creates an AST(Abstract Syntax Tree) when given a string
pub mod parser;
/// The evaluator module contains the GabrEnv struct which provides an environment in order to run gabelang code
pub mod evaluator;

use evaluator::GabrEnv;
use parser::Parser;

/// Config for running the gabelang interpretter
#[derive(Default)]
pub struct Config {
    flags: HashMap<String, String>
}

impl Config {
    /// Constructs a Config from a list of args
    ///
    /// Expects args to come in flag, value pairs
    ///
    /// Currently the only supported flag is file, which expects a filename to interpret
    ///
    /// No flags/empty arguments implys default behavior which is running the repl
    ///
    /// # Examples
    ///
    /// Getting args from the command line to generate config
    /// ```
    /// use std::env;
    /// use gabelang::Config;
    /// let args: Vec<String> = env::args().collect();
    /// let config = Config::build(&args);
    /// ```
    ///
    /// Manually setting a file argument
    /// ```
    /// use gabelang::Config;
    /// let config = Config::build(&[String::from("--file"), String::from("index.gabe")]);
    /// ```
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
