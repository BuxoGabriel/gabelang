use crate::{ast, lexer::{Lexer, Token, TOKENTYPE}};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>
}

impl<'a> Parser<'a> {
    pub fn new(contents: &'a str) -> Parser<'a> {
        let lexer = Lexer::new(contents);
        Self {
            lexer,
            current_token: None,
            peek_token: None
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.get_next_token();
    }

    pub fn parse_ast(&mut self) -> Result<ast::Program, String> {
        self.next_token();
        self.next_token();
        let mut statements: Vec<Box<dyn ast::Statement>> = Vec::new();
        while let Some(token) = self.current_token.as_ref() {
            if token.token_type == TOKENTYPE::LET {
                statements.push(Box::new(self.parse_let_statement()?));
            }
            self.next_token();
        }
        Ok(ast::Program {
            statements
        })
    }

    // Prerequisite: caller must check that current token is a let token
    fn parse_let_statement(&mut self) -> Result<ast::LetStatement, String> {
        // Assert that the current token is a let token
        // Todo: compiler marcro to remove asserts when building for production
        assert_eq!(self.current_token.is_none(), false);
        let let_token: Token = self.current_token.take().unwrap();
        assert_eq!(let_token.token_type, TOKENTYPE::LET);
        // If no token after let keyword or token after let keyword is not an identifier it is an invalid let statement
        if self.peek_token.is_none() {
            return Err(String::from("Let statement must have an identifier after keywork let"));
        }
        if self.peek_token.as_ref().unwrap().token_type != TOKENTYPE::IDENTIFIER {
            return Err(String::from("Let statement must have an identifier after keyword let"));
        }
        self.next_token();
        let identifier_token = self.current_token.take().unwrap();
        let identifier = ast::Identifier {
            name: identifier_token.literal.clone(),
            token: identifier_token
        };
        // If no token after identifier or token after identifier is not an equal sign it is an invalid let statement
        if self.peek_token.is_none() {
            return Err(String::from("Let statement must have a \"=\" after variable name"));
        }
        if self.peek_token.as_ref().unwrap().token_type != TOKENTYPE::EQUAL {
            return Err(String::from("Let statement must have a \"=\" after variable name"));
        }
        self.next_token();
        self.next_token();
        let expression = self.parse_expression()?;
        Ok(ast::LetStatement {
            identifier,
            token: let_token,
            expression
        })
    }

    fn parse_expression(&mut self) -> Result<Box<dyn ast::Expression>, String> {
        // invalid expression if it is blank
        if self.current_token.is_none() {
            return Err(String::from("Expression must start with a literal or variable"));
        }
        match self.current_token.as_ref().unwrap().token_type {
            TOKENTYPE::NUMBER => {
                Ok(self.parse_number()?)
            }
            _ => Err(String::from("Invalid expression value"))
        }
    }

    // Precondition: Caller must check that current_token is a number
    fn parse_number(&mut self) -> Result<Box<ast::Number>, String> {
        // Assert that the current token is a number
        assert_eq!(self.current_token.is_none(), false);
        let number_token = self.current_token.take().unwrap();
        assert_eq!(number_token.token_type, TOKENTYPE::NUMBER);
        // Create and return number Expression
        let value = match number_token.literal.parse() {
            Ok(num) => num,
            Err(_) => {
                return Err(String::from("Parse number encountered error parsing literal as number"));
            }
        };
        Ok(Box::from(ast::Number {
            token: number_token,
            value
        }))
    }
}
