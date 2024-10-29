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

    pub fn parse_program(&mut self) -> Result<ast::Program, String> {
        self.next_token();
        self.next_token();
        let mut statements: Vec<Box<dyn ast::Statement>> = Vec::new();
        while let Some(token) = self.current_token.as_ref() {
            if token.token_type == TOKENTYPE::LET {
                statements.push(Box::new(self.parse_let_statement()?));
            }
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
        // Advance to identifier
        self.next_token();
        // If no token after let keyword or token after let keyword is not an identifier it is an invalid let statement
        if self.current_token.is_none() {
            return Err(String::from("Let statement must have an identifier after keywork let"));
        }
        if self.current_token.as_ref().unwrap().token_type != TOKENTYPE::IDENTIFIER {
            return Err(String::from("Let statement must have an identifier after keyword let"));
        }
        let identifier_token = self.current_token.take().unwrap();
        let identifier = ast::Identifier {
            name: identifier_token.literal.clone(),
            token: identifier_token
        };
        // Advance to = sign
        self.next_token();
        // If no token after identifier or token after identifier is not an equal sign it is an invalid let statement
        if self.current_token.is_none() {
            return Err(String::from("Let statement must have a \"=\" after variable name"));
        }
        if self.current_token.as_ref().unwrap().token_type != TOKENTYPE::EQUAL {
            return Err(String::from("Let statement must have a \"=\" after variable name"));
        }
        // Advance to expression
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
                let peek_token = self.peek_token.as_ref();
                if peek_token.is_none() {
                    return Err(String::from("Invalid expression, did you forget a semicolon?"));
                }
                match peek_token.unwrap().token_type {
                    TOKENTYPE::PLUS |
                    TOKENTYPE::MINUS |
                    TOKENTYPE::ASTERISK |
                    TOKENTYPE::SLASH => Ok(self.parse_operation()?),
                    TOKENTYPE::SEMICOLON => Ok(self.parse_number()?),
                    _ => Err(String::from("Invalid expression value"))
                }
            }
            _ => Err(String::from("Invalid expression value"))
        }
    }

    // Precondition: Caller must check that current_token is a number
    // Additionally, the parsed number should never be the last token of the program
    fn parse_number(&mut self) -> Result<Box<ast::Number>, String> {
        // Assert that the current token is a number
        assert_eq!(self.current_token.is_none(), false);
        let number_token = self.current_token.take().unwrap();
        assert_eq!(number_token.token_type, TOKENTYPE::NUMBER);
        // Advance parser to next token
        self.next_token();
        if self.current_token.as_ref().unwrap().token_type == TOKENTYPE::SEMICOLON {
            self.next_token();
        }
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

    // Precondition: Caller must check that current token is a number
    // Additionally, the number must be followed by a valid operator to call this function
    fn parse_operation(&mut self) -> Result<Box<dyn ast::Expression>, String> {
        let op1 = self.parse_number();
        let operand_token = self.current_token.take().unwrap();
        self.next_token();
        let op2 = self.parse_expression();
        Ok(Box::from(ast::ArithmaticExpression {
            token: operand_token,
            op1: op1?,
            op2: op2?
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_let_statement() {
        let let_statement = String::from("let var_x = 20; let var_y = 30;");
        let mut parser = Parser::new(&let_statement);
        let program = parser.parse_program();
        assert_eq!(program.is_ok(), true);
        let program: ast::Program = program.unwrap();
        println!("{:?}", program);
        assert_eq!(program.statements.len(), 2);
        assert_eq!(program.statements[0].token_literal(), "let");
        assert_eq!(program.statements[1].token_literal(), "let");
    }
}
