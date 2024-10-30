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
        while self.current_token.as_ref().is_some() {
            statements.push(self.parse_statement()?);
        }
        Ok(ast::Program {
            statements
        })
    }

    // Precondition: Caller must only call this if the current token is an open squig
    fn parse_block(&mut self) -> Result<ast::CodeBlock, String> {
        // Assert that the current token is an if token
        // Todo: compiler macro to remove asserts when building for production
        assert_eq!(self.current_token.is_none(), false);
        let open_token: Token = self.current_token.take().unwrap();
        assert_eq!(open_token.token_type, TOKENTYPE::LSQUIG);
        // Advance to statements
        self.next_token();
        let mut statements = Vec::new();
        while self.current_token.is_some() {
            if self.current_token.as_ref().unwrap().token_type == TOKENTYPE::RSQUIG {
                // If you reach the close token then the block is done being read
                let close_token: Token = self.current_token.take().unwrap();
                self.next_token();
                let code_block = ast::CodeBlock {
                    open_token,
                    close_token,
                    statements
                };
                return Ok(code_block);
            } else {
                statements.push(self.parse_statement()?);
            }
        }
        Err(String::from("Code block was not closed, did you forget a \"}\""))
    }


    fn parse_statement(&mut self) -> Result<Box<dyn ast::Statement>, String> {
        match self.current_token.as_ref().unwrap().token_type {
            TOKENTYPE::LET => Ok(self.parse_let_statement()?),
            TOKENTYPE::IF => Ok(self.parse_if_statement()?),
            _ => Err(String::from("Invalid statement"))
        }
    }

    // Precondition: caller must check that current token is a let token
    fn parse_let_statement(&mut self) -> Result<Box<ast::LetStatement>, String> {
        // Assert that the current token is a let token
        // Todo: compiler macro to remove asserts when building for production
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
        Ok(Box::from(ast::LetStatement {
            identifier,
            token: let_token,
            expression
        }))
    }

    // Precondition: Caller must only call this if the current token is an if token
    fn parse_if_statement(&mut self) -> Result<Box<ast::IfStatement>, String> {
        // Assert that the current token is an if token
        // Todo: compiler macro to remove asserts when building for production
        assert_eq!(self.current_token.is_none(), false);
        let if_token: Token = self.current_token.take().unwrap();
        assert_eq!(if_token.token_type, TOKENTYPE::IF);
        // Advance to expression to validate
        self.next_token();
        let condition = self.parse_expression()?;
        let then = self.parse_block()?;
        Ok(Box::from(ast::IfStatement {
            token: if_token,
            condition,
            then
        }))
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
                    TOKENTYPE::SLASH => Ok(self.parse_operation(None)?),
                    TOKENTYPE::SEMICOLON |
                    TOKENTYPE::RPAREN |
                    TOKENTYPE:: LSQUIG => Ok(self.parse_number()?),
                    _ => Err(String::from("Invalid expression value"))
                }
            },
            TOKENTYPE::LPAREN => Ok(self.parse_group_expression()?),
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
        if self.current_token.is_some() && self.current_token.as_ref().unwrap().token_type == TOKENTYPE::SEMICOLON {
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

    // Precondition: If starting on a number, must pass None as an argument
    // Otherwise, pass in Ok(op1) and will expect to start on operand
    fn parse_operation(&mut self, first_operand:Option<Box<dyn ast::Expression>>) -> Result<Box<ast::ArithmaticExpression>, String> {
        let op1 = match first_operand {
            Some(op1) => op1,
            None => self.parse_number()?
        };
        let operand_token = self.current_token.take().unwrap();
        self.next_token();
        let op2 = self.parse_expression()?;
        Ok(Box::from(ast::ArithmaticExpression {
            token: operand_token,
            op1,
            op2
        }))
    }

    // Precondition: Caller must only call this if the current token is an open parenthesis
    fn parse_group_expression(&mut self) -> Result<Box<dyn ast::Expression>, String> {
        // Assert that this method was called while the current token is on open parenthesis
        assert_eq!(self.current_token.is_some(), true);
        let open_token = self.current_token.take().unwrap();
        assert_eq!(open_token.token_type, TOKENTYPE::LPAREN);
        // Advance parser to interior expression
        self.next_token();
        let expression = self.parse_expression()?;
        // Check that the opened group is closed
        if self.current_token.is_none() {
            return Err(String::from("Group expression was not closed, did you forget a \")\""));
        }
        let close_token = self.current_token.take().unwrap();
        if close_token.token_type != TOKENTYPE::RPAREN {
            return Err(String::from("Group expression was not closed, did you forget a \")\""));
        }
        // Contruct Group expression
        let mut expression: Box<dyn ast::Expression> = Box::from(ast::GroupExpression{
            open_token,
            close_token,
            expression
        });

        // Move past close expression
        self.next_token();
        if self.current_token.is_none() {
            return Err(String::from("Group expression closing can not be last token, did you forget a \";\""));
        }
        let next_token_type = self.current_token.as_ref().unwrap().token_type.clone();
        if next_token_type == TOKENTYPE::SEMICOLON {
            self.next_token();
        }

        // If next token is arithmatic then pass in the group expression as the lhs
        if Parser::token_type_is_arithmatic(next_token_type) {
            expression = self.parse_operation(Some(expression))?;
        }
        Ok(expression)
    }

    fn token_type_is_arithmatic(token: TOKENTYPE) -> bool {
        token == TOKENTYPE::PLUS ||
        token == TOKENTYPE::MINUS ||
        token == TOKENTYPE::ASTERISK ||
        token == TOKENTYPE::SLASH
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_let_statement() {
        let let_statement = String::from("let var_x = 20; let var_y = 30; let var_z = 1;");
        let mut parser = Parser::new(&let_statement);
        let program = parser.parse_program();
        assert_eq!(program.is_ok(), true);
        let program: ast::Program = program.unwrap();
        println!("{:?}", program);
        assert_eq!(program.statements.len(), 3);
        assert_eq!(program.statements[0].token_literal(), "let");
        assert_eq!(program.statements[1].token_literal(), "let");
        assert_eq!(program.statements[2].token_literal(), "let");
    }
}
