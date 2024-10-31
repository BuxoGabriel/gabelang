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
        // If no token after identifier or token after identifier is not an equal sign it is an invalid let statement
        if 
        self.peek_token.is_none() ||
        self.peek_token.as_ref().unwrap().token_type != TOKENTYPE::EQUAL {
            return Err(String::from("Let statement must have a \"=\" after variable name"));
        }
        // Now that we know that parse identifier will not panic, i.e., it is followed by a token, we get the identifier token
        let identifier = self.parse_identifier();
        // Advance to expression / skip equal sign
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
        let current_token_type = self.current_token.as_ref().unwrap().token_type.clone();
        if 
        current_token_type == TOKENTYPE::NUMBER ||
        current_token_type == TOKENTYPE::IDENTIFIER {
            // Make sure that there is a token after the number/identifier so they do not panic
            let peek_token = self.peek_token.as_ref();
            if peek_token.is_none() {
                return Err(String::from("Invalid expression, did you forget a semicolon?"));
            }
            let peek_token_type = peek_token.unwrap().token_type.clone();
            // Parse number/identifier expression
            let expression: Box<dyn ast::Expression> = if current_token_type == TOKENTYPE::NUMBER {
                self.parse_number()?
            } else {
                Box::from(self.parse_identifier())
            };
            match peek_token_type {
                // If next arg is arithmatic then parse operation with this number/indentifier
                TOKENTYPE::PLUS |
                TOKENTYPE::MINUS |
                TOKENTYPE::ASTERISK |
                TOKENTYPE::SLASH => Ok(self.parse_operation(expression)?),
                // These tokens signify end of expression so return just the number/identifier
                TOKENTYPE::SEMICOLON |
                TOKENTYPE::RPAREN |
                TOKENTYPE:: LSQUIG => Ok(expression),
                // Unexpected token after number or identifier indicates invalid expression
                _ => Err(String::from("Invalid expression value, unexpected token recieved"))
            }
        } else if current_token_type == TOKENTYPE::LPAREN {
            Ok(self.parse_group_expression()?)
        } else {
            Err(String::from("Invalid expression value"))
        }
    }

    // Precondition: Caller must check that current_token is an identifier
    // Additionally, the identifier should never be the last token of the program
    fn parse_identifier(&mut self) -> ast::Identifier {
        // Assert that the current token is a number
        assert_eq!(self.current_token.is_none(), false);
        let identifier_token = self.current_token.take().unwrap();
        assert_eq!(identifier_token.token_type, TOKENTYPE::IDENTIFIER);
        // Advance parser to next token
        self.next_token();
        assert!(self.current_token.is_some());
        if self.current_token.as_ref().unwrap().token_type == TOKENTYPE::SEMICOLON {
            self.next_token();
        }
        ast::Identifier {
            name: identifier_token.literal.clone(),
            token: identifier_token
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
        let value = match number_token.literal.parse::<i64>() {
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
    fn parse_operation(&mut self, op1:Box<dyn ast::Expression>) -> Result<Box<ast::ArithmaticExpression>, String> {
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
            expression = self.parse_operation(expression)?;
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
    use ast::Node;

    fn expect_coerce_to_let(statement: &dyn ast::Statement) -> &ast::LetStatement {
        let statement = statement.as_any();
        statement.downcast_ref::<ast::LetStatement>().expect("Failed to downcast statement to let statement!")
    }

    fn test_let(let_statement: &ast::LetStatement, name: String) {
        assert_eq!(let_statement.identifier.name, name);
        assert_eq!(let_statement.identifier.token_literal(), name);
    }

    fn expect_coerce_to_number(expression: &dyn ast::Expression) -> &ast::Number {
        let expression = expression.as_any();
        expression.downcast_ref::<ast::Number>().expect("Failed to downcast expression to number literal!")
    }

    fn test_number(number_literal: &ast::Number, value: i64) {
        assert_eq!(number_literal.value, value);
        assert_eq!(number_literal.token_literal(), value.to_string());
    }

    fn expect_coerce_to_if(statement: &dyn ast::Statement) -> &ast::IfStatement {
        let statement = statement.as_any();
        statement.downcast_ref::<ast::IfStatement>().expect("Failed to downcast statement to if statement")
    }

    #[test]
    fn test_let_statement() {
        let let_statement = String::from("let var_x = 20; let var_y = 30; let var_z = 1;");
        let mut parser = Parser::new(&let_statement);
        let program = parser.parse_program();
        assert_eq!(program.is_ok(), true);
        let program: ast::Program = program.unwrap();
        assert_eq!(program.statements.len(), 3);
        let mut statements = program.statements.into_iter();
        // Test Statement 1
        let statement_1 = statements.next().unwrap();
        assert_eq!(statement_1.token_literal(), "let");
        let let_1 = expect_coerce_to_let(statement_1.as_ref());
        test_let(let_1, String::from("var_x"));
        let number = expect_coerce_to_number(let_1.expression.as_ref());
        test_number(number, 20);
        // Test Statement 2
        let statement_2 = statements.next().unwrap();
        assert_eq!(statement_2.token_literal(), "let");
        let let_2 = expect_coerce_to_let(statement_2.as_ref());
        test_let(let_2, String::from("var_y"));
        let number = expect_coerce_to_number(let_2.expression.as_ref());
        test_number(number, 30);
        // Test Statement 3
        let statement_3 = statements.next().unwrap();
        assert_eq!(statement_3.token_literal(), "let");
        let let_3 = expect_coerce_to_let(statement_3.as_ref());
        test_let(let_3, String::from("var_z"));
        let number = expect_coerce_to_number(let_3.expression.as_ref());
        test_number(number, 1);
    }

    #[test]
    fn test_if_statement() {
        let input = String::from("if i + 2 { let b = 20; }");
        let mut parser = Parser::new(&input);
        let program = parser.parse_program();
        assert!(program.is_ok());
        let mut statements = program.unwrap().statements.into_iter();
        let statement = statements.next().expect("First statement in program is None");
        assert_eq!(statement.token_literal(), "if");
        let if_statement = expect_coerce_to_if(statement.as_ref());
        let condition = &if_statement.condition;
        assert_eq!(condition.token_literal(), "+");
        let then = &if_statement.then;
        assert_eq!(then.token_literal(), "{}");
    }
}
