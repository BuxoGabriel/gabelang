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
            TOKENTYPE::RETURN => Ok(self.parse_return_statement()?),
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

    // Precondition: Caller must only call this if the current token is an if token
    fn parse_return_statement(&mut self) -> Result<Box<ast::ReturnStatement>, String> {
        assert!(self.current_token.is_some());
        assert_eq!(self.current_token.as_ref().unwrap().token_type, TOKENTYPE::RETURN);
        let token = self.current_token.take().unwrap();
        self.next_token();
        if self.current_token.is_none() {
            return Err(String::from("Invalid return statement: return can not be the last token, did you forget a \";\""));
        }
        let return_value = match self.current_token.as_ref().unwrap().token_type {
            TOKENTYPE::SEMICOLON => {
                self.next_token();
                None
            }
            _ => Some(self.parse_expression()?)
        };
        Ok(Box::from(ast::ReturnStatement {
            token,
            return_value
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
    use std::any::Any;

    use super::*;
    use ast::Node;

    fn expect_coerce<T: 'static>(statement: &dyn Any) -> &T {
        statement.downcast_ref::<T>().expect("Failed to downcast &dyn Node to concrete type!")
    }

    fn test_let(let_statement: &ast::LetStatement, name: String) {
        assert_eq!(let_statement.identifier.name, name);
        assert_eq!(let_statement.identifier.token_literal(), name);
    }

    fn test_number(number_literal: &ast::Number, value: i64) {
        assert_eq!(number_literal.value, value);
        assert_eq!(number_literal.token_literal(), value.to_string());
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
        let let_1 = expect_coerce::<ast::LetStatement>(statement_1.as_any());
        test_let(let_1, String::from("var_x"));
        let number = expect_coerce::<ast::Number>(let_1.expression.as_any());
        test_number(number, 20);
        // Test Statement 2
        let statement_2 = statements.next().unwrap();
        assert_eq!(statement_2.token_literal(), "let");
        let let_2 = expect_coerce::<ast::LetStatement>(statement_2.as_any());
        test_let(let_2, String::from("var_y"));
        let number = expect_coerce::<ast::Number>(let_2.expression.as_any());
        test_number(number, 30);
        // Test Statement 3
        let statement_3 = statements.next().unwrap();
        assert_eq!(statement_3.token_literal(), "let");
        let let_3 = expect_coerce::<ast::LetStatement>(statement_3.as_any());
        test_let(let_3, String::from("var_z"));
        let number = expect_coerce::<ast::Number>(let_3.expression.as_any());
        test_number(number, 1);
    }

    #[test]
    fn test_if_statement() {
        let input = String::from("if i + 2 { let b = 20; }");
        let mut parser = Parser::new(&input);
        let program = parser.parse_program().expect("Failed to parse Program");
        let mut statements = program.statements.into_iter();
        let statement = statements.next().expect("First statement in program is None");
        assert_eq!(statement.token_literal(), "if");
        let if_statement = expect_coerce::<ast::IfStatement>(statement.as_any());
        let condition = &if_statement.condition;
        assert_eq!(condition.token_literal(), "+");
        let operation = expect_coerce::<ast::ArithmaticExpression>(condition.as_any());
        assert_eq!(operation.op1.token_literal(), "i");
        assert_eq!(operation.op2.token_literal(), "2");
        let then = &if_statement.then;
        assert_eq!(then.token_literal(), "{}");
    }

    #[test]
    fn test_return_statement() {
        let input = String::from("return 20; return (2 * 3) + a;");
        let mut parser = Parser::new(&input);
        let program = parser.parse_program().expect("Failed to parse Program;");
        // Test statements has right length
        let mut statements = program.statements.into_iter();
        assert_eq!(statements.len(), 2);
        // Test that first return is correct
        let statement_1 = statements.next().expect("First statement in program is None");
        assert_eq!(statement_1.token_literal(), "return");
        let return_1 = expect_coerce::<ast::ReturnStatement>(statement_1.as_any());
        assert_eq!(return_1.return_value.as_ref().expect("First return value is None").token_literal(), "20");
        // Test that second return is correct
        let statement_2 = statements.next().expect("Second statement in program is None");
        assert_eq!(statement_2.token_literal(), "return");
        let return_2 = expect_coerce::<ast::ReturnStatement>(statement_2.as_any());
        let return_2_value = return_2.return_value.as_ref().expect("Second return value is None");
        assert_eq!(return_2_value.token_literal(), "+");
    }

    #[test]
    fn test_return_none() {
        let input = String::from("return;");
        let mut parser = Parser::new(&input);
        let statements = parser.parse_program().expect("Failed to parse program").statements;
        assert_eq!(statements.len(), 1);
        let return_statement = statements.into_iter().next().expect("First statement in program is None");
        assert_eq!(return_statement.token_literal(), "return");
        let return_statement = expect_coerce::<ast::ReturnStatement>(return_statement.as_any());
        assert!(return_statement.return_value.is_none());
    }

    #[test]
    fn test_program() {
        let input = String::from("
let a = 20;
if a + 2 {
    let b = 20;
    return b;
}
let d = (1 * 2) + a;");
        let mut parser = Parser::new(&input);
        let program = parser.parse_program();
        assert!(program.is_ok());
        let statements = program.unwrap().statements;
        assert_eq!(statements.len(), 3);
        let mut statements = statements.into_iter();
        // Testing First statement is let statement
        let statement_1 = statements.next().expect("First statement is None");
        assert_eq!(statement_1.token_literal(), "let");
        let let_statement = expect_coerce::<ast::LetStatement>(statement_1.as_any());
        test_let(let_statement, String::from("a"));
        assert_eq!(let_statement.expression.token_literal(), "20");
        // Testing second statement is if statement
        let statement_2 = statements.next().expect("Second statement is None");
        assert_eq!(statement_2.token_literal(), "if");
        let if_statement = expect_coerce::<ast::IfStatement>(statement_2.as_any());
        let condition = &if_statement.condition;
        assert_eq!(condition.token_literal(), "+");
        let operation = expect_coerce::<ast::ArithmaticExpression>(condition.as_any());
        assert_eq!(operation.op1.token_literal(), "a");
        assert_eq!(operation.op2.token_literal(), "2");
        assert_eq!(if_statement.then.token_literal(), "{}");
        let then_code_block = expect_coerce::<ast::CodeBlock>(&if_statement.then);
        assert_eq!(then_code_block.statements.len(), 2);
        // Testing interior of if statement
        // Testing interior statement 1
        let mut then_statements = then_code_block.statements.iter();
        let statement_1 = then_statements.next().expect("First statement in if block is None");
        assert_eq!(statement_1.token_literal(), "let");
        let statement_1 = expect_coerce::<ast::LetStatement>(statement_1.as_any());
        test_let(statement_1, String::from("b"));
        assert_eq!(statement_1.expression.token_literal(), "20");
        // Testing interior statement 2
        let statement_2 = then_statements.next().expect("Second statement in if block is None");
        assert_eq!(statement_2.token_literal(), "return");
        let statement_2 = expect_coerce::<ast::ReturnStatement>(statement_2.as_any());
        assert!(statement_2.return_value.is_some());
        assert_eq!(statement_2.return_value.as_ref().unwrap().token_literal(), "b");
        // Testing statement 3 complex math let statement
        let statement_3 = statements.next().expect("Third statement is None");
        assert_eq!(statement_3.token_literal(), "let");
        let let_statement = expect_coerce::<ast::LetStatement>(statement_3.as_any());
        test_let(let_statement, String::from("d"));
        let let_expression = let_statement.expression.as_ref();
        assert_eq!(let_expression.token_literal(), "+");
        let addition_expression = expect_coerce::<ast::ArithmaticExpression>(let_expression.as_any());
        let lhs = &addition_expression.op1;
        assert_eq!(lhs.token_literal(), "()");
        let rhs = &addition_expression.op2;
        assert_eq!(rhs.token_literal(), "a");
        let group_expression = expect_coerce::<ast::GroupExpression>(lhs.as_any());
        assert_eq!(group_expression.expression.token_literal(), "*");
        let multiplication_expression = expect_coerce::<ast::ArithmaticExpression>(group_expression.expression.as_any());
        assert_eq!(multiplication_expression.op1.token_literal(), "1");
        assert_eq!(multiplication_expression.op2.token_literal(), "2");
    }
}
