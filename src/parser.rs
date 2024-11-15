use std::rc::Rc;

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

    fn current_token_is(&self, token_type: TOKENTYPE) -> bool {
        self.current_token.is_some() && self.current_token.as_ref().unwrap().token_type == token_type
    }

    fn peek_token_is(&self, token_type: TOKENTYPE) -> bool {
        self.peek_token.is_some() && self.peek_token.as_ref().unwrap().token_type == token_type
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, String> {
        self.next_token();
        self.next_token();
        let mut statements: Vec<Rc<dyn ast::Statement>> = Vec::new();
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
            if self.current_token_is(TOKENTYPE::RSQUIG) {
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


    fn parse_statement(&mut self) -> Result<Rc<dyn ast::Statement>, String> {
        match self.current_token.as_ref().unwrap().token_type {
            TOKENTYPE::LET => Ok(self.parse_let_statement()?),
            TOKENTYPE::IDENTIFIER => {
                if self.peek_token_is(TOKENTYPE::EQUAL) {
                    Ok(self.parse_assign_statement()?)
                } else {
                    Ok(self.parse_expression_statement()?)
                }
            },
            TOKENTYPE::WHILE => Ok(self.parse_while_loop()?),
            TOKENTYPE::IF => Ok(self.parse_if_statement()?),
            TOKENTYPE::RETURN => Ok(self.parse_return_statement()?),
            TOKENTYPE::FN => Ok(self.parse_function()?),
            _ => Ok(self.parse_expression_statement()?)
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Rc<ast::ExpressionStatement>, String> {
        let expression = Box::from(self.parse_expression(0)?);
        if !self.current_token_is(TOKENTYPE::SEMICOLON) {
            return Err("Invalid Expression Statement: Expression statement should end with a semicolon".to_string())
        }
        self.next_token();
        Ok(Rc::from(ast::ExpressionStatement {
            expression
        }))
    }
    // Precondition: caller must check that current token is a let token
    fn parse_let_statement(&mut self) -> Result<Rc<ast::LetStatement>, String> {
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
        if !self.peek_token_is(TOKENTYPE::EQUAL) {
            return Err(String::from("Let statement must have a \"=\" after variable name"));
        }
        // Now that we know that parse identifier will not panic, i.e., it is followed by a token, we get the identifier token
        let identifier = self.parse_identifier();
        // Advance to expression / skip equal sign
        self.next_token();
        let expression = self.parse_expression(0)?;
        if !self.current_token_is(TOKENTYPE::SEMICOLON) {
            return Err(String::from("Let statement must end with a semicolon"));
        } else {
            self.next_token();
        }
        Ok(Rc::from(ast::LetStatement {
            identifier,
            token: let_token,
            expression
        }))
    }

    // Precondition: caller must ensure that current token is an identifier token
    fn parse_assign_statement(&mut self) -> Result<Rc<ast::AssignStatement>, String> {
        // Assert that current token is identifier
        assert!(self.current_token_is(TOKENTYPE::IDENTIFIER));
        // If no token after identifier or token after identifier is not an equal sign it is an invalid assignment statement
        if !self.peek_token_is(TOKENTYPE::EQUAL) {
            return Err(String::from("variable assignment must have a \"=\" after variable name"));
        }
        let ident = self.parse_identifier();
        // Move on to expression
        self.next_token();
        let expression = self.parse_expression(0)?;
        if !self.current_token_is(TOKENTYPE::SEMICOLON) {
            return Err(String::from("Variable assignment must end with a semicolon"));
        } else {
            self.next_token();
        }
        Ok(Rc::from(ast::AssignStatement {
            ident,
            expression
        }))
    }

    // Precondition: Caller must only call this if the current token is a while token
    fn parse_while_loop(&mut self) -> Result<Rc<ast::WhileLoop>, String> {
        // Assert that the current token is a while token
        assert!(self.current_token_is(TOKENTYPE::WHILE));
        let token: Token = self.current_token.take().unwrap();
        // Advance to expression to validate
        self.next_token();
        let condition = self.parse_expression(0)?;
        let body = self.parse_block()?;
        Ok(Rc::from(ast::WhileLoop {
            token,
            condition,
            body
        }))
    }

    // Precondition: Caller must only call this if the current token is an if token
    fn parse_if_statement(&mut self) -> Result<Rc<ast::IfStatement>, String> {
        // Assert that the current token is an if token
        assert!(self.current_token_is(TOKENTYPE::IF));
        let if_token: Token = self.current_token.take().unwrap();
        // Advance to expression to validate
        self.next_token();
        let condition = self.parse_expression(0)?;
        let then_block = self.parse_block()?;
        let else_block = if self.current_token_is(TOKENTYPE::ELSE) {
            self.next_token();
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Rc::from(ast::IfStatement {
            token: if_token,
            condition,
            then_block,
            else_block
        }))
    }

    // Precondition: Caller must only call this if the current token is a return token
    fn parse_return_statement(&mut self) -> Result<Rc<ast::ReturnStatement>, String> {
        assert!(self.current_token_is(TOKENTYPE::RETURN));
        let token = self.current_token.take().unwrap();
        self.next_token();
        if self.current_token.is_none() {
            return Err(String::from("Invalid return statement: return can not be the last token, did you forget a semicolon"));
        }
        let return_value = if self.current_token_is(TOKENTYPE::SEMICOLON) {
            None
        } else {
            Some(self.parse_expression(0)?)
        };
        if !self.current_token_is(TOKENTYPE::SEMICOLON) {
            return Err("Invalid return statement: expected semicolon".to_string()); 
        } else {
            self.next_token();
        }
        Ok(Rc::from(ast::ReturnStatement {
            token,
            return_value
        }))
    }

    // Precondition: Caller must only call this if the current token is an if token
    fn parse_function(&mut self) -> Result<Rc<ast::Function>, String> {
        // Ensure parse function was not called while token is not fn
        assert!(self.current_token.is_some());
        assert_eq!(self.current_token.as_ref().unwrap().token_type, TOKENTYPE::FN);
        // Get fn token
        let token = self.current_token.take().unwrap();
        // Move on to function Identifier
        self.next_token();
        if self.current_token.is_none() || self.current_token.as_ref().unwrap().token_type != TOKENTYPE::IDENTIFIER {
            return Err("Invalid Function: function must have an identifier before parameters".to_string());
        }
        let ident = self.parse_identifier();
        // Expect an open parenthesis before parameters after function name
        if self.current_token.is_none() || self.current_token.as_ref().unwrap().token_type != TOKENTYPE::LPAREN {
            return Err("Invalid Function: function must have an open parenthesis after identifier".to_string());
        }
        // Move on to parameters
        self.next_token();
        let mut params = Vec::new();
        while self.current_token_is(TOKENTYPE::IDENTIFIER) {
            params.push(self.parse_identifier());
            if self.current_token_is(TOKENTYPE::COMMA) {
                self.next_token()
            } else {
                break
            }
        }
        if !self.current_token_is(TOKENTYPE::RPAREN) {
            return Err(format!("Invalid function: Invalid function parameter or function closed improperly, found token: {:?}", self.current_token));
        }
        // Move on to then block
        self.next_token();
        if !self.current_token_is(TOKENTYPE::LSQUIG) {
            return Err("Invalid function: Expected code block after parameters".to_string());
        }
        let body = self.parse_block()?;
        Ok(Rc::from(ast::Function {
            token,
            ident,
            params,
            body
        }))
    }

    // Precondition: Caller must make sure that current token is an identifier and that the
    // next token is an open parenthesis
    fn parse_function_call(&mut self) -> Result<ast::FunctionCall, String> {
        assert!(self.current_token_is(TOKENTYPE::IDENTIFIER));
        // Get function Identifier
        let ident = self.parse_identifier();
        assert!(self.current_token_is(TOKENTYPE::LPAREN));
        // Move on to parameters
        self.next_token();
        let mut params: Vec<Box<dyn ast::Expression>> = Vec::new();
        while self.current_token.is_some() && !self.current_token_is(TOKENTYPE::RPAREN) {
            params.push(self.parse_expression(0)?);
            if self.current_token_is(TOKENTYPE::COMMA) {
                self.next_token()
            } else {
                break
            }
        }
        // Param list should end with a close parenthesis otherwise it is an invalid function call
        if self.current_token_is(TOKENTYPE::RPAREN) {
            self.next_token();
        } else {
            return Err(format!("Invalid Function Call: Function call must end parameter list with a close parenthesis, ends with: {:?}", self.current_token));
        }
        Ok(ast::FunctionCall {
            ident,
            params
        })
    }

    fn parse_expression(&mut self, precidence: i8) -> Result<Box<dyn ast::Expression>, String> {
        // invalid expression if it is blank
        if self.current_token.is_none() {
            return Err("Invalid Expression: Expression must start with a literal or variable".to_string());
        }
        // Return group expression if expression starts with an open paren
        if self.current_token_is(TOKENTYPE::LPAREN) {
            return Ok(self.parse_group_expression()?)
        }
        if !self.current_token_is(TOKENTYPE::NUMBER) && !self.current_token_is(TOKENTYPE::IDENTIFIER) {
            return Err(format!("Invalid Expression: Expression must start with a literal or variable, started with: {:?}", self.current_token));
        }
        // Get "left side" of the expression
        let mut left_side: Box<dyn ast::Expression> = self.parse_prefix()?;
        while self.current_token.is_some() {
            // Recursively parse right side of equation and build on expression "left side"
            let op_prec: i8 = Self::token_type_precedence(self.current_token.as_ref().unwrap().token_type.clone());
            // If reaches a lower precidence operation than its parent call it should not simplify
            // it for the right hand side.
            // Also if it reaches an unexpected token then expression should be complete. This is
            // done bc we can assert that precidence will always be >= 0 and so an unexpected -1
            // will always break out of the loop
            if op_prec <= precidence {
                break
            }
            left_side = self.parse_infix(left_side)?;
        }
        Ok(left_side)
    }

    // Parses infix expressions including literals and identifiers
    fn parse_prefix(&mut self) -> Result<Box<dyn ast::Expression>, String> {
        let token = if self.current_token.is_some() {
            self.current_token.as_ref().unwrap()
        } else {
            return Err("Invalid Expression: Expression must start with a literal or variable".to_string());
        };
        match token.token_type {
            TOKENTYPE::IDENTIFIER => {
                let expression: Box<dyn ast::Expression> = if self.peek_token_is(TOKENTYPE::LPAREN) {
                    Box::from(self.parse_function_call()?)
                } else {
                    Box::from(self.parse_identifier())
                };
                Ok(expression)
            }
            TOKENTYPE::NUMBER => Ok(self.parse_number()?),
            _ => Err("Invalid Expression: Expression must start with a literal or variable".to_string())
        }
    }

    // Precondition: Caller must make sure this is called on an arithmetic token and provide left side of the equation as first parameter
    fn parse_infix(&mut self, op1:Box<dyn ast::Expression>) -> Result<Box<ast::InfixExpression>, String> {
        let operand_token = self.current_token.take().unwrap();
        self.next_token();
        let precidence: i8 = Self::token_type_precedence(operand_token.token_type.clone());
        let op2 = self.parse_expression(precidence)?;
        Ok(Box::from(ast::InfixExpression {
            token: operand_token,
            op1,
            op2
        }))
    }

    // Precondition: Caller must check that current_token is an identifier
    fn parse_identifier(&mut self) -> ast::Identifier {
        // Assert that the current token is a number
        assert_eq!(self.current_token.is_none(), false);
        let identifier_token = self.current_token.take().unwrap();
        assert_eq!(identifier_token.token_type, TOKENTYPE::IDENTIFIER);
        // Advance parser to next token
        self.next_token();
        ast::Identifier {
            name: identifier_token.literal.clone(),
            token: identifier_token
        }
    }

    // Precondition: Caller must check that current_token is a number
    fn parse_number(&mut self) -> Result<Box<ast::Number>, String> {
        // Assert that the current token is a number
        assert_eq!(self.current_token.is_none(), false);
        let number_token = self.current_token.take().unwrap();
        assert_eq!(number_token.token_type, TOKENTYPE::NUMBER);
        // Advance parser to next token
        self.next_token();
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

    // Precondition: Caller must only call this if the current token is an open parenthesis
    fn parse_group_expression(&mut self) -> Result<Box<dyn ast::Expression>, String> {
        // Assert that this method was called while the current token is on open parenthesis
        assert_eq!(self.current_token.is_some(), true);
        let open_token = self.current_token.take().unwrap();
        assert_eq!(open_token.token_type, TOKENTYPE::LPAREN);
        // Advance parser to interior expression
        self.next_token();
        let expression = self.parse_expression(0)?;
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
        if self.current_token_is(TOKENTYPE::SEMICOLON) {
            self.next_token();
        }

        // If next token is arithmatic then pass in the group expression as the lhs
        if self.current_token.is_some() && Parser::token_type_is_arithmatic(self.current_token.as_ref().unwrap().token_type.clone()) {
            expression = self.parse_infix(expression)?;
        }
        Ok(expression)
    }

    fn token_type_is_arithmatic(token: TOKENTYPE) -> bool {
        token == TOKENTYPE::PLUS ||
        token == TOKENTYPE::MINUS ||
        token == TOKENTYPE::ASTERISK ||
        token == TOKENTYPE::SLASH ||
        token == TOKENTYPE::EQ ||
        token == TOKENTYPE::NOTEQ
    }

    fn token_type_precedence(token: TOKENTYPE) -> i8 {
        match token {
            TOKENTYPE::EQ |
            TOKENTYPE::NOTEQ => 1,
            TOKENTYPE::LT |
            TOKENTYPE::GT => 2,
            TOKENTYPE::PLUS |
            TOKENTYPE::MINUS => 3,
            TOKENTYPE::ASTERISK |
            TOKENTYPE::SLASH => 4,
            _ => -1
        }
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
        let operation = expect_coerce::<ast::InfixExpression>(condition.as_any());
        assert_eq!(operation.op1.token_literal(), "i");
        assert_eq!(operation.op2.token_literal(), "2");
        let then = &if_statement.then_block;
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
    fn test_operator_precidence() {
        let input = String::from("let a = 1 * 2 - 3 == 3 * 4 > 3;");
        let mut parser = Parser::new(&input);
        let statements = parser.parse_program().expect("Failed to parse program").statements;
        assert_eq!(statements.len(), 1);
        let let_statement = statements.into_iter().next().expect("First statement in program is None");
        assert_eq!(let_statement.token_literal(), "let");
        let let_statement = expect_coerce::<ast::LetStatement>(let_statement.as_any());
        test_let(let_statement, "a".to_string());
        assert_eq!(let_statement.expression.token_literal(), "==");
        let expression = expect_coerce::<ast::InfixExpression>(let_statement.expression.as_any());
        assert_eq!(expression.to_string(), "(((1 * 2) - 3) == ((3 * 4) > 3))");
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
        let operation = expect_coerce::<ast::InfixExpression>(condition.as_any());
        assert_eq!(operation.op1.token_literal(), "a");
        assert_eq!(operation.op2.token_literal(), "2");
        assert_eq!(if_statement.then_block.token_literal(), "{}");
        let then_code_block = expect_coerce::<ast::CodeBlock>(&if_statement.then_block);
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
        let addition_expression = expect_coerce::<ast::InfixExpression>(let_expression.as_any());
        let lhs = &addition_expression.op1;
        assert_eq!(lhs.token_literal(), "()");
        let rhs = &addition_expression.op2;
        assert_eq!(rhs.token_literal(), "a");
        let group_expression = expect_coerce::<ast::GroupExpression>(lhs.as_any());
        assert_eq!(group_expression.expression.token_literal(), "*");
        let multiplication_expression = expect_coerce::<ast::InfixExpression>(group_expression.expression.as_any());
        assert_eq!(multiplication_expression.op1.token_literal(), "1");
        assert_eq!(multiplication_expression.op2.token_literal(), "2");
    }
}
