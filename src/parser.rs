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

    pub fn parse_program(&mut self) -> Result<Vec<ast::Statement>, String> {
        self.next_token();
        self.next_token();
        let mut statements: Vec<ast::Statement> = Vec::new();
        while self.current_token.as_ref().is_some() {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    // Precondition: Caller must only call this if the current token is an open squig
    fn parse_block(&mut self) -> Result<Vec<ast::Statement>, String> {
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
                self.next_token();
                return Ok(statements);
            } else {
                statements.push(self.parse_statement()?);
            }
        }
        Err(String::from("Code block was not closed, did you forget a \"}\""))
    }


    fn parse_statement(&mut self) -> Result<ast::Statement, String> {
        match self.current_token.as_ref().unwrap().token_type {
            TOKENTYPE::LET => Ok(self.parse_let_statement()?),
            TOKENTYPE::IDENTIFIER => {
                let assignable = self.parse_assignable()?;
                if self.current_token_is(TOKENTYPE::EQUAL) {
                    Ok(self.parse_assign_statement(assignable)?)
                } else if self.current_token_is(TOKENTYPE::LPAREN) {
                    Ok(ast::Statement::Expression(
                        self.parse_function_call(assignable)?
                    ))
                }else {
                    Ok(ast::Statement::Expression(
                        ast::Expression::Assignable(assignable)
                    ))
                }
            },
            TOKENTYPE::WHILE => Ok(self.parse_while_loop()?),
            TOKENTYPE::IF => Ok(self.parse_if_statement()?),
            TOKENTYPE::RETURN => Ok(self.parse_return_statement()?),
            TOKENTYPE::FN => Ok(self.parse_function()?),
            _ => Ok(self.parse_expression_statement()?)
        }
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement, String> {
        let expression = self.parse_expression(0)?;
        Ok(ast::Statement::Expression(expression))
    }
    // Precondition: caller must check that current token is a let token
    fn parse_let_statement(&mut self) -> Result<ast::Statement, String> {
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
        let ident = self.parse_identifier();
        // Advance to expression / skip equal sign
        self.next_token();
        let expression = self.parse_expression(0)?;
        if !self.current_token_is(TOKENTYPE::SEMICOLON) {
            return Err(String::from("Let statement must end with a semicolon"));
        } else {
            self.next_token();
        }
        Ok(ast::Statement::Let {
            ident,
            expression
        })
    }

    // Precondition: caller must ensure that current token is an equal token
    fn parse_assign_statement(&mut self, assignable: ast::Assignable) -> Result<ast::Statement, String> {
        if !self.current_token_is(TOKENTYPE::EQUAL) {
            return Err(String::from("variable assignment must have a \"=\" after variable name"));
        }
        // Move on to expression
        self.next_token();
        let expression = self.parse_expression(0)?;
        if !self.current_token_is(TOKENTYPE::SEMICOLON) {
            return Err(format!("Variable assignment must end with a semicolon, ended with {:?}", self.current_token));
        } else {
            self.next_token();
        }
        Ok(ast::Statement::Assign {
            assignable,
            expression
        })
    }

    // Precondition: Caller must only call this if the current token is a while token
    fn parse_while_loop(&mut self) -> Result<ast::Statement, String> {
        // Assert that the current token is a while token
        assert!(self.current_token_is(TOKENTYPE::WHILE));
        // let token: Token = self.current_token.take().unwrap();
        // Advance to expression to validate
        self.next_token();
        let cond = self.parse_expression(0)?;
        let body = self.parse_block()?;
        Ok(ast::Statement::While {
            cond,
            body
        })
    }

    // Precondition: Caller must only call this if the current token is an if token
    fn parse_if_statement(&mut self) -> Result<ast::Statement, String> {
        // Assert that the current token is an if token
        assert!(self.current_token_is(TOKENTYPE::IF));
        // let if_token: Token = self.current_token.take().unwrap();
        // Advance to expression to validate
        self.next_token();
        let cond = self.parse_expression(0)?;
        let body = self.parse_block()?;
        let r#else = if self.current_token_is(TOKENTYPE::ELSE) {
            self.next_token();
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(ast::Statement::If {
            cond,
            body,
            r#else
        })
    }

    // Precondition: Caller must only call this if the current token is a return token
    fn parse_return_statement(&mut self) -> Result<ast::Statement, String> {
        assert!(self.current_token_is(TOKENTYPE::RETURN));
        // let token = self.current_token.take().unwrap();
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
        Ok(ast::Statement::Return(return_value))
    }

    // Precondition: Caller must only call this if the current token is an if token
    fn parse_function(&mut self) -> Result<ast::Statement, String> {
        // Ensure parse function was not called while token is not fn
        assert!(self.current_token.is_some());
        assert_eq!(self.current_token.as_ref().unwrap().token_type, TOKENTYPE::FN);
        // Get fn token
        // let token = self.current_token.take().unwrap();
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
        Ok(ast::Statement::FuncDecl(ast::Function{
            ident,
            params,
            body
        }))
    }

    // Precondition: Caller must make sure that current token is an identifier and that the
    // next token is an open parenthesis
    fn parse_function_call(&mut self, func: ast::Assignable) -> Result<ast::Expression, String> {
        assert!(self.current_token_is(TOKENTYPE::LPAREN));
        // Move on to parameters
        self.next_token();
        let mut params: Vec<ast::Expression> = Vec::new();
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
        Ok(ast::Expression::FuncCall {
            func,
            params
        })
    }

    fn parse_expression(&mut self, precidence: i8) -> Result<ast::Expression, String> {
        // invalid expression if it is blank
        if self.current_token.is_none() {
            return Err("Invalid Expression: Expression can not be empty".to_string());
        }
        // Return group expression if expression starts with an open paren
        if self.current_token_is(TOKENTYPE::LPAREN) {
            return Ok(self.parse_group_expression()?)
        }
        // Return array literal if expression starts with an open square bracket
        if self.current_token_is(TOKENTYPE::LSQR) {
            return Ok(ast::Expression::Literal(self.parse_array_literal()?))
        }
        if self.current_token_is(TOKENTYPE::LSQUIG) {
            return Ok(ast::Expression::Literal(self.parse_object_literal()?))
        }
        if !self.current_token_is(TOKENTYPE::NUMBER) && !self.current_token_is(TOKENTYPE::IDENTIFIER) {
            return Err(format!("Invalid Expression: Expression must start with a literal or variable, started with: {:?}", self.current_token));
        }
        // Get "left side" of the expression
        let mut left_side: ast::Expression = self.parse_prefix()?;
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
    fn parse_prefix(&mut self) -> Result<ast::Expression, String> {
        let token = if self.current_token.is_some() {
            self.current_token.as_ref().unwrap()
        } else {
            return Err("Invalid Expression: Expression must start with a literal or variable".to_string());
        };
        match token.token_type {
            TOKENTYPE::IDENTIFIER => {
                let assignable = self.parse_assignable()?;
                if self.current_token_is(TOKENTYPE::LPAREN) {
                    return self.parse_function_call(assignable);
                }
                Ok(ast::Expression::Assignable(assignable))
            },
            TOKENTYPE::MINUS => {
                self.next_token();
                let number = self.parse_number()?;
                if let ast::Literal::NumberLit(num) = number {
                    Ok(ast::Expression::Literal(ast::Literal::NumberLit(-num)))
                } else {
                    Err("Can not negate non number literal".to_string())
                }
            }
            TOKENTYPE::NUMBER => Ok(ast::Expression::Literal(self.parse_number()?)),
            _ => Err("Invalid Expression: Expression must start with a literal or variable".to_string())
        }
    }

    // Precondition: Caller must make sure this is called on an arithmetic token and provide left side of the equation as first parameter
    fn parse_infix(&mut self, left:ast::Expression) -> Result<ast::Expression, String> {
        let operand_token = self.current_token.take().unwrap();
        self.next_token();
        let precidence: i8 = Self::token_type_precedence(operand_token.token_type.clone());
        let right = self.parse_expression(precidence)?;
        Ok(ast::Expression::Infix {
            op: operand_token.try_into()?,
            left: Box::new(left),
            right: Box::new(right)
        })
    }

    fn parse_assignable(&mut self) -> Result<ast::Assignable, String> {
        let mut assignable = ast::Assignable::Var(self.parse_identifier());
        while self.current_token.is_some() {
            match self.current_token.as_ref().unwrap().token_type {
                TOKENTYPE::LSQR => {
                    // Skip past left square bracker
                    self.next_token();
                    // Parse the index into the array
                    let index = Box::from(self.parse_expression(0)?);
                    // Array index is invalid if it is not closed
                    if !self.current_token_is(TOKENTYPE::RSQR) {
                        return Err("Array index expects to be closed with \"]\"".to_string());
                    }
                    // Skip past the right square bracket
                    self.next_token();
                    assignable = ast::Assignable::ArrayIndex {
                        array: Box::from(assignable), 
                        index
                    }
                },
                TOKENTYPE::DOT => {
                    // Skip past the dot
                    self.next_token();
                    // Get the property
                    let prop = self.parse_identifier();
                    assignable = ast::Assignable::ObjectProp { 
                        obj: Box::from(assignable),
                        prop
                    }
                },
                _ => break
            };
        }
        Ok(assignable)
    }

    // Precondition: Caller must check that current_token is an identifier
    fn parse_identifier(&mut self) -> String {
        // Assert that the current token exists 
        assert_eq!(self.current_token.is_none(), false);
        let identifier_token = self.current_token.take().unwrap();
        assert_eq!(identifier_token.token_type, TOKENTYPE::IDENTIFIER);
        // Advance parser to next token
        self.next_token();
        identifier_token.literal.clone()
    }

    // Precondition: Caller must check that current_token is an LSQR
    fn parse_array_literal(&mut self) -> Result<ast::Literal, String> {
        // Assert that the current token is a LSQR
        assert!(self.current_token_is(TOKENTYPE::LSQR));
        // let open_token = self.current_token.take().unwrap();
        // Advance parser to array items
        self.next_token();
        let mut values = Vec::new();
        while self.current_token.is_some() && !self.current_token_is(TOKENTYPE::RSQR) {
            values.push(Box::from(self.parse_expression(0)?));
            if self.current_token_is(TOKENTYPE::COMMA) {
                self.next_token();
            } else {
                break
            }
        }
        if !self.current_token_is(TOKENTYPE::RSQR) {
            return Err("Invalid array literal: Expected array literal to end with a \"]\"".to_string());
        }
        // let close_token = self.current_token.take().unwrap();
        self.next_token();
        Ok(ast::Literal::ArrayLit(values))
    }

    // Precondition: Caller must make sure that current_token is an left squigly
    fn parse_object_literal(&mut self) -> Result<ast::Literal, String> {
        assert!(self.current_token_is(TOKENTYPE::LSQUIG));
        // let open_token = self.current_token.take().unwrap();
        // Move on to fields
        self.next_token();
        let mut fields = Vec::new();
        while self.current_token_is(TOKENTYPE::IDENTIFIER) {
            let name = self.parse_identifier();
            if !self.current_token_is(TOKENTYPE::COLON) {
                return Err("Invalid Object Literal: Expected a colon after field name and before value expression".to_string());
            }
            // Move past colon to expression
            self.next_token();
            let value = Box::from(self.parse_expression(0)?);
            fields.push((name, value));
            // Each property should be delimited by a comma
            if self.current_token_is(TOKENTYPE::COMMA) {
                self.next_token()
            } else {
                break
            }
        }
        if !self.current_token_is(TOKENTYPE::RSQUIG) {
            return Err("Invalid Object Literal: Expected literal to be closed with a \"}\"".to_string());
        }
        // let close_token = self.current_token.take().unwrap();
        self.next_token();
        Ok(ast::Literal::ObjectLit(fields))
    }

    // Precondition: Caller must check that current_token is a number
    fn parse_number(&mut self) -> Result<ast::Literal, String> {
        // Assert that the current token is a number
        assert!(self.current_token_is(TOKENTYPE::NUMBER));
        let number_token = self.current_token.take().unwrap();
        // Advance parser to next token
        self.next_token();
        // Create and return number Expression
        let value = match number_token.literal.parse::<isize>() {
            Ok(num) => num,
            Err(_) => {
                return Err(String::from("Parse number encountered error parsing literal as number"));
            }
        };
        Ok(ast::Literal::NumberLit(value as i64))
    }

    // Precondition: Caller must only call this if the current token is an open parenthesis
    fn parse_group_expression(&mut self) -> Result<ast::Expression, String> {
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
        let mut expression: ast::Expression = ast::Expression::Group(Box::from(expression));

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
    use super::*;

    #[test]
    fn let_statement() {
        let input = String::from("let i = 12;");
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![
                ast::Statement::Let {
                    ident: "i".to_string(),
                    expression: ast::Expression::Literal(
                        ast::Literal::NumberLit(12)
                    )
                }
            ])
        );
    }

    #[test]
    fn return_statement() {
        let input = String::from("return i + 1;");
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![
                ast::Statement::Return(Some(ast::Expression::Infix {
                    op: ast::InfixOp::Add,
                    left: Box::from(ast::Expression::Assignable(ast::Assignable::Var("i".to_string()))),
                    right: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(1)))
                }))
            ])
        );
    }

    #[test]
    fn while_statement() {
        let input = "while i {i = i - 1;}".to_string();
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![
                ast::Statement::While { 
                    cond: ast::Expression::Assignable(
                        ast::Assignable::Var("i".to_string())
                    ),
                    body: vec![
                        ast::Statement::Assign { 
                            assignable: ast::Assignable::Var("i".to_string()),
                            expression: ast::Expression::Infix { 
                                op: ast::InfixOp::Sub,
                                left: Box::from(ast::Expression::Assignable(ast::Assignable::Var("i".to_string()))),
                                right: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(1)))
                            }
                        }
                    ]
                }
            ])
        )
    }
}
