use std::{fmt::Display, num::ParseIntError};

use crate::{ast::{self, join}, lexer::{Lexer, Location, Token, TOKENTYPE}};

#[derive(PartialEq, Debug)]
enum ParserErrorType {
    UnexpectedToken {
        expected_token: TOKENTYPE,
        recieved_token: TOKENTYPE,
    },
    UnexpectedTokenOption {
        expected_token_options: Vec<TOKENTYPE>,
        recieved_token: TOKENTYPE
    },
    TokenNotInfixOp(TOKENTYPE),
    ParseIntError(ParseIntError),
    UnexpectedEOF
}

impl From<ast::TokenNotInfixOp> for ParserErrorType {
    fn from(err: ast::TokenNotInfixOp) -> Self {
        Self::TokenNotInfixOp(err.0)
    }
}

impl From<ParseIntError> for ParserErrorType {
    fn from(err: ParseIntError) -> Self {
        Self::ParseIntError(err)
    }
}

impl Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEOF => f.write_str("Unexpected end of file"),
            Self::TokenNotInfixOp(token) => write!(f, "Could not translate token {} to Infix Operand", token),
            Self::ParseIntError(err) => write!(f, "Error parsing number: {}", err),
            Self::UnexpectedToken { expected_token, recieved_token } => write!(f, "Expected token {}, found {}", expected_token, recieved_token),
            Self::UnexpectedTokenOption { expected_token_options, recieved_token } => write!(f, "Expected one of the following tokens [{}], found {}", join(expected_token_options, ", "), recieved_token)
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct ParserError {
    error_type: ParserErrorType,
    location: Location
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parser Error at line {} column {}: {}", self.location.line, self.location.position, self.error_type)
    }
}

type ParseResult<T> = Result<T, ParserError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    location: Location
}

impl<'a> Parser<'a> {
    pub fn new(contents: &'a str) -> Parser<'a> {
        let lexer = Lexer::new(contents);
        Self {
            lexer,
            current_token: None,
            peek_token: None,
            location: Location::default()
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.get_next_token();
        if self.current_token.is_some() {
            self.location = self.current_token.as_ref().unwrap().location.clone();
        }
    }

    fn current_token_is(&self, token_type: TOKENTYPE) -> bool {
        self.current_token.is_some() && self.current_token.as_ref().unwrap().token_type == token_type
    }

    pub fn parse_program(&mut self) -> ParseResult<Vec<ast::Statement>> {
        self.next_token();
        self.next_token();
        let mut statements: Vec<ast::Statement> = Vec::new();
        while self.current_token.as_ref().is_some() {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    // Precondition: Caller must only call this if the current token is an open squig
    fn parse_block(&mut self) -> ParseResult<Vec<ast::Statement>> {
        // Expect that the current token is an if token
        self.expect_token(TOKENTYPE::LSQUIG)?;
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
        Err(self.error(ParserErrorType::UnexpectedToken { 
            expected_token: TOKENTYPE::RSQUIG,
            recieved_token: self.current_token.as_ref().unwrap().token_type.clone()
        }))
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        match self.current_token.as_ref().unwrap().token_type {
            TOKENTYPE::LET => Ok(self.parse_let_statement()?),
            TOKENTYPE::IDENTIFIER => {
                let assignable = self.parse_assignable()?;
                if let Some(token) = self.current_token.as_ref() {
                    match token.token_type {
                        TOKENTYPE::EQUAL => Ok(self.parse_assign_statement(assignable)?),
                        TOKENTYPE::LPAREN => Ok(ast::Statement::Expression(self.parse_function_call(assignable)?)),
                        TOKENTYPE::SEMICOLON => {
                            self.next_token();
                            Ok(ast::Statement::Expression(ast::Expression::Assignable(assignable)))
                        }
                        _ => {
                            let res = Ok(ast::Statement::Expression(self.parse_infix(ast::Expression::Assignable(assignable))?));
                            if self.current_token_is(TOKENTYPE::SEMICOLON) {
                                self.next_token();
                            };
                            res
                        }
                    }
                } else {
                    Ok(ast::Statement::Expression(ast::Expression::Assignable(assignable)))
                }
            },
            TOKENTYPE::WHILE => Ok(self.parse_while_loop()?),
            TOKENTYPE::IF => Ok(self.parse_if_statement()?),
            TOKENTYPE::RETURN => Ok(self.parse_return_statement()?),
            TOKENTYPE::FN => Ok(self.parse_function()?),
            _ => Ok(self.parse_expression_statement()?)
        }
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ast::Statement> {
        let expression = self.parse_expression(0)?;
        Ok(ast::Statement::Expression(expression))
    }

    // Precondition: caller must check that current token is a let token
    fn parse_let_statement(&mut self) -> ParseResult<ast::Statement> {
        // Expect the current token to be a let token
        self.expect_token(TOKENTYPE::LET)?;
        // Advance to identifier
        self.next_token();
        // If no token after let keyword or token after let keyword is not an identifier it is an invalid let statement
        if self.current_token.is_none() {
            return Err(self.error(ParserErrorType::UnexpectedEOF));
        }
        self.expect_token(TOKENTYPE::IDENTIFIER)?;
        // If no token after identifier or token after identifier is not an equal sign it is an invalid let statement
        self.expect_peek_token(TOKENTYPE::EQUAL)?;
        // Now that we know that parse identifier will not panic, i.e., it is followed by a token, we get the identifier token
        let ident = self.parse_identifier()?;
        // Advance to expression / skip equal sign
        self.next_token();
        let expression = self.parse_expression(0)?;
        self.expect_token(TOKENTYPE::SEMICOLON)?;
        self.next_token();
        Ok(ast::Statement::Let {
            ident,
            expression
        })
    }

    // Precondition: caller must ensure that current token is an equal token
    fn parse_assign_statement(&mut self, assignable: ast::Assignable) -> ParseResult<ast::Statement> {
        self.expect_token(TOKENTYPE::EQUAL)?;
        // Move on to expression
        self.next_token();
        let expression = self.parse_expression(0)?;
        self.expect_token(TOKENTYPE::SEMICOLON)?;
        self.next_token();
        Ok(ast::Statement::Assign {
            assignable,
            expression
        })
    }

    // Precondition: Caller must only call this if the current token is a while token
    fn parse_while_loop(&mut self) -> ParseResult<ast::Statement> {
        // Expect that the current token is a while token
        self.expect_token(TOKENTYPE::WHILE)?;
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
    fn parse_if_statement(&mut self) -> ParseResult<ast::Statement> {
        // Expect that the current token is an if token
        self.expect_token(TOKENTYPE::IF)?;
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
    fn parse_return_statement(&mut self) -> ParseResult<ast::Statement> {
        // Expect current token to be return
        self.expect_token(TOKENTYPE::RETURN)?;
        self.next_token();
        if self.current_token.is_none() {
            return Err(self.error(ParserErrorType::UnexpectedEOF));
        }
        let return_value = if self.current_token_is(TOKENTYPE::SEMICOLON) {
            None
        } else {
            Some(self.parse_expression(0)?)
        };
        self.expect_token(TOKENTYPE::SEMICOLON)?;
        self.next_token();
        Ok(ast::Statement::Return(return_value))
    }

    // Precondition: Caller must only call this if the current token is an if token
    fn parse_function(&mut self) -> ParseResult<ast::Statement> {
        // Expect current token to be fn 
        self.expect_token(TOKENTYPE::FN)?;
        // Move on to function Identifier
        self.next_token();
        self.expect_token(TOKENTYPE::IDENTIFIER)?;
        let ident = self.parse_identifier()?;
        // Expect an open parenthesis before parameters after function name
        self.expect_token(TOKENTYPE::LPAREN)?;
        // Move on to parameters
        self.next_token();
        let mut params = Vec::new();
        while self.current_token_is(TOKENTYPE::IDENTIFIER) {
            params.push(self.parse_identifier()?);
            if self.current_token_is(TOKENTYPE::COMMA) {
                self.next_token()
            } else {
                break
            }
        }
        self.expect_token(TOKENTYPE::RPAREN)?;
        // Parse function body
        self.next_token();
        self.expect_token(TOKENTYPE::LSQUIG)?;
        let body = self.parse_block()?;
        Ok(ast::Statement::FuncDecl(ast::Function{
            ident,
            params,
            body
        }))
    }

    // Precondition: Caller must make sure that current token is an identifier and that the
    // next token is an open parenthesis
    fn parse_function_call(&mut self, func: ast::Assignable) -> ParseResult<ast::Expression> {
        // Expect current tokent to be lparen
        self.expect_token(TOKENTYPE::LPAREN)?;
        // Move on to parameters
        self.next_token();
        let mut params: Vec<ast::Expression> = Vec::new();
        while  self.current_token.is_some() && !self.current_token_is(TOKENTYPE::RPAREN) {
            params.push(self.parse_expression(0)?);
            if self.current_token_is(TOKENTYPE::COMMA) {
                self.next_token()
            } else {
                break
            }
        }
        // Param list should end with a close parenthesis otherwise it is an invalid function call
        self.expect_token(TOKENTYPE::RPAREN)?;
        self.next_token();
        Ok(ast::Expression::FuncCall {
            func,
            params
        })
    }

    fn parse_expression(&mut self, precidence: i8) -> ParseResult<ast::Expression> {
        // invalid expression if it is blank
        if self.current_token.is_none() {
            return Err(self.error(ParserErrorType::UnexpectedEOF));
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
    fn parse_prefix(&mut self) -> ParseResult<ast::Expression> {
        // Expect prefix expression to start with an identifier, a minus, or a number
        self.expect_one_of(vec![TOKENTYPE::IDENTIFIER, TOKENTYPE::MINUS, TOKENTYPE::NUMBER])?;
        match self.current_token.as_ref().unwrap().token_type {
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
                    unreachable!()
                }
            }
            TOKENTYPE::NUMBER => Ok(ast::Expression::Literal(self.parse_number()?)),
            _ => unreachable!()
        }
    }

    // Precondition: Caller must make sure this is called on an arithmetic token and provide left side of the equation as first parameter
    fn parse_infix(&mut self, left:ast::Expression) -> ParseResult<ast::Expression> {
        let operand_token = self.current_token.take().unwrap();
        self.next_token();
        let precidence: i8 = Self::token_type_precedence(operand_token.token_type.clone());
        let right = self.parse_expression(precidence)?;
        let op = match operand_token.try_into() {
            Ok(op) => op,
            Err(err) => {
                let err: ast::TokenNotInfixOp = err;
                return Err(self.error(err.into()));
            }
        };
        Ok(ast::Expression::Infix {
            op,
            left: Box::new(left),
            right: Box::new(right)
        })
    }

    fn parse_assignable(&mut self) -> ParseResult<ast::Assignable> {
        let mut assignable = ast::Assignable::Var(self.parse_identifier()?);
        while self.current_token.is_some() {
            match self.current_token.as_ref().unwrap().token_type {
                TOKENTYPE::LSQR => {
                    // Skip past left square bracker
                    self.next_token();
                    // Parse the index into the array
                    let index = Box::from(self.parse_expression(0)?);
                    // Array index is invalid if it is not closed
                    self.expect_token(TOKENTYPE::RSQR)?;
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
                    let prop = self.parse_identifier()?;
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
    fn parse_identifier(&mut self) -> ParseResult<String> {
        // Expect the current token to be an identifier
        self.expect_token(TOKENTYPE::IDENTIFIER)?;
        let identifier_token = self.current_token.take().unwrap();
        // Advance parser to next token
        self.next_token();
        Ok(identifier_token.literal.clone())
    }

    // Precondition: Caller must check that current_token is an LSQR
    fn parse_array_literal(&mut self) -> ParseResult<ast::Literal> {
        // Expect the current token to be LSQR
        self.expect_token(TOKENTYPE::LSQR)?;
        // Advance parser to array items
        self.next_token();
        let mut values = Vec::new();
        while self.current_token.is_some() && !self.current_token_is(TOKENTYPE::RSQR) {
            values.push(self.parse_expression(0)?);
            if self.current_token_is(TOKENTYPE::COMMA) {
                self.next_token();
            } else {
                break
            }
        }
        self.expect_token(TOKENTYPE::RSQR)?;
        // let close_token = self.current_token.take().unwrap();
        self.next_token();
        Ok(ast::Literal::ArrayLit(values))
    }

    // Precondition: Caller must make sure that current_token is an left squigly
    fn parse_object_literal(&mut self) -> ParseResult<ast::Literal> {
        // Expect current token to be LSQUIG
        self.expect_token(TOKENTYPE::LSQUIG)?;
        // Move on to fields
        self.next_token();
        let mut fields = Vec::new();
        while self.current_token_is(TOKENTYPE::IDENTIFIER) {
            let name = self.parse_identifier()?;
            self.expect_token(TOKENTYPE::COLON)?;
            // Move past colon to expression
            self.next_token();
            let value = self.parse_expression(0)?;
            fields.push((name, value));
            // Each property should be delimited by a comma
            if self.current_token_is(TOKENTYPE::COMMA) {
                self.next_token()
            } else {
                break
            }
        }
        self.expect_token(TOKENTYPE::RSQUIG)?;
        self.next_token();
        Ok(ast::Literal::ObjectLit(fields))
    }

    // Precondition: Caller must check that current_token is a number
    fn parse_number(&mut self) -> ParseResult<ast::Literal> {
        // Expect that the current token is a number
        self.expect_token(TOKENTYPE::NUMBER)?;
        let number_token = self.current_token.take().unwrap();
        // Advance parser to next token
        self.next_token();
        // Create and return number Expression
        let value = match number_token.literal.parse::<isize>() {
            Ok(num) => num,
            Err(err) => {
                return Err(self.error(err.into()));
            }
        };
        Ok(ast::Literal::NumberLit(value as i64))
    }

    // Precondition: Caller must only call this if the current token is an open parenthesis
    fn parse_group_expression(&mut self) -> ParseResult<ast::Expression> {
        // Expect that the current token is LPAREN
        self.expect_token(TOKENTYPE::LPAREN)?;
        // Advance parser to interior expression
        self.next_token();
        let expression = self.parse_expression(0)?;
        // Check that the opened group is closed
        self.expect_token(TOKENTYPE::RPAREN)?;
        // Contruct Group expression
        let mut expression: ast::Expression = ast::Expression::Group(Box::from(expression));
        // Move past close expression
        self.next_token();
        if self.current_token.is_none() {
            return Err(self.error(ParserErrorType::UnexpectedEOF));
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

    fn error(&self, error_type: ParserErrorType) -> ParserError {
        ParserError { 
            error_type,
            location: self.location.clone()
        }
    }

    fn expect_token(&self, expected_token: TOKENTYPE) -> ParseResult<()> {
        if let Some(current_token) = self.current_token.as_ref() {
            if current_token.token_type == expected_token {
                Ok(())
            } else {
                Err(self.error(ParserErrorType::UnexpectedToken { expected_token, recieved_token: current_token.token_type.clone() }))
            }
        } else {
            Err(self.error(ParserErrorType::UnexpectedEOF))
        }
    }

    fn expect_peek_token(&self, expected_token: TOKENTYPE) -> ParseResult<()> {
        if let Some(peek_token) = self.peek_token.as_ref() {
            if peek_token.token_type == expected_token {
                Ok(())
            } else {
                Err(self.error(ParserErrorType::UnexpectedToken {
                    expected_token,
                    recieved_token: peek_token.token_type.clone()
                }))
            }
        } else {
            Err(self.error(ParserErrorType::UnexpectedEOF))
        }
    }
    
    fn expect_one_of(&self, expected_token_options: Vec<TOKENTYPE>) -> ParseResult<()> {
        if let Some(current_token) = self.current_token.as_ref() {
            if expected_token_options.contains(&current_token.token_type) {
                Ok(())
            } else {
                Err(self.error(ParserErrorType::UnexpectedTokenOption {
                    expected_token_options,
                    recieved_token: current_token.token_type.clone()
                }))
            }
        } else {
            Err(self.error(ParserErrorType::UnexpectedEOF))
        }
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
            Ok(vec![ast::Statement::Let {
                ident: "i".to_string(),
                expression: ast::Expression::Literal(
                    ast::Literal::NumberLit(12)
                )
            }])
        );
    }

    #[test]
    fn assign_statment() {
        let input = String::from("i = 12;\nvar = [1, 2, 3];\nvar_two = { a: 1, b: 2 };");
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![
                ast::Statement::Assign {
                    assignable: ast::Assignable::Var(String::from("i")),
                    expression: ast::Expression::Literal(
                        ast::Literal::NumberLit(12)
                    )
                },
                ast::Statement::Assign {
                    assignable: ast::Assignable::Var(String::from("var")),
                    expression: ast::Expression::Literal(
                        ast::Literal::ArrayLit(vec![
                            ast::Expression::Literal(ast::Literal::NumberLit(1)),
                            ast::Expression::Literal(ast::Literal::NumberLit(2)),
                            ast::Expression::Literal(ast::Literal::NumberLit(3)),
                        ])
                    )
                },
                ast::Statement::Assign {
                    assignable: ast::Assignable::Var(String::from("var_two")),
                    expression: ast::Expression::Literal(
                        ast::Literal::ObjectLit(vec![
                            (String::from("a"), ast::Expression::Literal(ast::Literal::NumberLit(1))),
                            (String::from("b"), ast::Expression::Literal(ast::Literal::NumberLit(2)))
                        ])
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
            Ok(vec![ast::Statement::Return(Some(ast::Expression::Infix {
                op: ast::InfixOp::Add,
                left: Box::from(ast::Expression::Assignable(ast::Assignable::Var("i".to_string()))),
                right: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(1)))
            }))])
        );
    }

    #[test]
    fn while_statement() {
        let input = "while i {i = i - 1;}".to_string();
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![ast::Statement::While { 
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
            }])
        )
    }

    #[test]
    fn if_statement() {
        let input = "if i > 2 * 2 {\n return i + 2;\n} else { return i; }".to_string();
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![ast::Statement::If {
                cond: ast::Expression::Infix {
                    left: Box::from(ast::Expression::Assignable(ast::Assignable::Var(String::from("i")))),
                    right: Box::from(ast::Expression::Infix {
                        left: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(2))),
                        right: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(2))),
                        op: ast::InfixOp::Mult
                    }),
                    op: ast::InfixOp::Gt
                },
                body: vec![ast::Statement::Return(Some(ast::Expression::Infix {
                    left: Box::from(ast::Expression::Assignable(ast::Assignable::Var(String::from("i")))),
                    right: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(2))),
                    op: ast::InfixOp::Add
                }))],
                r#else: Some(vec![ast::Statement::Return(Some(ast::Expression::Assignable(
                    ast::Assignable::Var(String::from("i"))
                )))])
            }])
        )
    }

    #[test]
    fn func_decl() {
        let input = "fn times_two(num) {\n\treturn num * 2;\n}".to_string();
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![
                ast::Statement::FuncDecl(ast::Function {
                    ident: String::from("times_two"),
                    params: vec![String::from("num")],
                    body: vec![ast::Statement::Return(Some(
                        ast::Expression::Infix {
                            left: Box::from(ast::Expression::Assignable(ast::Assignable::Var(String::from("num")))),
                            right: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(2))),
                            op: ast::InfixOp::Mult
                        }
                    ))]
                })
            ])
        )
    }

    #[test]
    fn expression_statement() {
        let input = "2 * times_two(num[2])".to_string();
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![ast::Statement::Expression(ast::Expression::Infix {
                left: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(2))),
                right:Box::from(ast::Expression::FuncCall {
                    func: ast::Assignable::Var(String::from("times_two")),
                    params: vec![ast::Expression::Assignable(ast::Assignable::ArrayIndex{
                        array: Box::from(ast::Assignable::Var(String::from("num"))),
                        index: Box::from(ast::Expression::Literal(ast::Literal::NumberLit(2)))
                    })]
                }),
                op: ast::InfixOp::Mult
            })])
        )
    }
}
