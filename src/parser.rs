use std::fmt::Display;
use std::iter::Peekable;

use crate::ast::{ self, join, TokenNotInfixOpErr };
use crate::lexer::{ Lexer, LexerError, Location, Token, TokenWithLocation };

#[derive(PartialEq, Debug)]
enum ParserErrorType {
    UnexpectedToken {
        expected_token: Token,
        recieved_token: Token,
    },
    UnexpectedTokenOption {
        expected_token_options: Vec<Token>,
        recieved_token: Token
    },
    TokenNotInfixOp(TokenNotInfixOpErr),
    LexerError(LexerError),
    UnexpectedEOF
}

impl From<ast::TokenNotInfixOpErr> for ParserErrorType {
    fn from(err: ast::TokenNotInfixOpErr) -> Self {
        Self::TokenNotInfixOp(err)
    }
}

impl Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEOF => f.write_str("Unexpected end of file"),
            Self::TokenNotInfixOp(err) => write!(f, "{}", err),
            Self::LexerError(err) => write!(f, "{}", err),
            Self::UnexpectedToken { expected_token, recieved_token } => write!(f, "Expected token {}, found {}", expected_token, recieved_token),
            Self::UnexpectedTokenOption { expected_token_options, recieved_token } => write!(f, "Expected one of the following tokens [{}], found {}", join(expected_token_options, ", "), recieved_token)
        }
    }
}

/// Error type generated when parsing fails
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

impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> Self {
        Self {
            location: err.location.clone(),
            error_type: ParserErrorType::LexerError(err)
        }
    }
}

type ParseResult<T> = Result<T, ParserError>;

/// Struct that turns a string into an AST(Abstract Syntax Tree)
pub struct Parser<'a> {
    token_iter: Peekable<Lexer<'a>>,
    location: Location
}

impl<'a> Parser<'a> {
    /// Creates a new parser from an input string
    pub fn new(contents: &'a str) -> Parser<'a> {
        let lexer = Lexer::new(contents);
        Self {
            token_iter: lexer.peekable(),
            location: Location::default()
        }
    }

    fn next_token(&mut self) -> ParseResult<Token> {
        match self.token_iter.next() {
            Some(token) => {
                let token = token?;
                self.location = token.clone_location();
                Ok(token.to_token())
            },
            None => Err(self.error(ParserErrorType::UnexpectedEOF))
        }
    }

    fn peek_next_token(&mut self) -> ParseResult<Option<&Token>> {
        match self.token_iter.peek() {
            Some(token) => match token.as_ref() {
                Ok(token) => {
                    Ok(Some(token.ref_token()))
                },
                Err(err) => Err(err.clone())?

            },
            None => Ok(None)
        }
    }

    fn peek_token_is(&mut self, token: &Token) -> ParseResult<bool> {
        Ok(self.peek_next_token()? == Some(token))
    }

    fn is_eof(&mut self) -> ParseResult<bool> {
        Ok(self.peek_next_token()?.is_none())
    }

    fn peek_token_is_arithmetic(&mut self) -> ParseResult<bool> {
        if let Some(token) = self.peek_next_token()? {
            match token {
                Token::PLUS |
                Token::MINUS |
                Token::ASTERISK |
                Token::SLASH |
                Token::EQ |
                Token::NOTEQ => Ok(true),
                _ => Ok(false)
            }
        } else {
            Err(self.error(ParserErrorType::UnexpectedEOF))
        }
    }

    fn skip_optional_token(&mut self, optional_token: &Token) -> ParseResult<bool> {
        if self.peek_token_is(optional_token)? {
            self.next_token()?;
            return Ok(true)
        }
        Ok(false)
    }

    /// Turns the input string into a vec of statements
    ///
    /// If parsing fails it returns a [ParserError]
    pub fn parse_program(&mut self) -> ParseResult<Vec<ast::Statement>> {
        let mut statements: Vec<ast::Statement> = Vec::new();
        while !self.is_eof()? {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    // Precondition: Caller must only call this if the current token is an open squig
    fn parse_block(&mut self) -> ParseResult<Vec<ast::Statement>> {
        // Expect that the current token is an lsquig token
        self.expect_token(Token::LSQUIG)?;
        let mut statements = Vec::new();
        while let Some(token) = self.peek_next_token()? {
            if token == &Token::RSQUIG {
                // If you reach the close token then the block is done being read
                self.next_token()?;
                return Ok(statements);
            } else {
                statements.push(self.parse_statement()?);
            }
        }
        Err(self.error(ParserErrorType::UnexpectedEOF))
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        match self.peek_next_token()?.unwrap() {
            Token::LET => Ok(self.parse_let_statement()?),
            Token::IDENTIFIER(_) => {
                let assignable = self.parse_assignable()?;
                Ok(self.parse_statement_after_assignable(assignable)?)
            },
            Token::WHILE => Ok(self.parse_while_loop()?),
            Token::IF => Ok(self.parse_if_statement()?),
            Token::RETURN => Ok(self.parse_return_statement()?),
            Token::FN => Ok(self.parse_function()?),
            _ => Ok(self.parse_expression_statement()?)
        }
    }

    fn parse_statement_after_assignable(&mut self, assignable: ast::Assignable) -> ParseResult<ast::Statement> {
        if let Some(token) = self.peek_next_token()? {
            match token {
                Token::EQUAL => Ok(self.parse_assign_statement(assignable)?),
                Token::LPAREN => {
                    let func_call = self.parse_function_call(assignable)?;
                    if self.is_eof()? || self.peek_token_is(&Token::SEMICOLON)? {
                        return Ok(ast::Statement::Expression(func_call))
                    }
                    let expression = self.parse_infix(func_call)?;
                    let res = Ok(ast::Statement::Expression(expression));
                    self.skip_optional_token(&Token::SEMICOLON)?;
                    res
                },
                Token::SEMICOLON => {
                    self.next_token()?;
                    Ok(ast::Statement::Expression(ast::Expression::Assignable(assignable)))
                }
                _ => {
                    let expression = self.parse_infix(ast::Expression::Assignable(assignable))?;
                    self.skip_optional_token(&Token::SEMICOLON)?;
                    Ok(ast::Statement::Expression(expression))
                }
            }
        } else {
            Ok(ast::Statement::Expression(ast::Expression::Assignable(assignable)))
        }
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ast::Statement> {
        let expression = self.parse_expression(0)?;
        Ok(ast::Statement::Expression(expression))
    }

    // Precondition: caller must check that current token is a let token
    fn parse_let_statement(&mut self) -> ParseResult<ast::Statement> {
        // Expect the current token to be a let token
        self.expect_token(Token::LET)?;
        // If no token after let keyword or token after let keyword is not an identifier it is an invalid let statement
        if self.is_eof()? {
            return Err(self.error(ParserErrorType::UnexpectedEOF));
        }
        // Now that we know that parse identifier will not panic, i.e., it is followed by a token, we get the identifier token
        let ident = self.parse_identifier()?;
        // Expect assignment operator in let statement
        self.expect_token(Token::EQUAL)?;
        let expression = self.parse_expression(0)?;
        self.expect_token(Token::SEMICOLON)?;
        Ok(ast::Statement::Let {
            ident,
            expression
        })
    }

    // Precondition: caller must ensure that current token is an equal token
    fn parse_assign_statement(&mut self, assignable: ast::Assignable) -> ParseResult<ast::Statement> {
        self.expect_token(Token::EQUAL)?;
        let expression = self.parse_expression(0)?;
        self.expect_token(Token::SEMICOLON)?;
        Ok(ast::Statement::Assign {
            assignable,
            expression
        })
    }

    // Precondition: Caller must only call this if the current token is a while token
    fn parse_while_loop(&mut self) -> ParseResult<ast::Statement> {
        // Expect that the current token is a while token
        self.expect_token(Token::WHILE)?;
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
        self.expect_token(Token::IF)?;
        let cond = self.parse_expression(0)?;
        let body = self.parse_block()?;
        let r#else = if self.skip_optional_token(&Token::ELSE)? {
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
        // Expect current token to be return and move to expression
        self.expect_token(Token::RETURN)?;
        let return_value = if self.peek_token_is(&Token::SEMICOLON)? {
            None
        } else {
            Some(self.parse_expression(0)?)
        };
        self.expect_token(Token::SEMICOLON)?;
        Ok(ast::Statement::Return(return_value))
    }

    // Precondition: Caller must only call this if the current token is an if token
    fn parse_function(&mut self) -> ParseResult<ast::Statement> {
        // Expect current token to be fn 
        self.expect_token(Token::FN)?;
        let ident = self.parse_identifier()?;
        // Expect an open parenthesis before parameters after function name
        self.expect_token(Token::LPAREN)?;
        let mut params = Vec::new();
        // Get identifier list
        while !self.peek_token_is(&Token::RPAREN)? {
            params.push(self.parse_identifier()?);
            if !self.skip_optional_token(&Token::COMMA)? {
                break
            };
        }
        self.expect_token(Token::RPAREN)?;
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
        self.expect_token(Token::LPAREN)?;
        // Get expressions list for function call
        let mut params: Vec<ast::Expression> = Vec::new();
        while !self.peek_token_is(&Token::RPAREN)? {
            params.push(self.parse_expression(0)?);
            if !self.skip_optional_token(&Token::COMMA)? {
                break
            };
        }
        // Param list should end with a close parenthesis otherwise it is an invalid function call
        self.expect_token(Token::RPAREN)?;
        Ok(ast::Expression::FuncCall {
            func,
            params
        })
    }

    fn parse_expression(&mut self, precidence: i8) -> ParseResult<ast::Expression> {
        // invalid expression if it is blank
        if self.is_eof()? {
            return Err(self.error(ParserErrorType::UnexpectedEOF));
        }
        // Return group expression if expression starts with an open paren
        if self.peek_token_is(&Token::LPAREN)? {
            return Ok(self.parse_group_expression()?)
        }
        // Return array literal if expression starts with an open square bracket
        if self.peek_token_is(&Token::LSQR)? {
            return Ok(ast::Expression::Literal(self.parse_array_literal()?))
        }
        if self.peek_token_is(&Token::LSQUIG)? {
            return Ok(ast::Expression::Literal(self.parse_object_literal()?))
        }
        // Get "left side" of the expression
        let mut left_side: ast::Expression = self.parse_prefix()?;
        while !self.is_eof()? {
            // Recursively parse right side of equation and build on expression "left side"
            let op_prec: i8 = Self::token_type_precedence(self.peek_next_token()?.unwrap().clone());
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
        match self.peek_next_token()?.unwrap().clone() {
            Token::IDENTIFIER(_) => {
                let assignable = self.parse_assignable()?;
                if self.peek_token_is(&Token::LPAREN)? {
                    return self.parse_function_call(assignable);
                }
                return Ok(ast::Expression::Assignable(assignable))
            },
            Token::MINUS => {
                // TODO make minus work with assigned values
                self.next_token()?;
                let number = self.parse_number()?;
                if let ast::Literal::NumberLit(num) = number {
                    Ok(ast::Expression::Literal(ast::Literal::NumberLit(-num)))
                } else {
                    unreachable!()
                }
            }
            Token::INT(_) => Ok(ast::Expression::Literal(self.parse_number()?)),
            Token::STRING(_)=> Ok(ast::Expression::Literal(self.parse_string_literal()?)),
            token => {
                Err(self.error(ParserErrorType::UnexpectedTokenOption { 
                    expected_token_options: vec![Token::MINUS, Token::IDENTIFIER(String::new()), Token::INT(0), Token::STRING(String::new())],
                    recieved_token: token.clone()
                }))
            }
        }
    }

    // Precondition: Caller must make sure this is called on an arithmetic token and provide left side of the equation as first parameter
    fn parse_infix(&mut self, left:ast::Expression) -> ParseResult<ast::Expression> {
        let operand_token = self.next_token()?;
        let precidence: i8 = Self::token_type_precedence(operand_token.clone());
        let right = self.parse_expression(precidence)?;
        let operand_token = TokenWithLocation::new(operand_token, self.location.clone());
        let op = match operand_token.try_into() {
            Ok(op) => op,
            Err(err) => {
                let err: ast::TokenNotInfixOpErr = err;
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
        while let Some(token) = self.peek_next_token()? {
            match token {
                Token::LSQR => {
                    // Skip past left square bracker
                    self.next_token()?;
                    // Parse the index into the array
                    let index = Box::from(self.parse_expression(0)?);
                    // Array index is invalid if it is not closed
                    self.expect_token(Token::RSQR)?;
                    assignable = ast::Assignable::ArrayIndex {
                        array: Box::from(assignable), 
                        index
                    }
                },
                Token::DOT => {
                    // Skip past the dot
                    self.next_token()?;
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
        match self.next_token()? {
            Token::IDENTIFIER(ident) => Ok(ident),
            other => Err(self.error(ParserErrorType::UnexpectedToken {
                expected_token: Token::IDENTIFIER(String::new()),
                recieved_token: other
            }))
        }
    }

    // Precondition: Caller must check that current_token is an LSQR
    fn parse_array_literal(&mut self) -> ParseResult<ast::Literal> {
        // Expect the current token to be LSQR
        self.expect_token(Token::LSQR)?;
        let mut values = Vec::new();
        while !self.peek_token_is(&Token::RSQR)? {
            values.push(self.parse_expression(0)?);
            if !self.skip_optional_token(&Token::COMMA)? {
                break
            }
        }
        self.expect_token(Token::RSQR)?;
        Ok(ast::Literal::ArrayLit(values))
    }

    // Precondition: Caller must make sure that current_token is an left squigly
    fn parse_object_literal(&mut self) -> ParseResult<ast::Literal> {
        // Expect current token to be LSQUIG
        self.expect_token(Token::LSQUIG)?;
        let mut fields = Vec::new();
        while !self.peek_token_is(&Token::RSQUIG)? {
            let name = self.parse_identifier()?;
            self.expect_token(Token::COLON)?;
            let value = self.parse_expression(0)?;
            fields.push((name, value));
            // Each property should be delimited by a comma
            if !self.skip_optional_token(&Token::COMMA)? {
                break
            }
        }
        self.expect_token(Token::RSQUIG)?;
        Ok(ast::Literal::ObjectLit(fields))
    }

    // Precondition: Caller must make sure that current_token is a string token
    fn parse_string_literal(&mut self) -> ParseResult<ast::Literal> {
        let token = self.next_token()?;
        if let Token::STRING(string) = token{
            Ok(ast::Literal::StringLit(string))
        } else {
            Err(self.error(ParserErrorType::UnexpectedToken { expected_token: Token::STRING(String::new()), recieved_token: token }))
        }
    }

    // Precondition: Caller must check that current_token is a number
    fn parse_number(&mut self) -> ParseResult<ast::Literal> {
        let token = self.next_token()?;
        if let Token::INT(val) = token.clone() {
            Ok(ast::Literal::NumberLit(val as i64))
        } else {
            Err(self.error(ParserErrorType::UnexpectedToken {
                expected_token: Token::INT(0),
                recieved_token: token
            }))
        }
    }

    // Precondition: Caller must only call this if the current token is an open parenthesis
    fn parse_group_expression(&mut self) -> ParseResult<ast::Expression> {
        // Expect that the current token is LPAREN
        self.expect_token(Token::LPAREN)?;
        let expression = self.parse_expression(0)?;
        // Check that the opened group is closed
        self.expect_token(Token::RPAREN)?;
        // Contruct Group expression
        let mut expression: ast::Expression = ast::Expression::Group(Box::from(expression));
        if self.is_eof()? {
            return Err(self.error(ParserErrorType::UnexpectedEOF));
        }
        self.skip_optional_token(&Token::SEMICOLON)?;
        // If next token is arithmatic then pass in the group expression as the lhs
        if !self.is_eof()? && self.peek_token_is_arithmetic()? {
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

    // throws an unexpected token error if next token does not match expected token, moves the
    // token iterator
    fn expect_token(&mut self, expected_token: Token) -> ParseResult<Token> {
        let token = self.next_token()?;
        if token != expected_token {
            return Err(ParserError { 
                error_type: ParserErrorType::UnexpectedToken { 
                    expected_token,
                    recieved_token: token
                },
                location: self.location.clone()
            })
        }
        Ok(token)
    }

    fn token_type_precedence(token: Token) -> i8 {
        match token {
            Token::EQ |
            Token::NOTEQ => 1,
            Token::LT |
            Token::GT => 2,
            Token::PLUS |
            Token::MINUS => 3,
            Token::ASTERISK |
            Token::SLASH => 4,
            _ => -1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn let_statement() {
        let input = String::from("let i = 12; let greeting = \"hello\";");
        assert_eq!(
            Parser::new(&input).parse_program(),
            Ok(vec![
                ast::Statement::Let {
                    ident: "i".to_string(),
                    expression: ast::Expression::Literal(
                        ast::Literal::NumberLit(12)
                    )
                },
                ast::Statement::Let {
                    ident: "greeting".to_string(),
                    expression: ast::Expression::Literal(
                        ast::Literal::StringLit("hello".to_string())
                    )
                }
            ])
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
