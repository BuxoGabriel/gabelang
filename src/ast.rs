use std::fmt::Debug;
use std::any::Any;

use crate::lexer::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement : Node + Debug {
    fn statement_node(&self) -> Box<dyn Node>;
}

pub trait Expression : Node + Debug {
    fn expression_node(&self) -> Box<dyn Node>;
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        match &self.statements.get(0) {
            Some(statement) => statement.token_literal(),
            None => String::from("")
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub name: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Number {
    pub token: Token,
    pub value: i64
}

impl Node for Number {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Number {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct ArithmaticExpression {
    pub token: Token,
    pub op1: Box<dyn Expression>,
    pub op2: Box<dyn Expression>
}

impl Node for ArithmaticExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for ArithmaticExpression {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct GroupExpression {
    pub open_token: Token,
    pub close_token: Token,
    pub expression: Box<dyn Expression>
}

impl Node for GroupExpression {
    fn token_literal(&self) -> String {
        let mut literal = String::from(&self.open_token.literal);
        literal.push_str(&self.close_token.literal);
        literal
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for GroupExpression {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub expression: Box<dyn Expression>
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct IfStatement {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub then: CodeBlock
}

impl Node for IfStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for IfStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    pub open_token: Token,
    pub close_token: Token,
    pub statements: Vec<Box<dyn Statement>>
}

impl Node for CodeBlock {
    fn token_literal(&self) -> String {
        let mut literal = String::from(&self.open_token.literal);
        literal.push_str(&self.close_token.literal);
        literal
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for CodeBlock {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}
