use std::fmt::Debug;

use crate::lexer::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
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
    pub fn token_literal(&self) -> &str {
        match &self.statements.get(0) {
            Some(statement) => statement.token_literal(),
            None => ""
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub name: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
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
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for Number {
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
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}
