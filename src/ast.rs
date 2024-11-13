use std::fmt::Debug;
use std::any::Any;

use crate::evaluator::{self, ObjectType};
use crate::lexer::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
    fn eval(&self) -> Result<ObjectType, String>;
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

impl Node for Program {
    fn token_literal(&self) -> String {
        match &self.statements.get(0) {
            Some(statement) => statement.token_literal(),
            None => String::from("")
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = String::new();
        for statement in self.statements.iter() {
            output.push_str(&statement.to_string())
        }
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        evaluator::eval_program(self)
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

    fn to_string(&self) -> String {
        self.name.clone()
    }

    fn eval(&self) -> Result<ObjectType, String> {
        evaluator::eval_identifier(self)
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

    fn to_string(&self) -> String {
        self.value.to_string()
    }

    fn eval(&self) -> Result<ObjectType, String> {
        evaluator::eval_number_literal(self)
    }
}

impl Expression for Number {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub op1: Box<dyn Expression>,
    pub op2: Box<dyn Expression>
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = String::from('(');
        output.push_str(&self.op1.to_string());
        output.push(' ');
        output.push_str(&self.token_literal());
        output.push(' ');
        output.push_str(&self.op2.to_string());
        output.push(')');
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        evaluator::eval_infix(self)
    }
}

impl Expression for InfixExpression {
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

    fn to_string(&self) -> String {
        let mut output = String::from('(');
        output.push_str(&self.expression.to_string());
        output.push(')');
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        self.expression.eval()
    }
}

impl Expression for GroupExpression {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Box<dyn Expression>
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.expression.token_literal()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.expression.to_string();
        output.push(';');
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        self.expression.eval()
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) -> Box<dyn Node> {
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

    fn to_string(&self) -> String {
        let mut output = self.token_literal();
        output.push(' ');
        output.push_str(&self.identifier.to_string());
        output.push_str(" = ");
        output.push_str(&self.expression.to_string());
        output.push_str(";\n");
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        Ok(ObjectType::NULL)
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

    fn to_string(&self) -> String {
        let mut output = self.token_literal();
        output.push('(');
        output.push_str(&self.condition.to_string());
        output.push_str(") ");
        output.push_str(&self.then.to_string());
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        evaluator::eval_if_statement(self)
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
    fn to_string(&self) -> String {
        let mut output = self.token_literal();
        if let Some(return_value) = &self.return_value {
            output.push(' ');
            output.push_str(&return_value.to_string());
        }
        output.push_str(";\n");
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        Ok(ObjectType::NULL)
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Function {
    pub token: Token,
    pub ident: Identifier,
    pub params: Vec<Identifier>,
    pub body: CodeBlock
}

impl Node for Function {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.token_literal();
        output.push(' ');
        output.push_str(&self.ident.to_string());
        output.push('(');
        for param in self.params.iter() {
            output.push_str(&param.to_string());
        }
        output.push_str(") ");
        output.push_str(&self.body.to_string());
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        Ok(ObjectType::NULL)
    }
}

impl Statement for Function {
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

    fn to_string(&self) -> String {
        let mut output = String::from("{\n");
        for statement in self.statements.iter() {
            output.push_str("    ");
            output.push_str(&statement.to_string());
        }
        output.push_str("}\n");
        output
    }

    fn eval(&self) -> Result<ObjectType, String> {
        evaluator::eval_codeblock(self)
    }
}

impl Statement for CodeBlock {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}
