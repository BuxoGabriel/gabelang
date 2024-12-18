use std::fmt::Debug;
use std::any::Any;
use std::rc::Rc;

use crate::evaluator::{self, GabrEnv, GabrValue};
use crate::lexer::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String>;
}

pub trait Statement : Node + Debug {}

pub trait Expression : Node + Debug {}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Rc<dyn Statement>>,
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

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_program(env, self)
    }
}

#[derive(Debug, Clone)]
pub struct CodeBlock {
    pub open_token: Token,
    pub close_token: Token,
    pub statements: Vec<Rc<dyn Statement>>
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

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_codeblock(env, self)
    }
}

impl Statement for CodeBlock {}

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

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_infix(env, self)
    }
}

impl Expression for InfixExpression {}

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

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        self.expression.eval(env)
    }
}

impl Expression for GroupExpression {}

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

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        self.expression.eval(env)
    }
}

impl Statement for ExpressionStatement {}

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

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_let_statement(env, self)
    }
}

impl Statement for LetStatement {}

#[derive(Debug)]
pub struct AssignStatement {
    pub ident: Identifier,
    pub expression: Box<dyn Expression>
}

impl Node for AssignStatement {
    fn token_literal(&self) -> String {
        self.ident.token_literal()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.ident.to_string();
        output.push_str(" = ");
        output.push_str(&self.expression.to_string());
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_assign_statement(env, self)
    }
}

impl Statement for AssignStatement {}

#[derive(Debug)]
pub struct WhileLoop {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub body: CodeBlock
}

impl Node for WhileLoop {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.token_literal();
        output.push_str(&self.condition.to_string());
        output.push(' ');
        output.push_str(&self.body.to_string());
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_while_loop(env, self)
    }
}

impl Statement for WhileLoop {}

#[derive(Debug)]
pub struct IfStatement {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub then_block: CodeBlock,
    pub else_block: Option<CodeBlock>
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
        output.push_str(&self.then_block.to_string());
        if let Some(else_block) = self.else_block.as_ref() {
            output.push_str("else ");
            output.push_str(&else_block.to_string());
        }
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_if_statement(env, self)
    }
}

impl Statement for IfStatement {}

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

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_return_statement(env, self)
    }
}

impl Statement for ReturnStatement {}

#[derive(Debug, Clone)]
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
        output.push_str(&self.params.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", "));
        output.push_str(") ");
        output.push_str(&self.body.to_string());
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_function(env, self)
    }
}

impl Statement for Function {}

#[derive(Debug)]
pub struct FunctionCall {
    pub ident: Identifier,
    pub params: Vec<Box<dyn Expression>>
}

impl Node for FunctionCall {
    fn token_literal(&self) -> String {
        self.ident.token_literal()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.token_literal();
        output.push('(');
        output.push_str(&self.params.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", "));
        output.push(')');
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_function_call(env, self)
    }
}

impl Expression for FunctionCall {}

#[derive(Debug, Clone)]
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

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_identifier(env, self)
    }
}

impl Expression for Identifier {}

#[derive(Debug)]
pub struct ArrayLiteral {
    pub open_token: Token,
    pub close_token: Token,
    pub values: Vec<Box<dyn Expression>>
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.open_token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.open_token.literal.clone();
        output.push_str(&self.values.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", "));
        output.push_str(&self.close_token.literal.clone());
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_array_literal(env, self)
    }
}

impl Expression for ArrayLiteral {}

#[derive(Debug)]
pub struct ObjectLiteral {
    pub open_token: Token,
    pub close_token: Token,
    pub fields: Vec<(Identifier, Box<dyn Expression>)>
}

impl Node for ObjectLiteral {
    fn token_literal(&self) -> String {
        let mut output = self.open_token.literal.clone();
        output.push_str(&self.close_token.literal.clone());
        output
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.open_token.literal.clone();
        output.push_str(
            &self.fields.iter().map(|(ident, value)| {
                let mut output = ident.name.clone();
                output.push_str(": ");
                output.push_str(&value.to_string());
                output
            }).collect::<Vec<String>>().join(" ")
        );
        output.push_str(&self.close_token.literal.clone());
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_object_literal(env, self)
    }
}

impl Expression for ObjectLiteral {}

#[derive(Debug)]
pub struct ArrayIndex {
    pub ident: Identifier,
    pub index: Box<dyn Expression>
}

impl Node for ArrayIndex {
    fn token_literal(&self) -> String {
        self.ident.token_literal()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.ident.to_string();
        output.push('[');
        output.push_str(&self.index.to_string());
        output.push(']');
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_array_index(env, self)
    }
}

impl Expression for ArrayIndex {}

#[derive(Debug)]
pub struct ObjectProperty {
    pub ident: Identifier,
    pub property: Identifier
}

impl Node for ObjectProperty {
    fn token_literal(&self) -> String {
        self.ident.token_literal()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = self.ident.to_string();
        output.push('.');
        output.push_str(&self.property.to_string());
        output
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_object_property(env, self)
    }
}

impl Expression for ObjectProperty {}

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

    fn eval(&self, _: &mut GabrEnv) -> Result<GabrValue, String> {
        evaluator::eval_number_literal(self)
    }
}

impl Expression for Number {}
