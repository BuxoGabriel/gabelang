use crate::ast::{self, Node};

pub enum ObjectType {
    NUMBER(i64),
    NULL
}

impl ObjectType {
    pub fn to_string(&self) -> Option<String> {
        match self {
            Self::NUMBER(num) => Some(num.to_string()),
            Self::NULL => None
        }
    }
}

pub fn eval(node: Box<dyn ast::Node>) -> Result<ObjectType, String> {
    node.eval()
}

pub fn eval_number_literal(num_lit: &ast::Number) -> Result<ObjectType, String> {
    Ok(ObjectType::NUMBER(num_lit.value))
}

pub fn eval_identifier(_ident: &ast::Identifier) -> Result<ObjectType, String> {
    Err("Eval identifier not implemented".to_string())
}

pub fn eval_infix(infix: &ast::InfixExpression) -> Result<ObjectType, String> {
    let op1 = match infix.op1.eval()? {
        ObjectType::NUMBER(num) => num,
        _ => return Err("Infix Evaluation Failed: Operand 1 did not evaluate to a number".to_string())
    };
    let op2 = match infix.op2.eval()? {
        ObjectType::NUMBER(num) => num,
        _ => return Err("Infix Evaluation Failed: Operand 2 did not evaluate to a number".to_string())
    };
    match infix.token.literal.as_str() {
        "+" => Ok(ObjectType::NUMBER(op1 + op2)),
        "-" => Ok(ObjectType::NUMBER(op1 - op2)),
        "*" => Ok(ObjectType::NUMBER(op1 * op2)),
        "/" => Ok(ObjectType::NUMBER(op1 / op2)),
        "==" => {
            if op1 == op2 {
                Ok(ObjectType::NUMBER(1))
            } else {
                Ok(ObjectType::NUMBER(0))
            }
        },
        "!=" => {
            if op1 != op2 {
                Ok(ObjectType::NUMBER(1))
            } else {
                Ok(ObjectType::NUMBER(0))
            }
        }
        ">" => {
            if op1 > op2 {
                Ok(ObjectType::NUMBER(1))
            } else {
                Ok(ObjectType::NUMBER(0))
            }
        },
        "<" => {
            if op1 < op2 {
                Ok(ObjectType::NUMBER(1))
            } else {
                Ok(ObjectType::NUMBER(0))
            }
        }
        _ => Err("Infix Evaluation Failed: Infix operation literal not a recognized".to_string())
    }
}

pub fn eval_if_statement(if_state: &ast::IfStatement) -> Result<ObjectType, String> {
    let condition_result = match if_state.condition.eval()? {
        ObjectType::NUMBER(res) => res != 0,
        _ => {
            return Err("If Statement evaluation error: If statement could not evaluate condition to a literal value".to_string());
        }
    };
    if condition_result {
        if_state.then.eval()
    } else {
        Ok(ObjectType::NULL)
    }
}

pub fn eval_codeblock(block: &ast::CodeBlock) -> Result<ObjectType, String> {
    let mut result = Ok(ObjectType::NULL);
    for statement in block.statements.iter() {
        result = statement.eval();
    }
    result
}

pub fn eval_program(program: &ast::Program) -> Result<ObjectType, String> {
    let mut result = Ok(ObjectType::NULL);
    for statement in program.statements.iter() {
        result = statement.eval();
    }
    result
}
