use crate::ast::{self, Node};

pub struct GabrValue {
    gabr_type: ObjectType,
    returning: bool,
}

impl GabrValue {
    fn new(gabr_type: ObjectType, returning: bool) -> Self {
        Self { gabr_type, returning }
    }

    pub fn to_string(&self) -> Option<String> {
        self.gabr_type.to_string()
    }
}

enum ObjectType {
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

pub fn eval_program(program: &ast::Program) -> Result<GabrValue, String> {
    let mut result = GabrValue::new(ObjectType::NULL, false);
    for statement in program.statements.iter() {
        result = statement.eval()?;
        if result.returning {
            return Ok(result)
        }
    }
    Ok(result)
}

pub fn eval_codeblock(block: &ast::CodeBlock) -> Result<GabrValue, String> { 
    let mut result = GabrValue::new(ObjectType::NULL, false);
    for statement in block.statements.iter() {
        result = statement.eval()?;
        if result.returning {
            return Ok(result)
        }
    }
    Ok(result)
}

pub fn eval_if_statement(if_state: &ast::IfStatement) -> Result<GabrValue, String> {
    let condition_result = match if_state.condition.eval()?.gabr_type {
        ObjectType::NUMBER(res) => res != 0,
        _ => {
            return Err("If Statement evaluation error: If statement could not evaluate condition to a literal value".to_string());
        }
    };
    if condition_result {
        if_state.then_block.eval()
    } else {
        match if_state.else_block.as_ref() {
            Some(else_block) => else_block.eval(),
            None => Ok(GabrValue::new(ObjectType::NULL, false))
        }
    }
}

pub fn eval_return_statement(return_state: &ast::ReturnStatement) -> Result<GabrValue, String> {
    match return_state.return_value.as_ref() {
        Some(value) => {
            let mut value = value.eval()?;
            value.returning = true;
            Ok(value)
        },
        None => Ok(GabrValue::new(ObjectType::NULL, true))
    }
}

pub fn eval_infix(infix: &ast::InfixExpression) -> Result<GabrValue, String> {
    let op1 = match infix.op1.eval()?.gabr_type {
        ObjectType::NUMBER(num) => num,
        _ => return Err("Infix Evaluation Failed: Operand 1 did not evaluate to a number".to_string())
    };
    let op2 = match infix.op2.eval()?.gabr_type {
        ObjectType::NUMBER(num) => num,
        _ => return Err("Infix Evaluation Failed: Operand 2 did not evaluate to a number".to_string())
    };
    let val = match infix.token.literal.as_str() {
        "+" => op1 + op2,
        "-" => op1 - op2,
        "*" => op1 * op2,
        "/" => op1 / op2,
        "==" => {
            if op1 == op2 {
                1
            } else {
                0
            }
        },
        "!=" => {
            if op1 != op2 {
                1
            } else {
                0
            }
        }
        ">" => {
            if op1 > op2 {
                1
            } else {
                0
            }
        },
        "<" => {
            if op1 < op2 {
                1
            } else {
                0
            }
        }
        _ => {
            return Err("Infix Evaluation Failed: Infix operation literal not a recognized".to_string());
        }
    };
    Ok(GabrValue::new(ObjectType::NUMBER(val), false))
}

pub fn eval_number_literal(num_lit: &ast::Number) -> Result<GabrValue, String> {
    Ok(GabrValue::new(ObjectType::NUMBER(num_lit.value), false))
}

pub fn eval_not_implemented() -> Result<GabrValue, String> {
    Err("Evaluation not yet implemented".to_string())
}
