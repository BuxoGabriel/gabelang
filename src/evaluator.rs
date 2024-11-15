use std::collections::HashMap;

use crate::ast::{self, Node};

pub struct GabrEnv {
    var_scopes: Vec<HashMap<String, GabrValue>>,
    func_scopes: Vec<HashMap<String, ast::Function>>
}

impl GabrEnv {
    pub fn new() -> Self {
        let var_scopes = vec![HashMap::new()];
        let func_scopes = vec![HashMap::new()];
        Self { var_scopes, func_scopes }
    }

    fn create_func(&mut self, name: String, val: ast::Function) {
        let scope = self.func_scopes.last_mut().expect("Tried to create function but no scopes are available in environment");
        scope.insert(name, val);
    }

    fn get_func(&self, name: String) -> Option<ast::Function> {
        for scope in self.func_scopes.iter().rev() {
            if let Some(val) = scope.get(&name) {
                return Some(val.clone());
            }
        }
        None
    }

    fn create_var(&mut self, name: String, val: GabrValue) {
        let scope = self.var_scopes.last_mut().expect("Tried to create variable but no scopes are available in environment");
        scope.insert(name, val);
    }

    fn set_var(&mut self, name: String, val: GabrValue) -> Result<(),String> {
        for scope in self.var_scopes.iter_mut().rev() {
            if scope.contains_key(&name) {
                scope.insert(name.clone(), val);
                return Ok(())
            }
        }
        Err("Environment does not contain variable to be altered".to_string())
    }

    fn get_var(&self, name: String) -> Option<&GabrValue> {
        for scope in self.var_scopes.iter().rev() {
            if let Some(val) = scope.get(&name) {
                return Some(val);
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.var_scopes.push(HashMap::new());
        self.func_scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.var_scopes.pop();
        self.func_scopes.pop();
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
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

pub fn eval_program(env: &mut GabrEnv, program: &ast::Program) -> Result<GabrValue, String> {
    let mut result = GabrValue::new(ObjectType::NULL, false);
    for statement in program.statements.iter() {
        result = statement.eval(env)?;
        if result.returning {
            return Ok(result)
        }
    }
    Ok(result)
}

pub fn eval_codeblock(env: &mut GabrEnv, block: &ast::CodeBlock) -> Result<GabrValue, String> { 
    let mut result = GabrValue::new(ObjectType::NULL, false);
    env.push_scope();
    for statement in block.statements.iter() {
        result = statement.eval(env)?;
        if result.returning {
            env.pop_scope();
            return Ok(result)
        }
    }
    env.pop_scope();
    Ok(result)
}

pub fn eval_function(env: &mut GabrEnv, func: &ast::Function) -> Result<GabrValue, String> {
    env.create_func(func.ident.name.clone(), func.clone());
    Ok(GabrValue::new(ObjectType::NULL, false))
}

pub fn eval_let_statement(env: &mut GabrEnv, let_state: &ast::LetStatement) -> Result<GabrValue, String> {
    let val = let_state.expression.eval(env)?;
    env.create_var(let_state.identifier.name.clone(), val);
    Ok(GabrValue::new(ObjectType::NULL, false))
}

pub fn eval_if_statement(env: &mut GabrEnv, if_state: &ast::IfStatement) -> Result<GabrValue, String> {
    let condition_result = match if_state.condition.eval(env)?.gabr_type {
        ObjectType::NUMBER(res) => res != 0,
        _ => {
            return Err("If Statement evaluation error: If statement could not evaluate condition to a literal value".to_string());
        }
    };
    if condition_result {
        if_state.then_block.eval(env)
    } else {
        match if_state.else_block.as_ref() {
            Some(else_block) => else_block.eval(env),
            None => Ok(GabrValue::new(ObjectType::NULL, false))
        }
    }
}

pub fn eval_return_statement(env: &mut GabrEnv, return_state: &ast::ReturnStatement) -> Result<GabrValue, String> {
    match return_state.return_value.as_ref() {
        Some(value) => {
            let mut value = value.eval(env)?;
            value.returning = true;
            Ok(value)
        },
        None => Ok(GabrValue::new(ObjectType::NULL, true))
    }
}

pub fn eval_infix(env: &mut GabrEnv, infix: &ast::InfixExpression) -> Result<GabrValue, String> {
    let op1 = match infix.op1.eval(env)?.gabr_type {
        ObjectType::NUMBER(num) => num,
        _ => return Err("Infix Evaluation Failed: Operand 1 did not evaluate to a number".to_string())
    };
    let op2 = match infix.op2.eval(env)?.gabr_type {
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

pub fn eval_function_call(env: &mut GabrEnv, func_call: &ast::FunctionCall) -> Result<GabrValue, String> {
    // Get name of function
    let func_name = func_call.ident.name.clone();
    // look in all scopes for a function that matches function name
    let func = match env.get_func(func_name.clone()) {
        Some(func) => func,
        None => {
            return Err(format!("Function \"{}\" could not be found", func_name));
        }
    };
    // Create new scope for parameters
    env.push_scope();
    // Add every parameter to new scope
    let params: Vec<(String, Result<GabrValue, String>)> = func.params.iter()
        .map(|param| param.name.clone())
        .zip(func_call.params.iter().map(|param| param.eval(env)))
        .collect();
    let mut err = Ok(());
    params.iter().for_each(|(name, val)| {
        match val {
            Ok(val) => env.create_var(name.clone(), val.clone()),
            Err(e) => err = Err(e),
        }
    });
    // If there was a problem evaluating a parameter return its error
    err?;
    // Evaluate the body of the function with the new context
    let mut result = func.body.eval(env)?;
    // Remove param variables scope
    env.pop_scope();
    // Function call should not automatically be interpretted as return funcCall(param);
    result.returning = false;
    Ok(result)
}

pub fn eval_identifier(env: &GabrEnv, ident: &ast::Identifier) -> Result<GabrValue, String> {
    match env.get_var(ident.name.clone()) {
        Some(val) => Ok(val.clone()),
        None => Err("Referenced Identifier could not be found".to_string())
    }
}

pub fn eval_number_literal(num_lit: &ast::Number) -> Result<GabrValue, String> {
    Ok(GabrValue::new(ObjectType::NUMBER(num_lit.value), false))
}

pub fn eval_not_implemented() -> Result<GabrValue, String> {
    Err("Evaluation not yet implemented".to_string())
}
