use std::collections::HashMap;

use crate::ast::{self, Node};

pub struct GabrEnv {
    var_scopes: Vec<HashMap<String, GabrValue>>,
}

impl GabrEnv {
    pub fn new() -> Self {
        let var_scopes = vec![HashMap::new()];
        Self { var_scopes }
    }

    fn create_func(&mut self, name: String, val: ast::Function) {
        let scope = self.var_scopes.last_mut().expect("Tried to create function but no scopes are available in environment");
        scope.insert(name, GabrValue::new(ObjectType::FUNCTION(val), false));
    }

    fn get_func(&self, name: String) -> Option<ast::Function> {
        for scope in self.var_scopes.iter().rev() {
            if let Some(val) = scope.get(&name) {
                match val.gabr_object.clone() {
                    ObjectType::FUNCTION(func) => {
                        return Some(func);
                    },
                    _ => {
                        return None;
                    }
                }
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
    }

    fn pop_scope(&mut self) {
        self.var_scopes.pop();
    }
}

#[derive(Clone)]
pub struct GabrValue {
    gabr_object: ObjectType,
    returning: bool,
}

impl GabrValue {
    fn new(gabr_object: ObjectType, returning: bool) -> Self {
        Self { gabr_object, returning }
    }

    pub fn to_string(&self) -> Option<String> {
        self.gabr_object.to_string()
    }
}

#[derive(Clone)]
enum ObjectType {
    NUMBER(i64),
    ARRAY(Vec<ObjectType>),
    FUNCTION(ast::Function),
    OBJECT(HashMap<String, ObjectType>),
    NULL
}

impl ObjectType {
    pub fn to_string(&self) -> Option<String> {
        match self {
            Self::NUMBER(val) => Some(val.to_string()),
            Self::ARRAY(vals) => {
                let mut output = String::from("[");
                output.push_str(&vals.iter().map(|v| {
                    match v.to_string() {
                        Some(s) => s,
                        None => "null".to_string()
                    }
                }).collect::<Vec<String>>().join(", "));
                output.push(']');
                Some(output)
            },
            Self::OBJECT(obj) => {
                let mut output = "{\n".to_string();
                for (key, val) in obj.iter() {
                    output.push('\t');
                    output.push_str(key);
                    output.push_str(": ");
                    let value_string = match val.to_string() {
                        Some(val) => val,
                        None => "undefined".to_string()
                    };
                    output.push_str(&value_string);
                    output.push('\n');
                }
                output.push('}');
                Some(output)
            },
            Self::FUNCTION(func) => {
                Some(format!("(Function: {})", func.ident.name))
            },
            Self::NULL => None
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Self::NUMBER(val) => *val != 0,
            Self::NULL => false,
            _ => true
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

pub fn eval_while_loop(env: &mut GabrEnv, while_loop: &ast::WhileLoop) -> Result<GabrValue, String> {
    let mut result = GabrValue::new(ObjectType::NULL, false);
    while while_loop.condition.eval(env)?.gabr_object.is_truthy() {
        result = while_loop.body.eval(env)?;
        if result.returning {
            return Ok(result)
        }
    }
    Ok(result)
}

pub fn eval_let_statement(env: &mut GabrEnv, let_state: &ast::LetStatement) -> Result<GabrValue, String> {
    let val = let_state.expression.eval(env)?;
    env.create_var(let_state.identifier.name.clone(), val);
    Ok(GabrValue::new(ObjectType::NULL, false))
}

pub fn eval_assign_statement(env: &mut GabrEnv, set_state: &ast::AssignStatement) -> Result<GabrValue, String> {
    let val = set_state.expression.eval(env)?;
    env.set_var(set_state.ident.name.clone(), val)?;
    Ok(GabrValue::new(ObjectType::NULL, false))
}

pub fn eval_if_statement(env: &mut GabrEnv, if_state: &ast::IfStatement) -> Result<GabrValue, String> {
    let condition_result = if_state.condition.eval(env)?.gabr_object.is_truthy();
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
    let op1 = match infix.op1.eval(env)?.gabr_object {
        ObjectType::NUMBER(num) => num,
        _ => return Err("Infix Evaluation Failed: Operand 1 did not evaluate to a number".to_string())
    };
    let op2 = match infix.op2.eval(env)?.gabr_object {
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

pub fn eval_array_literal(env: &mut GabrEnv, array_lit: &ast::ArrayLiteral) -> Result<GabrValue, String> {
    // Create an array of evaluated expressions
    let arr: Vec<Result<GabrValue, String>> = array_lit.values.iter().map(|v| v.eval(env)).collect();
    let mut err = Ok(());
    arr.iter().for_each(|v| {
        if let Err(e) = v {
            *&mut err = Err(e);
        }
    });
    err?;
    let arr: Vec<ObjectType> = arr.into_iter().map(|v| v.unwrap().gabr_object).collect();
    Ok(GabrValue::new(ObjectType::ARRAY(arr), false))
}

pub fn eval_object_literal(env: &mut GabrEnv, object_lit: &ast::ObjectLiteral) -> Result<GabrValue, String> {
    todo!()
}

pub fn eval_array_index(env: &mut GabrEnv, array_index: &ast::ArrayIndex) -> Result<GabrValue, String> {
    let arr = match env.get_var(array_index.ident.name.clone()) {
        Some(arr) => arr,
        None => {
            return Err(format!("Array \"{}\" could not be found", array_index.ident.name));
        }
    };
    if let ObjectType::ARRAY(vals) = arr.gabr_object.clone() {
        let index = array_index.index.eval(env)?;
        let index = match index.gabr_object {
            ObjectType::NUMBER(val) => val,
            _ => {
                return Err("Can not index array by non integer value".to_string())
            }
        };
        match vals.get(index as usize) {
            Some(val) => Ok(GabrValue::new(val.clone(), false)),
            None => Ok(GabrValue::new(ObjectType::NULL, false)),
        }
    } else {
        Err(format!("Variable \"{}\" is not an array and can not be indexed as such", array_index.ident.name))
    }
}

pub fn eval_number_literal(num_lit: &ast::Number) -> Result<GabrValue, String> {
    Ok(GabrValue::new(ObjectType::NUMBER(num_lit.value), false))
}
