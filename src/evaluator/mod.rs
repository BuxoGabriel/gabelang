use std::fmt::{ Display, Write };
use std::rc::Rc;
use std::collections::HashMap;

mod built_ins;

use crate::ast::{self, join, Assignable, Expression, InfixOp, Literal, PrefixOp, Statement};

pub struct GabrEnv {
    var_scopes: Vec<HashMap<String, ObjectType>>,
    built_ins: HashMap<String, Rc<dyn built_ins::BuiltIn>>
}

impl GabrEnv {
    pub fn new() -> Self {
        let var_scopes = vec![HashMap::new()];
        let built_ins = built_ins::load_built_ins();
        Self { var_scopes, built_ins }
    }

    pub fn eval_program(&mut self, program: &Vec<ast::Statement>) -> Result<GabrValue, String> {
        let mut result = GabrValue::new(ObjectType::NULL, false);
        self.push_scope();
        for statement in program.iter() {
            result = self.eval_statement(statement)?;
            if result.returning {
                self.pop_scope();
                return Ok(result)
            }
        }
        self.pop_scope();
        Ok(result)
    }

    fn create_func(&mut self, name: String, val: ast::Function) {
        let scope = self.var_scopes.last_mut().expect("Tried to create function but no scopes are available in environment");
        scope.insert(name, ObjectType::FUNCTION(val));
    }

    fn get_built_in(&self, name: String) -> Option<Rc<dyn built_ins::BuiltIn>> {
        self.built_ins.get(&name).map(|bi| bi.clone())
    }

    fn create_var(&mut self, name: String, val: ObjectType) {
        let scope = self.var_scopes.last_mut().expect("Tried to create variable but no scopes are available in environment");
        scope.insert(name, val);
    }

    fn get_var(&self, name: &str) -> Option<&ObjectType> {
        for scope in self.var_scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }

    fn get_var_mut(&mut self, name: &str) -> Option<&mut ObjectType> {
        for scope in self.var_scopes.iter_mut().rev() {
            if let Some(val) = scope.get_mut(name) {
                return Some(val);
            }
        }
        None
    }

    fn set_var(&mut self, name: &str, val: ObjectType) -> Result<(),String> {
        for scope in self.var_scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return Ok(())
            }
        }
        Err("Environment does not contain variable to be altered".to_string())
    }

    fn get_assignable(&mut self, assignable: &Assignable) -> Result<&ObjectType, String> {
        match assignable {
            Assignable::Var(var) => self.get_var(&var).ok_or("Could not find variable in any scope".to_string()),
            Assignable::ArrayIndex { array, index } => {
                let index = self.eval_expression(index)?;
                let index = match index {
                    ObjectType::NUMBER(num) => num,
                    _ => return Err("can not index array with non-number value".to_string())
                };
                let arr = self.get_assignable(array)?;
                if let ObjectType::ARRAY(arr) = arr {
                    return Ok(&arr[index as usize])
                } else {
                    Err("Can not index into non-array value".to_string())
                }
            },
            Assignable::ObjectProp { obj, prop } => {
                let obj = self.get_assignable(obj)?;
                if let ObjectType::OBJECT(obj) = obj {
                    Ok(obj.get(prop).ok_or("Property does not exist on this object".to_string())?)
                } else {
                    Err("Can not get property of non-object value".to_string())
                }
            }
        }
    }

    fn get_assignable_mut(&mut self, assignable: &Assignable) -> Result<&mut ObjectType, String> {
        match assignable {
            Assignable::Var(var) => self.get_var_mut(&var).ok_or("Could not find variable in any scope".to_string()),
            Assignable::ArrayIndex { array, index } => {
                let index = self.eval_expression(index)?;
                let index = match index {
                    ObjectType::NUMBER(num) => num,
                    _ => return Err("can not index array with non-number value".to_string())
                };
                let arr = self.get_assignable_mut(array)?;
                if let ObjectType::ARRAY(arr) = arr {
                    return Ok(&mut arr[index as usize])
                } else {
                    Err("Can not index into non-array value".to_string())
                }
            },
            Assignable::ObjectProp { obj, prop } => {
                let obj = self.get_assignable_mut(obj)?;
                if let ObjectType::OBJECT(obj) = obj {
                    Ok(obj.get_mut(prop).ok_or("Property does not exist on this object".to_string())?)
                } else {
                    Err("Can not get property of non-object value".to_string())
                }
            }
        }
    }

    fn set_assignable(&mut self, assignable: &Assignable, val: ObjectType) -> Result<(), String> {
        match assignable {
            Assignable::Var(var) => self.set_var(&var, val),
            Assignable::ArrayIndex { array, index } => {
                let index = self.eval_expression(index)?;
                let index = match index {
                    ObjectType::NUMBER(num) => num,
                    _ => return Err("Can not index string with non number value".to_string())
                };
                let arr = self.get_assignable_mut(array)?;
                if let ObjectType::ARRAY(arr) = arr {
                    arr[index as usize] = val;
                    Ok(())
                } else {
                    Err("Can not index into non-array value".to_string())
                }
            },
            Assignable::ObjectProp { obj, prop } => {
                let obj = self.get_assignable_mut(obj)?;
                if let ObjectType::OBJECT(obj) = obj {
                    obj.insert(prop.clone(), val).ok_or("Property does not exist on this object".to_string())?;
                    Ok(())
                } else {
                    Err("Can not get property of non-object value".to_string())
                }
            }
        }
    }

    fn push_scope(&mut self) {
        self.var_scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.var_scopes.pop();
    }

    fn load_params(&mut self, params: Vec<(String, Result<ObjectType, String>)>) -> Result<(), String> {
        let mut err = Ok(());
        params.iter().for_each(|(name, val)| {
            match val {
                Ok(val) => self.create_var(name.clone(), val.clone()),
                Err(e) => err = Err(e),
            }
        });
        // If there was a problem evaluating a parameter return its error
        err?;
        Ok(())
    }

    pub fn eval_statement(&mut self, statement: &Statement) -> Result<GabrValue, String> {
        match statement {
            Statement::Expression(expression) => {
                Ok(GabrValue::new(self.eval_expression(expression)?, true))
            },
            Statement::Let { ident, expression } => {
                let val = self.eval_expression(expression)?;
                self.create_var(ident.clone(), val);
                Ok(GabrValue::new(ObjectType::NULL, false))
            },
            Statement::Assign { assignable, expression } => {
                let val = self.eval_expression(expression)?;
                self.set_assignable(assignable, val)?;
                Ok(GabrValue::new(ObjectType::NULL, false))
            },
            Statement::Return(val) => {
                match val.as_ref() {
                    Some(value) => {
                        Ok(GabrValue::new(self.eval_expression(value)?, true))
                    },
                    None => Ok(GabrValue::new(ObjectType::NULL, true))
                }
            },
            Statement::If { cond, body, r#else } => {
                let condition_result = self.eval_expression(cond)?.is_truthy();
                if condition_result {
                    self.eval_program(body)
                } else {
                    match r#else.as_ref() {
                        Some(else_block) => self.eval_program(else_block),
                        None => Ok(GabrValue::new(ObjectType::NULL, false))
                    }
                }
            },
            Statement::While { cond, body } => {
                let mut result = GabrValue::new(ObjectType::NULL, false);
                while self.eval_expression(cond)?.is_truthy() {
                    result = self.eval_program(body)?;
                    if result.returning {
                        return Ok(result)
                    }
                }
                Ok(result)
            },
            Statement::FuncDecl(func) => {
                self.create_func(func.ident.clone(), func.clone());
                Ok(GabrValue::new(ObjectType::NULL, false))
            },
        }
    }

    fn eval_expression(&mut self, expression: &Expression) -> Result<ObjectType, String> {
        match expression {
            Expression::Prefix { op, expression } => {
                self.eval_prefix(*op, expression)
            },
            Expression::Infix { op, left, right } => {
                self.eval_infix(*op, left, right)
            },
            Expression::Group(expression) => {
                self.eval_expression(expression)
            },
            Expression::FuncCall { func, params } => {
                self.eval_function_call(func, params)
            },
            Expression::Assignable(val) => {
                match val {
                    Assignable::Var(var) => {
                        if let Some(val) = self.get_var(var) {
                            Ok(val.clone())
                        } else {
                            Err("Referenced Identifier could not be found".to_string())
                        }
                    },
                    Assignable::ArrayIndex { array, index } => {
                        let index = self.eval_expression(index)?;
                        let index = match index {
                            ObjectType::NUMBER(val) => val,
                            _ => {
                                return Err("Invalid Array Index: Can not index array by non integer value".to_string())
                            }
                        };
                        let arr = self.get_assignable(array)?;
                        if let ObjectType::ARRAY(vals) = arr {
                            match vals.get(index as usize) {
                                Some(val) => Ok(val.clone()),
                                None => Err("Invalid Array Index: Array indexed out of bounds".to_string()),
                            }
                        } else {
                            Err(format!("Variable \"{}\" is not an array and can not be indexed as such", arr))
                        }
                    },
                    Assignable::ObjectProp { obj , prop } => {
                        let obj = self.get_assignable(obj)?;
                        if let ObjectType::OBJECT(obj) = obj {
                            if let Some(prop) = obj.get(prop) {
                                Ok(prop.clone())
                            } else {
                                Ok(ObjectType::NULL)
                            }
                        } else {
                            Err(format!("Variable \"{}\" is not an object and does not have properties", prop))
                        }
                    },
                }
            },
            Expression::Literal(lit) => {
                match lit {
                    Literal::NumberLit(num) => Ok(ObjectType::NUMBER(num.clone())),
                    Literal::ArrayLit(arr) => {
                        // Create an array of evaluated expressions
                        let arr: Vec<Result<ObjectType, String>> = arr.iter().map(|v| self.eval_expression(v)).collect();
                        let mut err = Ok(());
                        arr.iter().for_each(|v| {
                            if let Err(e) = v {
                                *&mut err = Err(e);
                            }
                        });
                        err?;
                        let arr: Vec<ObjectType> = arr.into_iter().map(|v| v.unwrap()).collect();
                        Ok(ObjectType::ARRAY(arr))
                    },
                    Literal::ObjectLit(obj) => {
                        // Create an object
                        let fields: Vec<(String, Result<ObjectType, String>)> = obj.iter().map(|(ident, expression)| (ident.clone(), self.eval_expression(expression))).collect();
                        // Check for error in evaluated fields
                        let mut err = Ok(());
                        fields.iter().for_each(|(_, v)| {
                            if let Err(e) = v {
                                *&mut err = Err(e);
                            }
                        });
                        err?;
                        let fields: HashMap<String, ObjectType> = fields.into_iter().map(|(name, val)| {
                            let val = val.unwrap();
                            (name, val)
                        }).collect();
                        Ok(ObjectType::OBJECT(fields))
                    }
                }
            }
        }
    }

    fn eval_prefix(&mut self, op: PrefixOp, expression: &Expression) -> Result<ObjectType, String> {
        let expression = self.eval_expression(expression)?;
        match op {
            PrefixOp::Bang => {
                if let ObjectType::NUMBER(val) = expression {
                    if val != 0 {
                        Ok(ObjectType::NUMBER(0))
                    } else {
                        Ok(ObjectType::NUMBER(1))
                    }
                } else {
                    Err("Can not perform not operation on non numeric expression".to_string())
                }
            },
            PrefixOp::Neg => {
                if let ObjectType::NUMBER(val) = expression {
                    Ok(ObjectType::NUMBER(-val))
                } else {
                    Err("Can not perform not operation on non numeric expression".to_string())
                }
            }
        }
    }

    fn eval_infix(&mut self, op: InfixOp, left: &Expression, right: &Expression) -> Result<ObjectType, String> {
        let op1 = self.eval_expression(left)?;
        let op1 = match op1 {
            ObjectType::NUMBER(num) => num,
            _ => return Err(format!("Infix Evaluation Failed: left operand did not evaluate to a number, expression: {:?} evaluated to {:?}", left, op1))
        };
        let op2 = self.eval_expression(right)?;
        let op2 = match op2 {
            ObjectType::NUMBER(num) => num,
            _ => return Err(format!("Infix Evaluation Failed: left operand did not evaluate to a number, expression: {:?} evaluated to {:?}", right, op2))
        };
        let val = match op {
            InfixOp::Add => op1 + op2,
            InfixOp::Sub => op1 - op2,
            InfixOp::Mult => op1 * op2,
            InfixOp::Div => op1 / op2,
            InfixOp::Eq => {
                if op1 == op2 {
                    1
                } else {
                    0
                }
            },
            InfixOp::NotEq => {
                if op1 != op2 {
                    1
                } else {
                    0
                }
            }
            InfixOp::Gt => {
                if op1 > op2 {
                    1
                } else {
                    0
                }
            },
            InfixOp::Lt => {
                if op1 < op2 {
                    1
                } else {
                    0
                }
            }
        };
        Ok(ObjectType::NUMBER(val))
    }

    fn eval_function_call(&mut self, func_locator: &Assignable, params: &Vec<Expression>) -> Result<ObjectType, String> {
        // look in all scopes for a function that matches function name
        let func = self.get_assignable(func_locator).ok();
        if let Some(func) = func {
            let func = match func {
                ObjectType::FUNCTION(func) => func.clone(),
                _ => return Err("Can not call non-function value as function".to_string())
            };
            // Create (parameter, value) list
            let params: Vec<(String, Result<ObjectType, String>)> = func.params.iter()
                .map(|param| param.clone())
                .zip(params.iter().map(|param| self.eval_expression(param)))
                .collect();
            // Create new scope for parameters
            self.push_scope();
            self.load_params(params)?;
            // Evaluate the body of the function with the new context
            let result = self.eval_program(&func.body)?;
            // Remove param variables scope
            self.pop_scope();
            // Function call should not automatically be interpretted as return funcCall(param);
            Ok(result.gabr_object)
        } else {
            let func_name = match func_locator {
                Assignable::Var(ident) => ident,
                _ => return Err("Function call failed to evaluate: function could not be found".to_string()),
            };
            if let Some(built_in) = self.get_built_in(func_name.clone()) {
                let params: Vec<(String, Result<ObjectType, String>)> = built_in.get_params().iter()
                    .map(|param| param.clone())
                    .zip(params.iter().map(|param| self.eval_expression(param)))
                    .collect();
                // Create new scope for parameters
                self.push_scope();
                self.load_params(params)?;
                // Evaluate built in function in new context
                let result = built_in.eval(self)?;
                // Remove param variables scope
                self.pop_scope();
                // Function call should not automatically be interpretted as return funcCall(param);
                Ok(result)
            } else {
                Err(format!("Function {} could not be found", func_name))
            }
        }
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
}

impl Display for GabrValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.gabr_object)
    }
}

#[derive(Clone, Debug)]
enum ObjectType {
    NUMBER(i64),
    ARRAY(Vec<ObjectType>),
    FUNCTION(ast::Function),
    OBJECT(HashMap<String, ObjectType>),
    NULL
}

impl ObjectType {
    fn is_truthy(&self) -> bool {
        match self {
            Self::NUMBER(val) => *val != 0,
            Self::NULL => false,
            _ => true
        }
    }
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NUMBER(val) => write!(f, "{val}"),
            Self::ARRAY(vals) => {
                write!(f, "[{}]", join(vals, ", "))
            },
            Self::OBJECT(obj) => {
                let properties = obj.iter().map(|(key, val)| {
                    let mut output = String::new();
                    write!(output, "\t{key}: {val}");
                    output
                }).collect::<Vec<String>>();
                write!(f, "{{\n{}\n}}", join(&properties, "\n"))
            },
            Self::FUNCTION(func) => {
                write!(f, "(Function: {})", func)
            },
            Self::NULL => f.write_str("null")
        }
    }
}
