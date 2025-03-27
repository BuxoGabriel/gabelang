use std::error::Error;
use std::fmt::{ Display, Write };
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::cell::{RefCell, RefMut};
use std::collections::{HashMap, HashSet};

mod built_ins;
pub mod stack;

use stack::{Stack, StackError};
use crate::ast::{self, join, Assignable, Expression, InfixOp, Literal, PrefixOp, Statement};

#[derive(Debug)]
enum RuntimeError {
    StackError(StackError),
    InvalidArrayIndex,
    InvalidObjectIndex,
    InvalidIndexTarget,
    InvalidPropTarget
}

impl From<StackError> for RuntimeError {
    fn from(err: StackError) -> Self {
        Self::StackError(err)
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for RuntimeError {}

type RuntimeResult<T> = Result<T, RuntimeError>;

/// An isolated runtime environment for a gabelang program
pub struct Runtime {
    global_stack: Stack,
    built_ins: HashMap<String, Rc<dyn built_ins::BuiltIn>>
}

impl Runtime {
    /// Creates a new environment with no variables set and with built_in functions like len
    pub fn new() -> Self {
        let global_stack = Stack::new();
        let built_ins = built_ins::load_built_ins();
        Self { global_stack, built_ins }
    }

    /// Runs a gabelang program from an ast that must be generated by the gabelang Parser 
    ///
    /// When evaluating a program using eval_program_with_new_scope it also creates a new stack frame in order to isolate all variables declared in the program
    ///
    /// This means that if it is called multiple times it will "forget" all variables and functions
    /// declared in previous eval_program calls.
    ///
    /// To run a program without creating a new stack frame look for [Self::eval_program]
    ///
    /// To run a statement without creating a new stack frame look for [Self::eval_statement]
    pub fn eval_program_with_new_scope(&mut self, program: &Vec<ast::Statement>) -> RuntimeResult<GabrValue> {
        let mut result = GabrValue::new(ObjectInner::NULL.as_object(), false);
        self.global_stack.push_scope();
        for statement in program.iter() {
            result = self.eval_statement(statement)?;
            if result.returning {
                self.global_stack.pop_scope()?;
                return Ok(result)
            }
        }
        self.global_stack.pop_scope()?;
        Ok(result)
    }

    /// Runs a gabelang program from an ast that must be generated by the gabelang Parser 
    ///
    /// When evaluating a program using eval_program it uses the existing stack frame so it will
    /// "remember" previous programs run
    ///
    /// To run a program in a new stack frame look for [Self::eval_program_with_new_scope]
    ///
    /// To run a single statement look for [Self::eval_statement]
    pub fn eval_program(&mut self, program: &Vec<ast::Statement>) -> RuntimeResult<GabrValue> {
        let mut result = GabrValue::new(ObjectInner::NULL.as_object(), false); 
        for statement in program.iter() {
            result = self.eval_statement(statement)?;
            if result.returning {
                return Ok(result)
            }
        }
        Ok(result)
    }

    fn get_built_in(&self, name: String) -> Option<Rc<dyn built_ins::BuiltIn>> {
        self.built_ins.get(&name).map(|bi| bi.clone())
    }

    fn load_params(&mut self, params: Vec<(String, Result<Object, String>)>) -> Result<(), String> {
        todo!()
    }

    fn get_assignable(&mut self, assignable: &Assignable) -> RuntimeResult<Object> {
        match assignable {
            Assignable::Var(var) => Ok(self.global_stack.get_var(&var)?),
            Assignable::PropIndex { obj, index } => {
                let index = self.eval_expression(index)?;
                let index: &ObjectInner = &index.inner();
                match &mut self.get_assignable(obj)?.inner() as &mut ObjectInner {
                    ObjectInner::ARRAY(arr) => {
                        if let ObjectInner::NUMBER(num) = index {
                            let num = *num as usize;
                            if num < 0 {
                                Err(RuntimeError::InvalidArrayIndex)
                            } else if (num) < arr.len() {
                                Ok(arr[num].clone())
                            } else {
                                Err(RuntimeError::InvalidArrayIndex)
                            }
                        } else {
                            Err(RuntimeError::InvalidArrayIndex)
                        }
                    },
                    ObjectInner::OBJECT(obj) => {
                        if let ObjectInner::STRING(prop) = index {
                            Ok(obj.get(prop).map(|obj| obj.clone()).unwrap_or(ObjectInner::NULL.as_object()))
                        } else {
                            Err(RuntimeError::InvalidObjectIndex)
                        }
                    },
                    _ => Err(RuntimeError::InvalidIndexTarget)
                }
            },
            Assignable::ObjectProp { obj, prop } => {
                let obj = self.get_assignable(obj)?;
                let obj: &ObjectInner = &obj.inner();
                if let ObjectInner::OBJECT(obj) = obj {
                    // Undefined object properties are retrieved as NULL
                    Ok(obj.get(prop).map(|obj| obj.clone()).unwrap_or(ObjectInner::NULL.as_object()))
                } else {
                    Err(RuntimeError::InvalidPropTarget)
                }
            }
        }
    }

    fn set_assignable(&mut self, assignable: &Assignable, val: Object) -> RuntimeResult<()> {
        match assignable {
            Assignable::Var(var) => {
                Ok(self.global_stack.set_var(&var, val)?)
            },
            Assignable::PropIndex { obj, index } => {
                let index = self.eval_expression(index)?;
                let index: &ObjectInner = &index.inner();
                match &mut self.get_assignable(obj)?.inner() as &mut ObjectInner {
                    ObjectInner::ARRAY(arr) => {
                        if let ObjectInner::NUMBER(num) = index {
                            let num = *num as usize;
                            if num < 0 {
                                Err(RuntimeError::InvalidArrayIndex)
                            } else if (num as usize) < arr.len() {
                                arr[num] = val;
                                Ok(())
                            } else if (num) == arr.len() {
                                arr.push(val);
                                Ok(())
                            } else {
                                Err(RuntimeError::InvalidArrayIndex)
                            }
                        } else {
                            Err(RuntimeError::InvalidArrayIndex)
                        }
                    },
                    ObjectInner::OBJECT(obj) => {
                        if let ObjectInner::STRING(prop) = index {
                            obj.insert(prop.clone(), val);
                            Ok(())
                        } else {
                            Err(RuntimeError::InvalidObjectIndex)
                        }
                    },
                    _ => Err(RuntimeError::InvalidIndexTarget)
                }
            }
            Assignable::ObjectProp { obj, prop } => {
                let obj = self.get_assignable(obj)?;
                let obj_mut: &mut ObjectInner = &mut obj.inner();
                if let ObjectInner::OBJECT(obj) = obj_mut {
                    obj.insert(prop.clone(), val);
                    Ok(())
                } else {
                    Err(RuntimeError::InvalidPropTarget)
                }
            }
        }
    }


    /// Evaluates a single gabelang program statement generated by the parser in the environment's context
    pub fn eval_statement(&mut self, statement: &Statement) -> RuntimeResult<GabrValue> {
        match statement {
            Statement::Expression(expression) => {
                Ok(GabrValue::new(self.eval_expression(expression)?, true))
            },
            Statement::Let { ident, expression } => {
                let val = self.eval_expression(expression)?;
                self.global_stack.create_var(ident.clone(), val);
                Ok(GabrValue::new(ObjectInner::NULL.as_object(), false))
            },
            Statement::Assign { assignable, expression } => {
                let val = self.eval_expression(expression)?;
                self.set_assignable(assignable, val)?;
                Ok(GabrValue::new(ObjectInner::NULL.as_object(), false))
            },
            Statement::Return(val) => {
                match val.as_ref() {
                    Some(value) => {
                        Ok(GabrValue::new(self.eval_expression(value)?, true))
                    },
                    None => Ok(GabrValue::new(ObjectInner::NULL.as_object(), true))
                }
            },
            Statement::If { cond, body, r#else } => {
                let condition_result = self.eval_expression(cond)?.inner().is_truthy();
                if condition_result {
                    self.eval_program_with_new_scope(body)
                } else {
                    match r#else.as_ref() {
                        Some(else_block) => self.eval_program_with_new_scope(else_block),
                        None => Ok(GabrValue::new(ObjectInner::NULL.as_object(), false))
                    }
                }
            },
            Statement::DoWhile { body, cond } => {
                let mut result;
                loop {
                    result = self.eval_program_with_new_scope(body)?;
                    if result.returning ||
                    !self.eval_expression(cond)?.inner().is_truthy() {
                        break
                    }
                }
                Ok(result)
            },
            Statement::While { cond, body } => {
                let mut result = GabrValue::new(ObjectInner::NULL.as_object(), false);
                while self.eval_expression(cond)?.inner().is_truthy() {
                    result = self.eval_program_with_new_scope(body)?;
                    if result.returning {
                        break;
                    }
                }
                Ok(result)
            },
            Statement::For { init, cond, update, body } => {
                let mut result = GabrValue::new(ObjectInner::NULL.as_object(), false);
                self.global_stack.push_scope();
                self.eval_statement(init)?;
                while self.eval_expression(cond)?.inner().is_truthy() {
                    result = self.eval_program_with_new_scope(body)?;
                    self.eval_statement(update)?;
                    if result.returning {
                        break;
                    }
                }
                self.global_stack.pop_scope()?;
                Ok(result)
            }
            Statement::FuncDecl(func) => {
                self.global_stack.create_func(func.ident.clone(), func.clone());
                Ok(GabrValue::new(ObjectInner::NULL.as_object(), false))
            },
        }
    }

    fn eval_expression(&mut self, expression: &Expression) -> RuntimeResult<Object> {
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
            Expression::Assignable(assignable) => {
                self.get_assignable(assignable)
            },
            Expression::Literal(lit) => {
                match lit {
                    Literal::NumberLit(num) => Ok(ObjectInner::NUMBER(num.clone()).as_object()),
                    Literal::StringLit(string) => Ok(ObjectInner::STRING(string.clone()).as_object()),
                    Literal::Bool(bool) => Ok(ObjectInner::BOOL(*bool).as_object()),
                    Literal::ArrayLit(arr) => {
                        // Create an array of evaluated expressions
                        let arr: Vec<RuntimeResult<Object>> = arr.iter().map(|v| self.eval_expression(v)).collect();
                        let mut err = Ok(());
                        arr.into_iter().for_each(|v| {
                            if let Err(e) = v {
                                *&mut err = Err(e);
                            }
                        });
                        err?;
                        let arr: Vec<Object> = arr.into_iter().map(|v| v.unwrap()).collect();
                        Ok(ObjectInner::ARRAY(arr).as_object())
                    },
                    Literal::ObjectLit(obj) => {
                        // Create an object
                        let fields: Vec<(String, RuntimeResult<Object>)> = obj.iter().map(|(ident, expression)| (ident.clone(), self.eval_expression(expression))).collect();
                        // Check for error in evaluated fields
                        let mut err = Ok(());
                        fields.into_iter().for_each(|(_, v)| {
                            if let Err(e) = v {
                                *&mut err = Err(e);
                            }
                        });
                        err?;
                        let fields: HashMap<String, Object> = fields.into_iter().map(|(name, val)| {
                            let val = val.unwrap();
                            (name, val)
                        }).collect();
                        Ok(ObjectInner::OBJECT(fields).as_object())
                    }
                }
            }
        }
    }

    fn eval_prefix(&mut self, op: PrefixOp, expression: &Expression) -> RuntimeResult<Object> {
        let expression = self.eval_expression(expression)?;
        match op {
            PrefixOp::Bang => {
                if let ObjectInner::NUMBER(val) = *expression.inner() {
                    if val != 0 {
                        Ok(ObjectInner::NUMBER(0).as_object())
                    } else {
                        Ok(ObjectInner::NUMBER(1).as_object())
                    }
                } else {
                    Err("Can not perform not operation on non numeric expression".to_string())
                }
            },
            PrefixOp::Neg => {
                if let ObjectInner::NUMBER(val) = *expression.inner() {
                    Ok(ObjectInner::NUMBER(-val).as_object())
                } else {
                    Err("Can not perform not operation on non numeric expression".to_string())
                }
            }
        }
    }

    fn eval_infix(&mut self, op: InfixOp, left: &Expression, right: &Expression) -> RuntimeResult<Object> {
        let op1 = self.eval_expression(left)?.inner().clone();
        let op2 = self.eval_expression(right)?.inner().clone();
        let op1 = match op1 {
            ObjectInner::NUMBER(num) => num,
            ObjectInner::BOOL(bool) => bool as i64,
            ObjectInner::STRING(string1) => {
                if let ObjectInner::STRING(string2) = op2 {
                    return Ok(ObjectInner::STRING(string1 + &string2).as_object());
                } else {
                    return Err(format!("Infix Evaluation Failed: can not concat a string with a non string value"))
                }
            }
            _ => return Err(format!("Infix Evaluation Failed: left operand did not evaluate to a number or bool, expression: {:?} evaluated to {:?}", left, op1))
        };
        let op2 = match op2 {
            ObjectInner::NUMBER(num) => num,
            ObjectInner::BOOL(bool) => bool as i64,
            _ => return Err(format!("Infix Evaluation Failed: left operand did not evaluate to a number or bool, expression: {:?} evaluated to {:?}", right, op2))
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
        Ok(ObjectInner::NUMBER(val).as_object())
    }

    fn eval_function_call(&mut self, func_locator: &Assignable, params: &Vec<Expression>) -> RuntimeResult<Object> {
        // look in all scopes for a function that matches function name
        let func = self.get_assignable(func_locator).ok();
        if let Some(func) = func {
            let func = &mut *func.inner();
            let func = match func {
                ObjectInner::FUNCTION(func) => func.clone(),
                _ => return Err("Can not call non-function value as function".to_string())
            };
            // Create (parameter, value) list
            let params: Vec<(String, Result<Object, String>)> = func.ast.params.iter()
                .map(|param| param.clone())
                .zip(params.iter().map(|param| self.eval_expression(param)))
                .collect();
            // Create new scope for parameters
            self.global_stack.push_scope();
            self.load_params(params)?;
            // Evaluate the body of the function with the new context
            let result = self.eval_program(&func.ast.body)?;
            // Remove param variables scope
            self.global_stack.pop_scope()?;
            // Function call should not automatically be interpretted as return funcCall(param);
            Ok(result.gabr_object)
        } else {
            let func_name = match func_locator {
                Assignable::Var(ident) => ident,
                _ => return Err("Function call failed to evaluate: function could not be found".to_string()),
            };
            if let Some(built_in) = self.get_built_in(func_name.clone()) {
                let params: Vec<(String, Result<Object, String>)> = built_in.get_params().iter()
                    .map(|param| param.clone())
                    .zip(params.iter().map(|param| self.eval_expression(param)))
                    .collect();
                // Create new scope for parameters
                self.global_stack.push_scope();
                self.load_params(params)?;
                // Evaluate built in function in new context
                let result = built_in.eval(self)?;
                // Remove param variables scope
                self.global_stack.pop_scope()?;
                // Function call should not automatically be interpretted as return funcCall(param);
                Ok(result)
            } else {
                Err(format!("Function {} could not be found", func_name))
            }
        }
    }
}

/// A valid value in the gabelang language
///
/// Could be an integer, array, function, object, or NULL type
#[derive(Clone)]
pub struct GabrValue {
    gabr_object: Object,
    returning: bool,
}

impl GabrValue {
    fn new(gabr_object: Object, returning: bool) -> Self {
        Self { gabr_object, returning }
    }

    /// Checks if the current object is not null
    pub fn is_some(&self) -> bool {
        if let ObjectInner::NULL = *self.gabr_object.inner() {
            false
        } else {
            true
        }
    }
}

impl Display for GabrValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.gabr_object)
    }
}

#[derive(Clone, Debug)]
struct Object(Rc<RefCell<ObjectInner>>);

impl Object {
    fn inner(&self) -> RefMut<ObjectInner> {
        self.0.borrow_mut()
    }
}

impl PartialEq for Object {
    /// Two objects are equal if they point to the same place in memory
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let pointer = Rc::as_ptr(&self.0) as usize;
        pointer.hash(state);
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Creates a static internally mutable map variable on a per thread basis.
        // This variable will stick around between function calls because it is static
        thread_local! {
            static PRINTING: RefCell<HashSet<usize>> = RefCell::new(HashSet::new())
        }
        // cast Rc to a pointer to the underlying object
        let ptr = Rc::as_ptr(&self.0) as usize;
        // If we have seen this object before print "Cycle"
        if PRINTING.with(|printing| printing.borrow().contains(&ptr)) {
            return f.write_str("Cycle");
        }
        // Else indicate that we have seen this object,
        // Print it,
        // And then remove it from the hashset so it doesnt interupt future prints
        PRINTING.with(|printing| printing.borrow_mut().insert(ptr));
        let res = write!(f, "{}", self.0.borrow());
        PRINTING.with(|printing| printing.borrow_mut().remove(&ptr));
        res
    }
}

#[derive(Clone, Debug)]
enum ObjectInner {
    NUMBER(i64),
    STRING(String),
    ARRAY(Vec<Object>),
    FUNCTION(FunctionInner),
    OBJECT(HashMap<String, Object>),
    BOOL(bool),
    NULL
}

impl ObjectInner {
    fn is_truthy(&self) -> bool {
        match self {
            Self::NUMBER(val) => *val != 0,
            Self::NULL => false,
            Self::BOOL(b) if !*b => false,
            _ => true
        }
    }

    fn as_object(self) -> Object {
        Object(Rc::new(RefCell::new(self)))
    }
}

impl Display for ObjectInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NUMBER(val) => write!(f, "{val}"),
            Self::STRING(string) => f.write_str(string),
            Self::ARRAY(vals) => {
                write!(f, "[{}]", join(vals, ", "))
            },
            Self::OBJECT(obj) => {
                let properties = obj.iter().map(|(key, val)| {
                    let mut output = String::new();
                    write!(output, "\t{key}: {val}").unwrap();
                    output
                }).collect::<Vec<String>>();
                write!(f, "{{\n{}\n}}", join(&properties, "\n"))
            },
            Self::FUNCTION(func) => write!(f, "{func}"),
            Self::BOOL(bool) => write!(f, "{}", bool),
            Self::NULL => f.write_str("null")
        }
    }
}

#[derive(Debug, Clone)]
struct FunctionInner {
    ast: ast::Function,
    refs: HashMap<String, Object>
}

impl Display for FunctionInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(Function: {})", self.ast)
    }
}
