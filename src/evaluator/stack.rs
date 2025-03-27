use std::{collections::HashMap, error::Error, fmt::Display};

use crate::ast;

use super::{FunctionInner, Object, ObjectInner};

#[derive(Debug)]
pub enum StackError {
    PopEmptyFrame,
    VariableNotInScope,
    NoStack,
}

impl Display for StackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PopEmptyFrame => f.write_str("Could not pop stack frame because stack is empty!"),
            Self::VariableNotInScope => f.write_str("Could not find variable in any stack"),
            Self::NoStack => f.write_str("Could not perform operation since there are no stack frames."),
            
        }
    }
}

impl Error for StackError {}

type StackResult<T> = Result<T, StackError>;
type StackInner = Vec<HashMap<String, Object>>;

/// The Runtime Stack object
#[derive(Clone)]
pub struct Stack(StackInner);

impl  Stack {
    fn get(&self) -> &StackInner {
        &self.0
    }

    fn get_mut(&mut self) -> &mut StackInner  {
        &mut self.0
    }

    /// Creates a new stack object
    pub fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    /// Pushes a new stack frame on to the stack
    pub fn push_scope(&mut self) {
        self.get_mut().push(HashMap::new())
    }

    /// Pops a stack frame and all of its variables off the stack
    pub fn pop_scope(&mut self) -> StackResult<()> {
        self.get_mut().pop().map(|_| ()).ok_or(StackError::PopEmptyFrame)
    }

    /// Loads named parameters onto the topmost stack frame
    pub fn load_params(&mut self, params: Vec<(String, Object)>) -> StackResult<()> {
        for (name, val) in params.into_iter() {
            self.create_var(name, val)?;
        }
        Ok(())
    }

    /// Creates a new variable on the topmost stack frame
    pub fn create_var(&mut self, name: String, val: Object) -> StackResult<()> {
        let scope = self.get_mut().last_mut().ok_or(StackError::NoStack)?;
        scope.insert(name, val);
        Ok(())
    }

    /// Gets a variable from the the topmost stack frame that it can find it.
    /// If the variable can not be found on any stack frame, returns [Err<StackError>]
    pub fn get_var(&self, name: &str) -> StackResult<Object> {
        for scope in self.get().iter().rev() {
            if let Some(val) = scope.get(name) {
                return Ok(val.clone());
            }
        }
        Err(StackError::VariableNotInScope)
    }

    /// Searches for a variable on the topmost frame that it can find it.
    /// If the variable is found its value is set to val
    /// If the variable is not found it returns an [Err<StackError>]
    pub fn set_var(&mut self, name: &str, val: Object) -> StackResult<()> {
        for scope in self.get_mut().iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return Ok(())
            }
        }
        Err(StackError::VariableNotInScope)
    }

    /// Creates a new function and adds it to the topmost stack frame
    pub fn create_func(&mut self, name: String, ast: ast::Function) -> StackResult<()> {
        let mut refs = HashMap::new();
        for scope in self.get() {
            scope.iter().map(|(k, v)|(k.clone(), v.clone())).for_each(|(k, v) | {refs.insert(k, v);});
        }
        let func = FunctionInner {
            ast,
            refs
        };
        let scope = self.get_mut().last_mut().ok_or(StackError::NoStack)?;
        scope.insert(name, ObjectInner::FUNCTION(func).as_object());
        Ok(())
    }

}
