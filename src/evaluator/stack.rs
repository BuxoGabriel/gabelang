use std::{collections::HashMap, error::Error, fmt::Display, rc::Rc};

use crate::ast;

use super::{FunctionInner, Object, ObjectInner};

/// A stack error is produced when an attempt to manipulate the stack is invalid or when trying to
/// access a variable that is not on the stack
#[derive(Debug)]
pub enum StackError {
    /// A Pop Empty Frame error is generated when trying to pop a stack frame from a stack with no
    /// frames
    PopEmptyFrame,
    /// A Variable Not In Scope error is generated when trying to manipulate or get the value of a
    /// variable that does not exist in the stack
    VariableNotInScope,
    /// A No Stack Error is generated when trying to get or use a variable from the stack but no
    /// stack frames exist
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
#[derive(Debug, Clone)]
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
            if let Some(obj) = scope.get_mut(name) {
                *obj.inner() = val.inner().clone();
                return Ok(())
            }
        }
        Err(StackError::VariableNotInScope)
    }

    /// Creates a new function and adds it to the topmost stack frame
    pub fn create_func(&mut self, name: String, ast: ast::Function) -> StackResult<()> {
        let func = FunctionInner {
            ast,
            context: self.flat_copy()
        };
        let scope = self.get_mut().last_mut().ok_or(StackError::NoStack)?;
        scope.insert(name, ObjectInner::FUNCTION(func).as_object());
        Ok(())
    }

    /// Creates a copy of the visible stack
    pub fn flat_copy(&self) -> Self {
        let mut stack = HashMap::new();
        self.get().iter()
            .for_each(|map| map.iter().for_each(|(k, v)| {
                stack.insert(k.clone(), v.clone());
            }));
        Self(vec![stack])
    }
}
