use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::rc::Rc;

use super::Object;

/// Errors produced by environment / variable operations.
#[derive(Debug)]
pub enum StackError {
    /// Tried to leave a scope that has no enclosing parent (i.e. tried to pop
    /// past the global environment).
    PopEmptyFrame,
    /// Tried to read or assign a variable that exists in no enclosing scope.
    VariableNotInScope,
}

impl Display for StackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PopEmptyFrame => f.write_str("Could not exit scope: environment has no enclosing parent."),
            Self::VariableNotInScope => f.write_str("Could not find variable in any enclosing scope."),
        }
    }
}

impl Error for StackError {}

type StackResult<T> = Result<T, StackError>;

/// A lexically-scoped environment frame.
///
/// Each `Environment` owns its own variable bindings and optionally holds an
/// `Rc<RefCell<Environment>>` to its enclosing scope, forming a chain that
/// mirrors the program's lexical nesting at runtime. Lookups and assignments
/// walk this chain. Functions capture their declaring environment by cloning
/// the `Rc`, which gives standard lexical-closure semantics without
/// snapshotting state at declaration time.
#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Object>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    /// Creates a new root environment with no enclosing scope.
    pub fn new() -> Self {
        Self { values: HashMap::new(), enclosing: None }
    }

    /// Creates a new environment whose enclosing scope is `parent`.
    pub fn new_enclosed(parent: Rc<RefCell<Environment>>) -> Self {
        Self { values: HashMap::new(), enclosing: Some(parent) }
    }

    /// Returns the enclosing scope, if any.
    pub fn enclosing(&self) -> Option<Rc<RefCell<Environment>>> {
        self.enclosing.clone()
    }

    /// Binds `name` to `val` in this environment, shadowing any outer binding.
    pub fn create_var(&mut self, name: String, val: Object) {
        self.values.insert(name, val);
    }

    /// Looks up `name`, walking the enclosing chain. Returns an `Rc`-clone of
    /// the stored `Object` handle (not a deep copy of the value).
    pub fn get_var(&self, name: &str) -> StackResult<Object> {
        if let Some(val) = self.values.get(name) {
            return Ok(val.clone());
        }
        match &self.enclosing {
            Some(parent) => parent.borrow().get_var(name),
            None => Err(StackError::VariableNotInScope),
        }
    }

    /// Walks the enclosing chain to find `name` and mutates the value in
    /// place through its `RefCell`. Errors if no enclosing scope binds
    /// `name`. (See `todo.md` item 4 for the aliasing caveat of mutating
    /// through the existing `Rc`.)
    pub fn set_var(&mut self, name: &str, val: Object) -> StackResult<()> {
        if let Some(obj) = self.values.get_mut(name) {
            *obj.inner() = val.inner().clone();
            return Ok(());
        }
        match &self.enclosing {
            Some(parent) => parent.borrow_mut().set_var(name, val),
            None => Err(StackError::VariableNotInScope),
        }
    }
}
