use std::{fmt::Display, error::Error, rc::Rc};
use std::collections::HashMap;

use crate::evaluator::{Runtime, Object};

use super::ObjectInner;

#[derive(Debug)]
pub struct BuiltInError(String);

impl Display for BuiltInError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Error for BuiltInError {}

type BuiltInResult<T> = Result<T, BuiltInError>;

pub fn load_built_ins() -> HashMap<String, Rc<dyn BuiltIn>> {
    let mut hash_map = HashMap::new();
    hash_map.insert("len".to_string(), Len{}.as_built_in());
    hash_map.insert("reverse".to_string(), Reverse{}.as_built_in());
    hash_map.insert("abs".to_string(), Abs{}.as_built_in());
    hash_map
}

pub trait BuiltIn {
    fn get_params(&self) -> Vec<String>;
    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object>;
    fn as_built_in(self) -> Rc<dyn BuiltIn>;
}

struct Len{}

impl BuiltIn for Len {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_obj".to_string() 
        ]
    }

    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let obj = rt.global_stack.get_var("_obj");
        if obj.is_err() {
            return Err(BuiltInError("Built-In \"len\" did not recieve expected arg \"_arr\"".to_owned()))
        };
        let obj = obj.unwrap();
        let obj = &*obj.inner();
        match obj {
            ObjectInner::ARRAY(arr) => Ok(ObjectInner::NUMBER(arr.len() as i64).as_object()),
            ObjectInner::OBJECT(map) => Ok(ObjectInner::NUMBER(map.keys().len() as i64).as_object()),
            ObjectInner::STRING(string) => Ok(ObjectInner::NUMBER(string.len() as i64).as_object()),
            _ => Err(BuiltInError("Built-In \"len\" expected array, object or string as first argument".to_owned()))
        }
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}

// returns a new reversed array object without altering parameter
struct Reverse{}

impl BuiltIn for Reverse {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_obj".to_string(),
        ]
    }
    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let obj = rt.global_stack.get_var("_obj");
        if obj.is_err() {
            return Err(BuiltInError("Built-In \"reverse\" did not recieve expected arg \"_obj\"".to_owned()))
        }
        let obj = obj.unwrap();
        let obj = &*obj.inner();
        match obj {
            ObjectInner::STRING(string) => Ok(ObjectInner::STRING(string.chars().rev().collect()).as_object()),
            ObjectInner::ARRAY(arr) => Ok(ObjectInner::ARRAY(arr.clone().into_iter().rev().collect()).as_object()),
            _ => Err(BuiltInError("Built-In \"reverse\" expects first arg\"str\" to be of type string or array".to_owned()))
        }
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}

struct Abs{}

impl BuiltIn for Abs {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_num".to_string(),
        ]
    }

    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let _num = rt.global_stack.get_var("_num");
        if _num.is_err() {
            return Err(BuiltInError("Built-In \"abs\" did not recieve arg abs".to_owned()));
        }
        let num = _num.unwrap();
        let num = &*num.inner();
        match num {
            ObjectInner::NUMBER(num) => Ok(ObjectInner::NUMBER(num.abs()).as_object()),
            _ => Err(BuiltInError("Built-In \"abs\" expects first argument to be a number".to_owned()))
        }
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}
