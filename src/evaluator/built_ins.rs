use std::rc::Rc;
use std::collections::HashMap;

use crate::evaluator::{GabrEnv, Object};

use super::ObjectInner;

pub fn load_built_ins() -> HashMap<String, Rc<dyn BuiltIn>> {
    let mut hash_map = HashMap::new();
    hash_map.insert("len".to_string(), Len{}.as_built_in());
    hash_map.insert("reverse".to_string(), Reverse{}.as_built_in());
    hash_map
}

pub trait BuiltIn {
    fn get_params(&self) -> Vec<String>;
    fn eval(&self, env: &mut GabrEnv) -> Result<Object, String>;
    fn as_built_in(self) -> Rc<dyn BuiltIn>;
}

struct Len{}

impl BuiltIn for Len {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_obj".to_string() 
        ]
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<Object, String> {
        let obj = env.get_var("_obj");
        if obj.is_none() {
            return Err("Built-In \"len\" did not recieve expected arg \"_arr\"".to_string())
        };
        let obj = obj.unwrap();
        let obj = &*obj.inner();
        match obj {
            ObjectInner::ARRAY(arr) => Ok(ObjectInner::NUMBER(arr.len() as i64).as_object()),
            ObjectInner::STRING(string) => Ok(ObjectInner::NUMBER(string.len() as i64).as_object()),
            _ => Err("Built-In \"len\" expected array or string as first argument".to_string())
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
    fn eval(&self, env: &mut GabrEnv) -> Result<Object, String> {
        let obj = env.get_var("_obj");
        if obj.is_none() {
            return Err("Built-In \"reverse\" did not recieve expected arg \"_obj\"".to_string())
        }
        let obj = obj.unwrap();
        let obj = &*obj.inner();
        match obj {
            ObjectInner::STRING(string) => Ok(ObjectInner::STRING(string.chars().rev().collect()).as_object()),
            ObjectInner::ARRAY(arr) => Ok(ObjectInner::ARRAY(arr.clone().into_iter().rev().collect()).as_object()),
            _ => Err("Built-In \"reverse\" expects first arg\"str\" to be of type string or array".to_string())
        }
    }
    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}
