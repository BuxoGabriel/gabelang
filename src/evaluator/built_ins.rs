use std::rc::Rc;
use std::collections::HashMap;

use crate::evaluator::{GabrEnv, ObjectType};

pub fn load_built_ins() -> HashMap<String, Rc<dyn BuiltIn>> {
    let mut hash_map = HashMap::new();
    hash_map.insert("len".to_string(), Rc::from(Len{}.as_built_in()));
    hash_map
}

pub trait BuiltIn {
    fn get_params(&self) -> Vec<String>;
    fn eval(&self, env: &mut GabrEnv) -> Result<ObjectType, String>;
    fn as_built_in(self) -> Rc<dyn BuiltIn>;
}

struct Len{}

impl BuiltIn for Len {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_obj".to_string() 
        ]
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<ObjectType, String> {
        let obj = env.get_var("_obj");
        if obj.is_none() {
            return Err("Built-In \"len\" expected did not recieve expected arg \"_arr\"".to_string())
        };
        match obj.unwrap() {
            ObjectType::ARRAY(arr) => Ok(ObjectType::NUMBER(arr.len() as i64)),
            ObjectType::STRING(string) => Ok(ObjectType::NUMBER(string.len() as i64)),
            _ => Err("Built-In \"len\" expected array value as argument".to_string())
        }
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}
