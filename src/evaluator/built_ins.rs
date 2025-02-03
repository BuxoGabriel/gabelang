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
            "_arr".to_string() 
        ]
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<ObjectType, String> {
        let arr = env.get_var("_arr");
        match arr {
            Some(arr) => {
                if let ObjectType::ARRAY(arr) = arr.clone() {
                    Ok(ObjectType::NUMBER(arr.len() as i64))
                } else {
                    Err("Built-In \"len\" expected array value as argument".to_string())
                }
            },
            None => Err("Built-In \"len\" expected did not recieve expected arg \"_arr\"".to_string())
        }
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}
