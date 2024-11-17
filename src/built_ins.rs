use std::rc::Rc;
use std::collections::HashMap;

use crate::evaluator::{GabrEnv, GabrValue, ObjectType};

pub fn load_built_ins() -> HashMap<String, Rc<dyn BuiltIn>> {
    let mut hash_map = HashMap::new();
    hash_map.insert("len".to_string(), Rc::from(Len{}.as_built_in()));
    hash_map
}

pub trait BuiltIn {
    fn get_params(&self) -> Vec<String>;
    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String>;
    fn as_built_in(self) -> Rc<dyn BuiltIn>;
}

struct Len{}

impl BuiltIn for Len {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_arr".to_string() 
        ]
    }

    fn eval(&self, env: &mut GabrEnv) -> Result<GabrValue, String> {
        let arr = env.get_var("_arr".to_string());
        match arr {
            Some(arr) => {
                if let ObjectType::ARRAY(arr) = arr.gabr_object.clone() {
                    Ok(GabrValue {
                        gabr_object: ObjectType::NUMBER(arr.len() as i64),
                        returning: false
                    })
                } else {
                    Err("built in function \"len\" expected array value as argument".to_string())
                }
            },
            None => Err("Array not provided to built in function \"len\"".to_string())
        }
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}
