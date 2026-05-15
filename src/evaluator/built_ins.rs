use std::fs;
use std::io::{self, Write};
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
    hash_map.insert("open".to_string(), Open{}.as_built_in());
    hash_map.insert("print".to_string(), Print{}.as_built_in());
    hash_map.insert("char_code".to_string(), CharCode{}.as_built_in());
    hash_map.insert("substring".to_string(), Substring{}.as_built_in());
    hash_map.insert("concat".to_string(), Concat{}.as_built_in());
    hash_map.insert("prompt".to_string(), Prompt{}.as_built_in());
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
        let obj = rt.current_context().borrow().get_var("_obj");
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
        let obj = rt.current_context().borrow().get_var("_obj");
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
        let _num = rt.current_context().borrow().get_var("_num");
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

struct Open{}

impl BuiltIn for Open {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_path".to_string(),
        ]
    }

    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let _path = rt.current_context().borrow().get_var("_path");
        if _path.is_err() {
            return Err(BuiltInError("Built-In \"open\" did not recieve arg _path".to_owned()));
        }
        let _path = _path.unwrap();
        let _path = &*_path.inner();
        match _path {
            ObjectInner::STRING(path) => {
                let content = fs::read_to_string(path).map_err(|_| BuiltInError("Built-In \"open\" failed to read file!".to_owned()))?;
                Ok(ObjectInner::STRING(content).as_object())
            },
            _ => Err(BuiltInError("Built-In \"open\" expects first argument to be a string".to_owned()))
        }
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}

struct Print{}

impl BuiltIn for Print {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_content".to_string(),
        ]
    }

    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let _content = rt.current_context().borrow().get_var("_content");
        if _content.is_err() {
            return Err(BuiltInError("Built-In \"print\" did not recieve arg _content".to_owned()));
        }
        let _content = _content.unwrap();
        let _content = &*_content.inner();
        println!("{_content}");
        Ok(ObjectInner::NULL.as_object())
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}

struct CharCode{}

impl BuiltIn for CharCode {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_s".to_string(),
            "_i".to_string(),
        ]
    }

    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let s = rt.current_context().borrow().get_var("_s")
            .map_err(|_| BuiltInError("Built-In \"char_code\" did not recieve arg _s".to_owned()))?;
        let i = rt.current_context().borrow().get_var("_i")
            .map_err(|_| BuiltInError("Built-In \"char_code\" did not recieve arg _i".to_owned()))?;
        let s = s.inner();
        let i = i.inner();
        let string = match &*s {
            ObjectInner::STRING(string) => string,
            _ => return Err(BuiltInError("Built-In \"char_code\" expects first argument to be a string".to_owned()))
        };
        let index = match &*i {
            ObjectInner::NUMBER(num) => *num,
            _ => return Err(BuiltInError("Built-In \"char_code\" expects second argument to be a number".to_owned()))
        };
        if index < 0 || (index as usize) >= string.len() {
            return Err(BuiltInError("Built-In \"char_code\" index out of bounds".to_owned()));
        }
        let byte = string.as_bytes()[index as usize];
        Ok(ObjectInner::NUMBER(byte as i64).as_object())
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}

struct Substring{}

impl BuiltIn for Substring {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_s".to_string(),
            "_start".to_string(),
            "_end".to_string(),
        ]
    }

    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let s = rt.current_context().borrow().get_var("_s")
            .map_err(|_| BuiltInError("Built-In \"substring\" did not recieve arg _s".to_owned()))?;
        let start = rt.current_context().borrow().get_var("_start")
            .map_err(|_| BuiltInError("Built-In \"substring\" did not recieve arg _start".to_owned()))?;
        let end = rt.current_context().borrow().get_var("_end")
            .map_err(|_| BuiltInError("Built-In \"substring\" did not recieve arg _end".to_owned()))?;
        let s = s.inner();
        let start = start.inner();
        let end = end.inner();
        let string = match &*s {
            ObjectInner::STRING(string) => string,
            _ => return Err(BuiltInError("Built-In \"substring\" expects first argument to be a string".to_owned()))
        };
        let start = match &*start {
            ObjectInner::NUMBER(num) => *num,
            _ => return Err(BuiltInError("Built-In \"substring\" expects second argument to be a number".to_owned()))
        };
        let end = match &*end {
            ObjectInner::NUMBER(num) => *num,
            _ => return Err(BuiltInError("Built-In \"substring\" expects third argument to be a number".to_owned()))
        };
        if start < 0 || end < 0 || start > end || (end as usize) > string.len() {
            return Err(BuiltInError("Built-In \"substring\" index out of bounds".to_owned()));
        }
        let slice = string[start as usize..end as usize].to_owned();
        Ok(ObjectInner::STRING(slice).as_object())
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}

struct Prompt{}

impl BuiltIn for Prompt {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_msg".to_string(),
        ]
    }

    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let msg = rt.current_context().borrow().get_var("_msg")
            .map_err(|_| BuiltInError("Built-In \"prompt\" did not recieve arg _msg".to_owned()))?;
        let msg = msg.inner();
        let msg = match &*msg {
            ObjectInner::STRING(string) => string.clone(),
            _ => return Err(BuiltInError("Built-In \"prompt\" expects first argument to be a string".to_owned()))
        };
        print!("{msg}");
        io::stdout().flush().map_err(|e| BuiltInError(format!("Built-In \"prompt\" failed to flush stdout: {e}")))?;
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).map_err(|e| BuiltInError(format!("Built-In \"prompt\" failed to read input: {e}")))?;
        if buf.ends_with('\n') { buf.pop(); }
        if buf.ends_with('\r') { buf.pop(); }
        Ok(ObjectInner::STRING(buf).as_object())
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}

struct Concat{}

impl BuiltIn for Concat {
    fn get_params(&self) -> Vec<String> {
        vec![
            "_a".to_string(),
            "_b".to_string(),
        ]
    }

    fn eval(&self, rt: &mut Runtime) -> BuiltInResult<Object> {
        let a = rt.current_context().borrow().get_var("_a")
            .map_err(|_| BuiltInError("Built-In \"concat\" did not recieve arg _a".to_owned()))?;
        let b = rt.current_context().borrow().get_var("_b")
            .map_err(|_| BuiltInError("Built-In \"concat\" did not recieve arg _b".to_owned()))?;
        let combined = format!("{}{}", &*a.inner(), &*b.inner());
        Ok(ObjectInner::STRING(combined).as_object())
    }

    fn as_built_in(self) -> Rc<dyn BuiltIn> {
        Rc::from(self)
    }
}
