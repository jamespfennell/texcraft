use crate::vm;
use crate::token::catcode::CatCodeMap;
use std::collections::HashMap;

pub fn new_env(source: &str) -> vm::Env<()> {
    let mut env = vm::Env::<()>::new(CatCodeMap::new_with_tex_defaults(), HashMap::new(), ());
    env.push_source("".to_string(), source.to_string()).unwrap();
    env
}
