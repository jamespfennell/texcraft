use crate::token::catcode::CatCodeMap;
use crate::vm;
use std::collections::HashMap;

pub fn new_vm(source: &str) -> vm::VM<()> {
    let mut vm = vm::VM::<()>::new(CatCodeMap::new_with_tex_defaults(), HashMap::new(), ());
    vm.push_source("".to_string(), source.to_string()).unwrap();
    vm
}
