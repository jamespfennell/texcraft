use crate::runtime;
use crate::token::catcode::CatCodeMap;

pub fn new_execution_input<T: AsRef<str>>(source: T) -> runtime::ExecutionInput<()> {
    let mut env = runtime::Env::<()>::new(CatCodeMap::new_with_tex_defaults(), ());
    env.push_source(source.as_ref().to_string()).unwrap();
    runtime::ExecutionInput::new(env)
}
