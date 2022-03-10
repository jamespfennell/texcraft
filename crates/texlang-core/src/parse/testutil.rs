use crate::runtime;
use crate::token::catcode::CatCodeMap;

pub fn new_env<T: AsRef<str>>(source: T) -> runtime::Env<()> {
    let mut env = runtime::Env::<()>::new(CatCodeMap::new_with_tex_defaults(), ());
    env.push_source(source.as_ref().to_string()).unwrap();
    env
}
