use std::{
    collections::HashMap,
    ffi::OsString,
    path::{Path, PathBuf},
};
use texlang::traits::*;
use texlang::*;
use texlang_common as common;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    #[cfg_attr(feature = "serde", serde(skip))]
    job_name: Option<PathBuf>,
    default_job_name: PathBuf,
    dump_format: i32,
    dump_validate: i32,
    #[cfg_attr(feature = "serde", serde(skip))]
    num_dumps: usize,
}

impl Default for Component {
    fn default() -> Self {
        Self {
            job_name: None,
            default_job_name: "jobname".into(),
            dump_format: 0,
            dump_validate: 0,
            num_dumps: 0,
        }
    }
}

impl Component {
    pub fn job_name(&self) -> &Path {
        match &self.job_name {
            None => &self.default_job_name,
            Some(job_name) => job_name,
        }
    }

    // TODO: this should be called after the first \input.
    // Probably should add a hook to the VM for this case.
    // TODO: maybe this should be bundled with \input
    /// Set the job name based on the provided file path.
    pub fn set_job_name(&mut self, file_path: &Path) {
        if let Some(file_stem) = file_path.file_stem() {
            self.job_name = Some(file_stem.into())
        }
    }
}

/// Get the `\jobname` primitive.
pub fn get_jobname<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(|token, input| {
        let job_name: String = input
            .state()
            .component()
            .job_name()
            .to_string_lossy()
            .into();
        input.push_string_tokens(token, &job_name);
        Ok(())
    })
}

/// Get the `\dump` primitive.
#[cfg(feature = "serde")]
pub fn get_dump<
    S: HasComponent<Component>
        + serde::Serialize
        + serde::de::DeserializeOwned
        + common::HasFileSystem,
>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(dump_primitive_fn)
}

#[cfg(feature = "serde")]
fn dump_primitive_fn<
    S: HasComponent<Component>
        + serde::Serialize
        + serde::de::DeserializeOwned
        + common::HasFileSystem,
>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
    let component = input.state().component();
    let mut job_name: OsString = component.job_name().into();
    let num_dumps = component.num_dumps;
    if num_dumps > 0 {
        job_name.push(format!["-{}", num_dumps + 1]);
    }
    let mut output_file: PathBuf = job_name.into();
    output_file.set_extension(match component.dump_format {
        1 => "fmt.json",
        2 => "fmt.bincode",
        _ => "fmt",
    });

    // TODO: error handle all these serialization errors.
    let serialized = match component.dump_format {
        0 => rmp_serde::encode::to_vec(input.vm()).unwrap(),
        1 => serde_json::to_vec_pretty(input.vm()).unwrap(),
        2 => bincode::serde::encode_to_vec(input.vm(), bincode::config::standard()).unwrap(),
        i => {
            return Err(error::SimpleFailedPreconditionError::new(format![
                r"\dumpFormat has invalid value {i}",
            ])
            .with_note(r"\dumpFormat must be either 0 (message pack), 1 (json) or 2 (bincode)")
            .into())
        }
    };

    if component.dump_validate != 0 {
        let initial_built_ins: HashMap<&str, command::BuiltIn<S>> = input
            .vm()
            .commands_map
            .initial_built_ins()
            .iter()
            .map(|(cs_name, command)| {
                (
                    input.vm().cs_name_interner().resolve(*cs_name).unwrap(),
                    command.clone(),
                )
            })
            .collect();
        match component.dump_format {
            0 => {
                let mut deserializer = rmp_serde::decode::Deserializer::from_read_ref(&serialized);
                vm::VM::<S>::deserialize(&mut deserializer, initial_built_ins);
            }
            1 => {
                let mut deserializer = serde_json::Deserializer::from_slice(&serialized);
                vm::VM::<S>::deserialize(&mut deserializer, initial_built_ins);
            }
            2 => {
                let deserialized: Box<vm::serde::DeserializedVM<S>> =
                    bincode::serde::decode_from_slice(&serialized, bincode::config::standard())
                        .unwrap()
                        .0;
                vm::serde::finish_deserialization(deserialized, initial_built_ins);
            }
            _ => unreachable!(),
        };
    }

    // TODO: error handle file write
    input
        .state()
        .file_system()
        .borrow_mut()
        .write_bytes(&output_file, &serialized)
        .unwrap();
    input.state_mut().component_mut().num_dumps += 1;
    Ok(())
}

pub fn get_dumpformat<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_singleton(
        |state: &S, _| &state.component().dump_format,
        |state: &mut S, _| &mut state.component_mut().dump_format,
    )
    .into()
}

pub fn get_dumpvalidate<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_singleton(
        |state: &S, _| &state.component().dump_validate,
        |state: &mut S, _| &mut state.component_mut().dump_validate,
    )
    .into()
}
