//! Variable commands related to time.

#[cfg(feature = "time")]
use chrono::prelude::*;
use texlang_core::{command, vm::HasComponent, variable};

/// Component for storing state related to the time commands.
pub struct Component {
    minutes_since_midnight: i32,
    day: i32,
    month: i32,
    year: i32,
}

impl Component {
    /// Create a new component with the variables initialized to the current time.
    #[cfg(feature = "time")]
    pub fn new() -> Component {
        let dt: DateTime<Local> = Local::now();
        Component {
            minutes_since_midnight: 60 * (dt.time().hour() as i32) + (dt.time().minute() as i32),
            day: dt.date().day() as i32,
            month: dt.date().month() as i32,
            year: dt.date().year(),
        }
    }

    #[cfg(not(feature = "time"))]
    pub fn new() -> Component {
        Component {
            minutes_since_midnight: 0,
            day: 0,
            month: 0,
            year: 0,
        }
    }

    /// Create a new component with the variable initialized with the provided values.
    ///
    /// This is useful in situations where the DateTime library can't be used; e.g., when
    /// Texcraft is compiled to WebAssembly and running in the browser.
    pub fn new_with_values(
        minutes_since_midnight: i32,
        day: i32,
        month: i32,
        year: i32,
    ) -> Component {
        Component {
            minutes_since_midnight,
            day,
            month,
            year,
        }
    }
}

impl Default for Component {
    fn default() -> Self {
        Self::new()
    }
}

/// Get the `\time` command.
pub fn get_time<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_variable(
        |_, _, _| -> anyhow::Result<variable::Variable<S>> {
            Ok(variable::Variable::Int(variable::TypedVariable::new(
                |state: &S, _: u32| -> &i32 { &state.component().minutes_since_midnight },
                |state: &mut S, _: u32| -> &mut i32 {
                    &mut state.component_mut().minutes_since_midnight
                },
                0,
            )))
        },
        0,
    )
}

/// Get the `\day` command.
pub fn get_day<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_variable(
        |_, _, _| -> anyhow::Result<variable::Variable<S>> {
            Ok(variable::Variable::Int(variable::TypedVariable::new(
                |state: &S, _: u32| -> &i32 { &state.component().day },
                |state: &mut S, _: u32| -> &mut i32 { &mut state.component_mut().day },
                0,
            )))
        },
        0,
    )
}

/// Get the `\month` command.
pub fn get_month<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_variable(
        |_, _, _| -> anyhow::Result<variable::Variable<S>> {
            Ok(variable::Variable::Int(variable::TypedVariable::new(
                |state: &S, _: u32| -> &i32 { &state.component().month },
                |state: &mut S, _: u32| -> &mut i32 { &mut state.component_mut().month },
                0,
            )))
        },
        0,
    )
}

/// Get the `\year` command.
pub fn get_year<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_variable(
        |_, _, _| -> anyhow::Result<variable::Variable<S>> {
            Ok(variable::Variable::Int(variable::TypedVariable::new(
                |state: &S, _: u32| -> &i32 { &state.component().year },
                |state: &mut S, _: u32| -> &mut i32 { &mut state.component_mut().year },
                0,
            )))
        },
        0,
    )
}
