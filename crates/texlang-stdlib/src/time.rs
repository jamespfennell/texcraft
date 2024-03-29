//! Time commands (`\year`, `\month`, etc.)

#[cfg(feature = "time")]
use chrono::prelude::*;
use texlang::{command, variable, vm::HasComponent};

/// Component for storing state related to the time commands.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    minutes_since_midnight: i32,
    day: i32,
    month: i32,
    year: i32,
}

#[cfg(feature = "time")]
impl Default for Component {
    fn default() -> Self {
        let dt: DateTime<Local> = Local::now();
        Self {
            minutes_since_midnight: 60 * (dt.time().hour() as i32) + (dt.time().minute() as i32),
            day: dt.day() as i32,
            month: dt.month() as i32,
            year: dt.year(),
        }
    }
}

#[cfg(not(feature = "time"))]
impl Default for Component {
    fn default() -> Self {
        Self {
            minutes_since_midnight: 0,
            day: 0,
            month: 0,
            year: 0,
        }
    }
}

impl Component {
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

/// Get the `\time` command.
pub fn get_time<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_singleton(
        |state: &S, _: variable::Index| -> &i32 { &state.component().minutes_since_midnight },
        |state: &mut S, _: variable::Index| -> &mut i32 {
            &mut state.component_mut().minutes_since_midnight
        },
    )
    .into()
}

/// Get the `\day` command.
pub fn get_day<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_singleton(
        |state: &S, _: variable::Index| -> &i32 { &state.component().day },
        |state: &mut S, _: variable::Index| -> &mut i32 { &mut state.component_mut().day },
    )
    .into()
}

/// Get the `\month` command.
pub fn get_month<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_singleton(
        |state: &S, _: variable::Index| -> &i32 { &state.component().month },
        |state: &mut S, _: variable::Index| -> &mut i32 { &mut state.component_mut().month },
    )
    .into()
}

/// Get the `\year` command.
pub fn get_year<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_singleton(
        |state: &S, _: variable::Index| -> &i32 { &state.component().year },
        |state: &mut S, _: variable::Index| -> &mut i32 { &mut state.component_mut().year },
    )
    .into()
}
