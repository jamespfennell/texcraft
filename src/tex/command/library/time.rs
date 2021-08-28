//! Variable commands related to time.

use crate::tex::variable;
use chrono::prelude::*;

/// Trait for states that contain a [time Component](Component).
pub trait HasTime {
    fn get_time(&self) -> &Component;
    fn get_time_mut(&mut self) -> &mut Component;
}

/// Component for storing state related to the time commands.
pub struct Component {
    minutes_since_midnight: i32,
    day: i32,
    month: i32,
    year: i32,
}

impl Component {
    /// Create a new component with the variables initialized to the current time.
    pub fn new() -> Component {
        let dt: DateTime<Local> = Local::now();
        Component {
            minutes_since_midnight: 60 * (dt.time().hour() as i32) + (dt.time().minute() as i32),
            day: dt.date().day() as i32,
            month: dt.date().month() as i32,
            year: dt.date().year(),
        }
    }
}

fn read_minutes_since_midnight<S: HasTime>(state: &S, _: usize) -> &i32 {
    &state.get_time().minutes_since_midnight
}

fn write_minutes_since_midnight<S: HasTime>(state: &mut S, _: usize) -> &mut i32 {
    &mut state.get_time_mut().minutes_since_midnight
}

/// Get the `\time` command.
pub fn get_time<S: HasTime>() -> variable::Variable<S> {
    variable::Variable::Int(variable::TypedVariable::new(
        read_minutes_since_midnight,
        write_minutes_since_midnight,
        0,
    ))
}

fn read_day<S: HasTime>(state: &S, _: usize) -> &i32 {
    &state.get_time().day
}

fn write_day<S: HasTime>(state: &mut S, _: usize) -> &mut i32 {
    &mut state.get_time_mut().day
}

/// Get the `\day` command.
pub fn get_day<S: HasTime>() -> variable::Variable<S> {
    variable::Variable::Int(variable::TypedVariable::new(read_day, write_day, 0))
}

fn read_month<S: HasTime>(state: &S, _: usize) -> &i32 {
    &state.get_time().month
}

fn write_month<S: HasTime>(state: &mut S, _: usize) -> &mut i32 {
    &mut state.get_time_mut().month
}

/// Get the `\month` command.
pub fn get_month<S: HasTime>() -> variable::Variable<S> {
    variable::Variable::Int(variable::TypedVariable::new(read_month, write_month, 0))
}

fn read_year<S: HasTime>(state: &S, _: usize) -> &i32 {
    &state.get_time().year
}

fn write_year<S: HasTime>(state: &mut S, _: usize) -> &mut i32 {
    &mut state.get_time_mut().year
}

/// Get the `\year` command.
pub fn get_year<S: HasTime>() -> variable::Variable<S> {
    variable::Variable::Int(variable::TypedVariable::new(read_year, write_year, 0))
}
