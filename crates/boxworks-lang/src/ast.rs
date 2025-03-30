//! Box language abstract syntax tree
//!
use crate::cst::TreeIter;
use crate::ErrorAccumulator;

use super::cst;
use super::error::Error;
use super::lexer;
use super::Str;
use std::borrow::Cow;

/// Element of a horizontal list
#[derive(Debug, PartialEq, Eq)]
pub enum Horizontal<'a> {
    Text(Text<'a>),
    Glue(Glue<'a>),
    Kern(Kern<'a>),
    Hlist(Hlist<'a>),
}

impl<'a> Horizontal<'a> {
    pub fn lower(&self) -> cst::FuncCall<'a> {
        lower_horizontal(self)
    }
}

impl<'a> std::fmt::Display for Horizontal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lower())
    }
}

/// Parse Box language source code into a horizontal list.
pub fn parse_horizontal_list(source: &str) -> Result<Vec<Horizontal>, Vec<Error>> {
    let errs: ErrorAccumulator = Default::default();
    let lexer = lexer::Lexer::new(source, errs.clone());
    let calls = cst::parse_using_lexer(lexer, errs.clone());
    let v = convert(calls, &errs);
    errs.check()?;
    Ok(v)
}

fn convert<'a>(calls: impl cst::TreeIter<'a>, errs: &ErrorAccumulator<'a>) -> Vec<Horizontal<'a>> {
    let mut v: Vec<Horizontal> = vec![];
    for call in calls {
        match call {
            cst::TreeItem::FuncCall { func_name, iter } => {
                if let Some(elem) = convert_call(func_name, iter, errs) {
                    v.push(elem);
                }
            }
            cst::TreeItem::Comment { value: _ } => continue,
        }
    }
    v
}

macro_rules! functions {
    ( $( (
        struct $name: ident <$lifetime: lifetime>  {
            $(
                $field_name: ident : $field_type: ty,
            )+
        }
        $(
            impl Horizontal {
                func_name: $func_name: expr,
                variant: $variant: ident,
            }
        )?
    ), )+ ) => {
        $(
        #[derive(Debug, Default, PartialEq, Eq)]
        pub struct $name <$lifetime> {
            $(
                pub $field_name : Arg<$lifetime, $field_type>,
            )+
        }
        impl<$lifetime> Args<$lifetime> for $name <$lifetime> {
            const FIELD_NAMES: &'static[&'static str] = &[ $( stringify!($field_name), )+];
            fn assign_to_field<T: cst::TreeIter<$lifetime>>(&mut self, field_name: Str<$lifetime>, arg: cst::ArgsItem<$lifetime, T>, func_name: Str<$lifetime>, value_source: Str<$lifetime>,  errs: &ErrorAccumulator<$lifetime>)
            {
                match field_name.str() {
                $(
                    stringify!($field_name) => {
                        self.$field_name.assign(arg, field_name.str(), func_name.clone(), value_source, errs);
                    }
                )+
                    _ => {
                        errs.add(Error::NoSuchArgument{function_name: func_name.clone(), argument: field_name });
                    }
                }
            }
            fn lower(&self) -> Vec<cst::Arg<$lifetime>> {
                vec![
                    $(
                        self.$field_name.lower(Some(stringify!($field_name).into())),
                    )+
                ]
            }
        }
        )+

        fn convert_call<'a>(
            func_name: Str<'a>,
            call: impl cst::ArgsIter<'a>,
            errs: &ErrorAccumulator<'a>,
        ) -> Option<Horizontal<'a>> {
            let h = match func_name.str() {
                $( $(
                    $func_name => Horizontal::$variant($name::build(func_name, call, errs)?),
                )? )+
                _ => {
                    errs.add(Error::NoSuchFunction {
                        function_name: func_name.clone(),
                    });
                    return None;
                }
            };
            Some(h)
        }
        fn lower_horizontal<'a>(h: &Horizontal<'a>) -> cst::FuncCall<'a> {
            match h {
                $( $(
                    Horizontal::$variant(args) => cst::FuncCall {
                        comments: vec![],
                        func_name: $func_name.into(),
                        args: args.lower(),
                        trailing_comments: vec![],
                    },
                )? )+
            }
        }
    };
}

/// Concrete strongly-type arguments to a function.
trait Args<'b>: Default {
    const FIELD_NAMES: &'static [&'static str];
    fn assign_to_field<T: cst::TreeIter<'b>>(
        &mut self,
        field_name: Str<'b>,
        arg: cst::ArgsItem<'b, T>,
        func_name: Str<'b>,
        value_source: Str<'b>,
        errs: &ErrorAccumulator<'b>,
    );

    fn build(
        func_name: Str<'b>,
        args: impl cst::ArgsIter<'b>,
        errs: &ErrorAccumulator<'b>,
    ) -> Option<Self> {
        let mut p: Self = Default::default();
        let start = errs.len();
        let mut field_names = Self::FIELD_NAMES.iter();
        let mut last_keyword_arg: Option<Str> = None;
        for arg in args {
            let (key, value_source) = match &arg {
                cst::ArgsItem::Regular {
                    key,
                    value: _,
                    value_source,
                } => (key, value_source.clone()),
                cst::ArgsItem::List {
                    key,
                    square_open: _,
                    iter,
                } => (key, iter.remaining_source()),
                cst::ArgsItem::Comment { .. } => continue,
            };
            let field_name = match key {
                // Positional argument
                None => {
                    if let Some(keyword_arg) = &last_keyword_arg {
                        errs.add(Error::PositionalArgAfterKeywordArg {
                            positional_arg: value_source,
                            keyword_arg: keyword_arg.clone(),
                        });
                        continue;
                    }
                    let Some(field_name) = field_names.next() else {
                        errs.add(Error::TooManyPositionalArgs {
                            extra_positional_arg: value_source,
                            function_name: func_name.clone(),
                            max_positional_args: Self::FIELD_NAMES.len(),
                        });
                        continue;
                    };
                    Str::new(field_name)
                }
                // Keyword argument
                Some(field_name) => {
                    last_keyword_arg = Some(Str {
                        end: value_source.end,
                        ..*field_name
                    });
                    field_name.clone()
                }
            };
            p.assign_to_field(field_name, arg, func_name.clone(), value_source, errs);
        }
        if errs.len() == start {
            Some(p)
        } else {
            None
        }
    }

    fn lower(&self) -> Vec<cst::Arg<'b>>;
}

functions!(
    (
        struct Text<'a> {
            content: Cow<'a, str>,
            font: i32,
        }
        impl Horizontal {
            func_name: "text",
            variant: Text,
        }
    ),
    (
        struct Glue<'a> {
            width: core::Scaled,
            stretch: (core::Scaled, core::GlueOrder),
            shrink: (core::Scaled, core::GlueOrder),
        }
        impl Horizontal {
            func_name: "glue",
            variant: Glue,
        }
    ),
    (
        struct Kern<'a> {
            width: core::Scaled,
        }
        impl Horizontal {
            func_name: "kern",
            variant: Kern,
        }
    ),
    (
        struct Hlist<'a> {
            width: core::Scaled,
            content: Vec<Horizontal<'a>>,
        }
        impl Horizontal {
            func_name: "hlist",
            variant: Hlist,
        }
    ),
);

/// An argument of type `T` to a function.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Arg<'a, T> {
    /// Value of the argument.
    pub value: T,
    /// Source of the argument in Box source code.
    ///
    /// If this is [`None`], a value was not provided in source
    ///     code and the default value is being used.
    pub source: Option<Str<'a>>,
}

impl<'a, T> From<T> for Arg<'a, T> {
    fn from(value: T) -> Self {
        Arg {
            value,
            source: None,
        }
    }
}

// I think this private_bounds warning is a Rust bug?
// All of the impl methods are private to this module so the bound
// can't be seen from outside the module.
#[allow(private_bounds)]
impl<'a, T> Arg<'a, T>
where
    T: Value<'a>,
{
    fn assign<F: cst::TreeIter<'a>>(
        &mut self,
        arg: cst::ArgsItem<'a, F>,
        field_name: &'a str,
        func_name: Str<'a>,
        value_source: Str<'a>,
        errs: &ErrorAccumulator<'a>,
    ) {
        if let Some(first_assignment) = &self.source {
            errs.add(Error::DuplicateArgument {
                parameter_name: field_name,
                first_assignment: first_assignment.clone(),
                second_assignment: value_source,
            });
            return;
        }
        let cast_result = match arg {
            cst::ArgsItem::Regular { value, .. } => T::try_cast_from_value(&value),
            cst::ArgsItem::List { iter, .. } => T::try_cast_from_list(iter, errs),
            cst::ArgsItem::Comment { .. } => return,
        };
        match cast_result {
            Ok(val) => {
                self.value = val;
                self.source = Some(value_source.clone());
            }
            Err(err) => errs.add(Error::IncorrectType {
                wanted_type: err.want,
                got_type: err.got,
                got_raw_value: value_source.clone(),
                function_name: func_name.clone(),
                parameter_name: field_name,
            }),
        }
    }
    fn lower(&self, key: Option<Str<'a>>) -> cst::Arg<'a> {
        cst::Arg {
            comments: vec![],
            key,
            value: self.value.lower(),
            value_source: self.source.clone().unwrap_or("".into()),
        }
    }
}

/// Values in the AST.
///
/// These can possibly be obtained from a [`cst::Value`]
///     and always lowered to a [`cst::Value`].
trait Value<'a>: Sized {
    const DESCRIPTION: &'static str;

    /// Try to cast a [`cst::Value`] to this type.
    fn try_cast_from_value(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        Self::type_mismatch_error(value.description())
    }

    /// Try to cast the value to this type in cases where multiple errors can occur.
    fn try_cast_from_list<F: cst::TreeIter<'a>>(
        value: F,
        errs: &ErrorAccumulator<'a>,
    ) -> Result<Self, TryCastError> {
        _ = value;
        _ = errs;
        Self::type_mismatch_error("a list")
    }

    fn type_mismatch_error(got: &'static str) -> Result<Self, TryCastError> {
        Err(TryCastError {
            got,
            want: Self::DESCRIPTION,
        })
    }

    /// Lower this value to a [`cst::Value`].
    fn lower(&self) -> cst::Value<'a>;
}

/// Error created when casting a value to a concrete type fails.
struct TryCastError {
    pub got: &'static str,
    pub want: &'static str,
}

impl<'a> Value<'a> for Cow<'a, str> {
    const DESCRIPTION: &'static str = "a string";
    fn try_cast_from_value(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        match value {
            cst::Value::String(s) => Ok(s.clone()),
            _ => Self::type_mismatch_error(value.description()),
        }
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::String(self.clone())
    }
}

impl<'a> Value<'a> for i32 {
    const DESCRIPTION: &'static str = "an integer";
    fn try_cast_from_value(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        match value {
            cst::Value::Integer(i) => Ok(*i),
            _ => Self::type_mismatch_error(value.description()),
        }
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::Integer(*self)
    }
}

impl<'a> Value<'a> for core::Scaled {
    const DESCRIPTION: &'static str = "a number";
    fn try_cast_from_value(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        match value {
            cst::Value::Scaled(i) => Ok(*i),
            _ => Self::type_mismatch_error(value.description()),
        }
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::Scaled(*self)
    }
}

impl<'a> Value<'a> for (core::Scaled, core::GlueOrder) {
    const DESCRIPTION: &'static str = "a stretch or shrink glue component";
    fn try_cast_from_value(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        match value {
            cst::Value::Scaled(i) => Ok((*i, core::GlueOrder::Normal)),
            cst::Value::InfiniteGlue(s, o) => Ok((*s, *o)),
            _ => Self::type_mismatch_error(value.description()),
        }
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::InfiniteGlue(self.0, self.1)
    }
}

impl<'a> Value<'a> for Vec<Horizontal<'a>> {
    const DESCRIPTION: &'static str = "a list";
    fn try_cast_from_list<F: cst::TreeIter<'a>>(
        value: F,
        errs: &ErrorAccumulator<'a>,
    ) -> Result<Self, TryCastError> {
        Ok(convert(value, errs))
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::List(cst::Tree {
            calls: self.iter().map(|e| e.lower()).collect(),
            trailing_comments: vec![],
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_hlist() {
        let input = r#"
            hlist(
                width=1pt,
                content=[text("Hello")],
            )
        "#;

        let want = vec![Horizontal::Hlist(Hlist {
            width: Arg {
                value: core::Scaled::ONE.into(),
                source: Some("1pt".into()),
            },
            content: Arg {
                value: vec![Horizontal::Text(Text {
                    content: Arg {
                        value: "Hello".into(),
                        source: Some(r#""Hello""#.into()),
                    },
                    font: Arg {
                        value: 0,
                        source: None,
                    },
                })],
                source: Some(r#"[text("Hello")]"#.into()),
            },
        })];

        let got = parse_horizontal_list(&input).expect("parsing succeeds");

        assert_eq!(got, want);
    }
}
