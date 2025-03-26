//! Box language abstract syntax tree
//!
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
    let calls = cst::Tree::build(cst::parse_using_lexer(lexer, errs.clone()));
    let v = convert(&calls.calls, &errs);
    errs.check()?;
    Ok(v)
}

fn convert<'a>(calls: &[cst::FuncCall<'a>], errs: &ErrorAccumulator<'a>) -> Vec<Horizontal<'a>> {
    let mut v: Vec<Horizontal> = vec![];
    for call in calls {
        if let Some(elem) = convert_call(call, errs) {
            v.push(elem);
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
            fn assign_to_field(&mut self, field_name: Str<$lifetime>, arg: &cst::Arg<$lifetime>, call: &cst::FuncCall<$lifetime>, errs: &ErrorAccumulator<$lifetime>)
            {
                match field_name.str() {
                $(
                    stringify!($field_name) => {
                        self.$field_name.assign(arg, field_name.str(), call, errs);
                    }
                )+
                    _ => {
                        errs.add(Error::NoSuchArgument{function_name: call.func_name.clone(), argument: field_name });
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
            call: &cst::FuncCall<'a>,
            errs: &ErrorAccumulator<'a>,
        ) -> Option<Horizontal<'a>> {
            let h = match call.func_name.str() {
                $( $(
                    $func_name => Horizontal::$variant($name::build(&call, errs)?),
                )? )+
                _ => {
                    errs.add(Error::NoSuchFunction {
                        function_name: call.func_name.clone(),
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
    fn assign_to_field(
        &mut self,
        field_name: Str<'b>,
        arg: &cst::Arg<'b>,
        call: &cst::FuncCall<'b>,
        errs: &ErrorAccumulator<'b>,
    );

    fn build(call: &cst::FuncCall<'b>, errs: &ErrorAccumulator<'b>) -> Option<Self> {
        let mut p: Self = Default::default();
        let start = errs.len();
        let mut field_names = Self::FIELD_NAMES.iter();
        let mut last_keyword_arg: Option<Str> = None;
        for arg in &call.args {
            let field_name = match &arg.key {
                // Positional argument
                None => {
                    if let Some(keyword_arg) = &last_keyword_arg {
                        errs.add(Error::PositionalArgAfterKeywordArg {
                            positional_arg: arg.value_source.clone(),
                            keyword_arg: keyword_arg.clone(),
                        });
                        continue;
                    }
                    let Some(field_name) = field_names.next() else {
                        errs.add(Error::TooManyPositionalArgs {
                            extra_positional_arg: arg.value_source.clone(),
                            function_name: call.func_name.clone(),
                            max_positional_args: Self::FIELD_NAMES.len(),
                        });
                        continue;
                    };
                    Str::new(field_name)
                }
                // Keyword argument
                Some(field_name) => {
                    last_keyword_arg = Some(Str {
                        end: arg.value_source.end,
                        ..*field_name
                    });
                    field_name.clone()
                }
            };
            p.assign_to_field(field_name, arg, call, errs);
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
    fn assign(
        &mut self,
        arg: &cst::Arg<'a>,
        field_name: &'a str,
        call: &cst::FuncCall<'a>,
        errs: &ErrorAccumulator<'a>,
    ) {
        if let Some(first_assignment) = &self.source {
            errs.add(Error::DuplicateArgument {
                parameter_name: field_name,
                first_assignment: first_assignment.clone(),
                second_assignment: arg.value_source.clone(),
            });
            return;
        }
        match T::try_cast_complex(&arg.value, errs) {
            Ok(val) => {
                self.value = val;
                self.source = Some(arg.value_source.clone());
            }
            Err(err) => errs.add(Error::IncorrectType {
                wanted_type: err.want,
                got_type: err.got,
                got_raw_value: arg.value_source.clone(),
                function_name: call.func_name.clone(),
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
    /// Try to cast a [`cst::Value`] to this type.
    fn try_cast(value: &cst::Value<'a>) -> Result<Self, TryCastError>;

    /// Try to cast the value to this type in cases where multiple errors can occur.
    fn try_cast_complex(
        value: &cst::Value<'a>,
        errs: &ErrorAccumulator<'a>,
    ) -> Result<Self, TryCastError> {
        _ = errs;
        Self::try_cast(value)
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
    fn try_cast(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        match value {
            cst::Value::String(s) => Ok(s.clone()),
            _ => Err(TryCastError {
                got: value.description(),
                want: "a string",
            }),
        }
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::String(self.clone())
    }
}

impl<'a> Value<'a> for i32 {
    fn try_cast(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        match value {
            cst::Value::Integer(i) => Ok(*i),
            _ => Err(TryCastError {
                got: value.description(),
                want: "an integer",
            }),
        }
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::Integer(*self)
    }
}

impl<'a> Value<'a> for core::Scaled {
    fn try_cast(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        match value {
            cst::Value::Scaled(i) => Ok(*i),
            _ => Err(TryCastError {
                got: value.description(),
                want: "a number",
            }),
        }
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::Scaled(*self)
    }
}

impl<'a> Value<'a> for (core::Scaled, core::GlueOrder) {
    fn try_cast(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        match value {
            cst::Value::Scaled(i) => Ok((*i, core::GlueOrder::Normal)),
            cst::Value::InfiniteGlue(s, o) => Ok((*s, *o)),
            _ => Err(TryCastError {
                got: value.description(),
                want: "a stretch or shrink glue component",
            }),
        }
    }
    fn lower(&self) -> cst::Value<'a> {
        cst::Value::InfiniteGlue(self.0, self.1)
    }
}

impl<'a> Value<'a> for Vec<Horizontal<'a>> {
    fn try_cast(value: &cst::Value<'a>) -> Result<Self, TryCastError> {
        _ = value;
        unimplemented!("must call try_cast_complex")
    }
    fn try_cast_complex(
        value: &cst::Value<'a>,
        errs: &ErrorAccumulator<'a>,
    ) -> Result<Self, TryCastError> {
        match value {
            cst::Value::List(l) => Ok(convert(&l.calls, errs)),
            _ => Err(TryCastError {
                got: value.description(),
                want: "a list",
            }),
        }
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
