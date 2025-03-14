//! Terminal coloring
//!
//! The Texcraft project uses the
//! [Colored crate](https://docs.rs/colored/latest/colored/) for terminal coloring.
//! However, per [Texcraft's dependency policy](https://texcraft.dev/governance/dependencies.html),
//! use of this crate must be behind a Cargo feature.
//! In the case of Colored, the Cargo feature is `color`.
//!
//! This module implements the Cargo feature.
//!
//! The module contains a single trait [`Colorize`].
//! When the Cargo feature is enabled, this trait is essentially identical to
//!     the Colored crate's `Colorize` trait.
//! Specifically, it just forwards all method calls to the Colored crate.
//! When the Cargo feature is disabled, the trait is a no-op.
//! In both cases, downstream code can just call methods on the trait:
//!
//! ```
//! use texcraft_stdext::color::Colorize;
//! println!["{}", "Hello, World".bold().bright_red()];
//! ```

#[cfg(feature = "color")]
pub type ColoredString = colored::ColoredString;

#[cfg(not(feature = "color"))]
pub type ColoredString = &str;

macro_rules! colorize_impl {
    ( $( $method_name: ident, )+ ) => {
        /// Trait that provides coloring methods on strings.
        ///
        /// See the module documentation for information.
        pub trait Colorize {
            $(
                fn $method_name(self) -> ColoredString;
            )+
        }
        #[cfg(feature="color")]
        impl Colorize for ColoredString {
            $(
                fn $method_name(self) -> ColoredString {
                    colored::Colorize::$method_name(self)
                }
            )+
        }
        #[cfg(feature="color")]
        impl Colorize for &str {
            $(
                fn $method_name(self) -> ColoredString {
                    colored::Colorize::$method_name(self)
                }
            )+
        }
        #[cfg(not(feature="color"))]
        impl Colorize for &str {
            $(
                fn $method_name(self) -> ColoredString {
                    self
                }
            )+
        }

    };
}

colorize_impl!(
    bold,
    bright_cyan,
    bright_blue,
    bright_red,
    bright_yellow,
    italic,
);
