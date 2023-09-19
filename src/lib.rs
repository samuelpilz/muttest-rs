#![cfg_attr(test, allow(clippy::bool_assert_comparison))]
#![cfg_attr(feature = "selftest", allow(clippy::all))]
#![allow(clippy::wrong_self_convention)]
//! Rust Mutation Testing core library. The internals defined here are not meant for users of the `muttest` library.
//!
//! This crate is structured as follows:
//! - The module `mutables` and its submodules define and describe the behavior of mutables.
//! - The module `tests` defines infrastructure for unit-testing the mutables and the infrastructure.

use std::{
    fmt::{self, Display},
    io,
    ops::ControlFlow,
    path::{Path, PathBuf},
    sync::MutexGuard,
    time::{Duration, Instant},
};

use once_cell::sync::Lazy;
use report::TestId;

use crate::{
    context::MuttestContext,
    report::{CrateId, CrateLocalMutableId, MutableId, Span},
};

pub mod context;
pub mod mutator;
pub mod report;
pub mod transformer;

#[cfg(test)]
pub(crate) mod tests;

/// a module for reexport from `muttest` crate
pub mod api {
    pub use crate::mutator;
    pub use crate::report::{
        CrateId, CrateLocalMutableId, CrateLocalTestId, LineColumn, MutableId, Span, TestId,
    };
    pub use crate::transformer::test_transformer::TestSkip;
    pub use crate::{
        internal_error, write_attr_location, write_mutable_location, Error, MuttestTest,
        PossibleMutations,
    };

    pub use std::{
        borrow::Cow, column, concat, default::Default, file, line, marker::PhantomData,
        module_path, ops::ControlFlow, option::Option,
    };

    pub fn phantom_for_type<T>(_: &T) -> PhantomData<T> {
        PhantomData
    }

    pub fn type_name_of<T>(_: &T) -> &'static str {
        std::any::type_name::<T>()
    }

    pub fn id<T>(t: T) -> T {
        t
    }

    pub fn with_type<T>(t: T, _: PhantomData<T>) -> T {
        t
    }

    pub fn typed<T>(t: T) -> (T, PhantomData<T>) {
        (t, PhantomData)
    }

    pub fn phantom_unwrap<T>(_: PhantomData<T>) -> T {
        internal_error(Error::PhantomUnwrap)
    }
}

// recompile this crate for selftest
#[cfg(feature = "selftest")]
const _RECOMPILE_ON_ENVVAR_CHANGE: Option<&str> = option_env!("MUTTEST_DIR");

static MUTTEST_CONTEXT: Lazy<Option<MuttestContext>> = Lazy::new(|| {
    // create context. Return if no context is created
    let ctx = MuttestContext::new_from_env().unwrap_muttest_error()?;
    // start timeout watchdog thread
    std::thread::spawn(|| {
        let deadline = MUTTEST_CONTEXT
            .as_ref()
            .unwrap_or_else(|| internal_error(Error::ContextError))
            .test_deadline
            .as_ref()
            .unwrap_or_else(|| internal_error(Error::ContextError));
        while *deadline.lock().unwrap() > Instant::now() {
            std::thread::sleep(Duration::from_millis(100));
        }
        // TODO: improve test timeout reporting which test failed
        // let current_test = ctx.testcase.read().unwrap();
        // println!("timeout {:?}", current_test);
        std::process::exit(EXIT_TIMEOUT);
    });
    Some(ctx)
});

pub const EXIT_TIMEOUT: i32 = 124;
pub const EXIT_TEST_FAIL: i32 = 101;
pub const EXIT_MUTTEST_ERROR: i32 = 102;
pub const EXIT_TEST_FAIL_EARLY: i32 = 105;
pub const EXIT_ABORT: i32 = 134;

pub fn abort_internal_error() -> ! {
    // TODO: make unit-testable?
    #[cfg(not(feature = "selftest"))]
    std::process::exit(EXIT_MUTTEST_ERROR);
    #[cfg(feature = "selftest")]
    panic!()
}
pub fn internal_error(err: Error) -> ! {
    Err(err).unwrap_muttest_error()
}
pub trait UnwrapMuttestError<T> {
    fn unwrap_muttest_error(self) -> T;
}
impl<T> UnwrapMuttestError<T> for Result<T, Error> {
    fn unwrap_muttest_error(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => {
                eprintln!("{e:?}");
                use std::io::Write;
                let _ = std::io::stderr().flush();
                abort_internal_error()
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("unicode error env var {0}")]
    EnvVarUnicode(&'static str),
    #[error("env var {0} not defined")]
    EnvVarMissing(&'static str),
    #[error("env var {0} invalid")]
    EnvVarInvalid(&'static str),
    #[error("context construction failed")]
    ContextError,
    #[error("failed to read csv file: {0}")]
    Csv(#[from] csv::Error),
    #[error("failed to read csv file {0}: {1}")]
    CsvFile(PathBuf, Box<Error>),
    #[error("invalid mutation format: '{0}'")]
    MutationFormat(String),
    #[error("invalid target format: '{0}'")]
    TargetFormat(String),
    #[error("invalid Id: '{0}'")]
    IdFormat(String),
    #[error("not a known mutable id: '{0}'")]
    UnknownMutableId(MutableId),
    #[error("not a known mutable id: '{0}'")]
    UnknownCrateLocalMutableId(CrateLocalMutableId),
    #[error("not a known crate id: '{0}'")]
    UnknownCrateId(CrateId),
    #[error("not a known mutate attr id: '{0}'")]
    UnknownMutateAttr(u32),
    #[error("unknown mutable kind {0}")]
    UnknownMutableKind(String),
    #[error("invalid location: '{0}'")]
    LocationFormat(String),
    #[error("invalid path: '{0}'")]
    PathFormat(String),
    #[error("invalid mutation: '{0}'")]
    InvalidMutation(&'static str),
    #[error("specialized function call invalid {0}")]
    InvalidSpecialization(&'static str),
    #[error("invalid type {0}")]
    InvalidType(String),
    #[error("invalid coverage value {0}")]
    InvalidCoverageValue(String),
    #[error("invalid mode")]
    InvalidMode(String),
    #[error("phantom unwrap")]
    PhantomUnwrap,
    #[error("muttest attr error {0}")]
    Syn(#[from] syn::Error),
    #[error("mutator `{0}` transform error: {1}")]
    MutatorTransform(&'static str, syn::Error),
}
impl Error {
    pub fn in_csv_file(self, f: impl AsRef<Path>) -> Self {
        Self::CsvFile(f.as_ref().to_owned(), Box::new(self))
    }
}

pub fn write_attr_location(crate_id: CrateId, attr_id: u32, file: &str, span: Span) {
    if let Some(c) = MUTTEST_CONTEXT.as_ref() {
        c.write_attr_location(crate_id, attr_id, file, span)
    }
}

pub fn write_mutable_location(m_id: MutableId, span: Span) {
    if let Some(c) = MUTTEST_CONTEXT.as_ref() {
        c.write_mutable_location(m_id, span)
    }
}

impl MutableId {
    fn context(&self) -> Option<&'static MuttestContext> {
        #[cfg(test)]
        return self.test_context();
        #[cfg(not(test))]
        MUTTEST_CONTEXT.as_ref()
    }

    /// get the active mutation for a mutable
    pub fn get_action(&self) -> Option<&'static str> {
        self.context()?.get_action(self)
    }

    /// reports analysis of mutables gathered by static analysis
    pub fn write_types<T: Display>(&self, ty: &str, mutations: T) {
        if let Some(c) = self.context() {
            c.write_types(self, ty, mutations)
                .expect("unable to write types")
        }
    }

    pub fn write_coverage(&self) {
        if let Some(c) = self.context() {
            c.write_coverage::<&str>(self, None)
                .expect("unable to write coverage")
        }
    }
    pub fn write_coverage_behavior<C: Display>(&self, behavior: C) {
        if let Some(c) = self.context() {
            c.write_coverage(self, Some(behavior))
                .expect("unable to write coverage")
        }
    }
}

pub struct PossibleMutations<'a>(pub &'a [(&'static str, bool)]);
impl fmt::Display for PossibleMutations<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for &(m, ok) in self.0 {
            if ok {
                if !first {
                    write!(f, "\x1f")?;
                } else {
                    first = false;
                }
                write!(f, "{m}")?;
            }
        }
        Ok(())
    }
}

pub struct MuttestTest {
    pub id: TestId,
    pub name: &'static str,
    pub module: &'static str,
    pub file: &'static str,
    pub should_panic: bool,
    pub running: Option<MutexGuard<'static, ()>>,
}

impl MuttestTest {
    pub fn start(mut self) -> ControlFlow<(), Self> {
        if let Some(ctx) = &*MUTTEST_CONTEXT {
            self.running = ctx.start_test(self.id.clone())?;
        }
        ControlFlow::Continue(self)
    }
}

impl Drop for MuttestTest {
    fn drop(&mut self) {
        if self.running.is_none() {
            return; // test was skipped
        }

        // evaluate test results and clean
        let success = std::thread::panicking() == self.should_panic;
        if let Some(ctx) = &*MUTTEST_CONTEXT {
            let should_continue = ctx.end_test(&self.id, success);
            if should_continue.is_break() {
                std::process::exit(EXIT_TEST_FAIL_EARLY);
            }
        }
    }
}

pub fn display_or_empty_if_none(d: &Option<impl fmt::Display>) -> &dyn fmt::Display {
    match d {
        Some(d) => d,
        None => &"",
    }
}
