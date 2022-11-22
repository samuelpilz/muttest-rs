#![cfg_attr(test, allow(clippy::bool_assert_comparison))]
//! Rust Mutation Testing core library.
//!
//! There are some internals here that are not meant for mutation testing users.

use std::{
    fmt,
    fs::File,
    io,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use lazy_static::lazy_static;
use serde::Serialize;

use crate::{
    context::{IMuttestContext, MuttestContext},
    mutable_id::{BakedMutableId, CrateLocalMutableId, MutableId},
};

pub mod context;
pub mod mutable;
pub mod mutable_id;
pub mod report;
pub mod transformer;

#[cfg(test)]
pub(crate) mod tests;

/// a module for reexport from `muttest` crate
pub mod api {
    pub use crate::mutable;
    pub use crate::mutable_id::BakedMutableId;
    pub use crate::{BakedLocation, LineColumn, Span};

    pub use std::{
        borrow::Cow, column, concat, file, line, marker::PhantomData, module_path,
        ops::ControlFlow, option::Option, sync::RwLock,
    };

    pub fn phantom_for_type<T>(_: &T) -> PhantomData<T> {
        PhantomData
    }

    pub fn id<T>(t: T) -> T {
        t
    }

    /// make a correct string of possible mutations from a (&str,bool) tuple list
    pub fn mutation_string_from_bool_list(l: &[(&'static str, bool)]) -> String {
        l.iter()
            .filter(|(_, ok)| *ok)
            .map(|(m, _)| *m)
            .collect::<Vec<_>>()
            .join(":")
    }
    pub fn mutation_string_opt(mutation: &str, opt: bool) -> &str {
        if opt {
            mutation
        } else {
            ""
        }
    }
}

#[macro_export]
macro_rules! env_var_muttest_dir {
    () => {
        "MUTTEST_DIR"
    };
}

// recompile this crate for selftest
#[cfg(test)]
const _RECOMPILE_ON_ENVVAR_CHANGE: Option<&str> = option_env!(env_var_muttest_dir!());

lazy_static! {
    static ref MUTTEST_CONTEXT: Option<MuttestContext<File>> =
        MuttestContext::new_from_env().expect("unable to create muttest context");
}

#[derive(Debug)]
pub struct Mutation {
    mutation: Option<Arc<str>>,
    skip: bool,
    #[cfg(test)]
    _nesting_token: Option<crate::tests::NestingToken>,
}

impl Mutation {
    pub fn new_from_option(mutation: Option<Arc<str>>) -> Self {
        Self {
            mutation,
            skip: false,
            #[cfg(test)]
            _nesting_token: None,
        }
    }
    pub fn new_skip() -> Self {
        Self {
            skip: true,
            ..Self::new_from_option(None)
        }
    }
    pub fn as_option(&self) -> Option<&str> {
        self.mutation.as_deref()
    }
    pub fn is_skip(&self) -> bool {
        self.skip
    }
}

impl Default for Mutation {
    fn default() -> Self {
        Self::new_skip()
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
    #[error("failed to read csv file: {0}")]
    Csv(#[from] csv::Error),
    #[error("failed to read csv file {0}: {1}")]
    CsvFile(PathBuf, csv::Error),
    #[error("invalid mutation format: '{0}'")]
    MutationFormat(String),
    #[error("invalid target format: '{0}'")]
    TargetFormat(String),
    #[error("invalid MutableId: '{0}'")]
    MutableIdFormat(String),
    #[error("not a known mutable id: '{0}'")]
    UnknownMutableId(MutableId),
    #[error("not a known mutable id: '{0}'")]
    UnknownCrateLocalMutableId(CrateLocalMutableId),
    #[error("not a known mutate attr id: '{0}'")]
    UnknownMutateAttr(usize),
    #[error("unknown mutable kind {0}")]
    UnknownMutableKind(String),
    #[error("invalid location: '{0}'")]
    LocationFormat(String),
    #[error("invalid path: '{0}'")]
    PathFormat(String),
}
impl Error {
    pub fn in_csv_file(self, f: impl AsRef<Path>) -> Self {
        match self {
            Self::Csv(e) => Self::CsvFile(f.as_ref().to_owned(), e),
            s => s,
        }
    }
}

impl BakedMutableId {
    fn context(self) -> Option<impl context::IMuttestContext> {
        #[cfg(test)]
        return self.test_context();
        #[cfg(not(test))]
        MUTTEST_CONTEXT
            .as_ref()
            .filter(|ctx| ctx.tracks_mutable(self))
    }

    /// reports details of mutables gathered by static analysis
    pub fn report_details(self, loc: BakedLocation, ty: &str, mutations: &str) {
        if let Some(c) = self.context() {
            c.write_details(self.crate_local_id(), loc, ty, mutations)
        }
    }

    pub fn report_coverage(self, behavior: Option<&str>) {
        if let Some(c) = self.context() {
            c.write_coverage(self.crate_local_id(), behavior)
        }
    }

    /// get the active mutation for a mutable
    pub fn get_active_mutation(self) -> Mutation {
        #[cfg(test)]
        if self.attr_id != 0 {
            return self.get_selftest_mutation();
        }

        self.context()
            .map(|ctx| {
                Mutation::new_from_option(ctx.mutations().get(&self.crate_local_id()).cloned())
            })
            .unwrap_or_default()
    }
}

// TODO: remove this when proc-macros can access more location info
pub struct BakedLocation {
    pub file: &'static str,
    pub module: &'static str,
    pub attr_span: Span,
    pub span: Span,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Span {
    pub start: LineColumn,
    pub end: Option<LineColumn>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum PathSegment {
    Mod(String),
    Fn(String),
    Impl(String),
}

impl Span {
    fn from(span: proc_macro2::Span) -> Option<Self> {
        let start = span.start();
        if start.line == 0 {
            return None;
        }
        let end = Some(span.end()).filter(|&end| end.line != 0 && start != end);
        // TODO: report errors
        Some(Self {
            start: LineColumn {
                line: start.line.try_into().unwrap(),
                column: start.column.try_into().unwrap(),
            },
            end: end.map(|end| LineColumn {
                line: end.line.try_into().unwrap(),
                column: end.column.try_into().unwrap(),
            }),
        })
    }
}

pub fn display_or_empty_if_none(d: &Option<impl fmt::Display>) -> &dyn fmt::Display {
    match d {
        Some(d) => d,
        None => &"",
    }
}
fn parse_or_none_if_empty<T: FromStr>(s: &str) -> Result<Option<T>, <T as FromStr>::Err> {
    if s.is_empty() {
        Ok(None)
    } else {
        Ok(Some(s.parse()?))
    }
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegment::Mod(i) => write!(f, "mod {i}"),
            PathSegment::Fn(i) => write!(f, "fn {i}"),
            PathSegment::Impl(i) => write!(f, "impl {i}"),
        }
    }
}
impl FromStr for PathSegment {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(' ') {
            Some(("mod", i)) => Ok(Self::Mod(i.to_owned())),
            Some(("fn", i)) => Ok(Self::Fn(i.to_owned())),
            Some(("impl", i)) => Ok(Self::Impl(i.to_owned())),
            _ => Err(Error::PathFormat(s.to_owned())),
        }
    }
}

impl FromStr for Span {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let start;
        let mut end = None;
        match s.split_once('-') {
            Some((s, e)) => {
                start = s.parse()?;
                end = Some(e.parse()?);
            }
            None => {
                start = s.parse()?;
            }
        }

        Ok(Self { start, end })
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start.line, self.start.column)?;
        if let Some(end) = self.end {
            write!(f, "-{}:{}", end.line, end.column)?;
        }
        Ok(())
    }
}

impl FromStr for LineColumn {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (l, c) = s
            .split_once(':')
            .ok_or_else(|| Error::LocationFormat(s.to_owned()))?;
        Ok(LineColumn {
            line: l.parse().map_err(|_| Error::LocationFormat(s.to_owned()))?,
            column: c.parse().map_err(|_| Error::LocationFormat(s.to_owned()))?,
        })
    }
}
