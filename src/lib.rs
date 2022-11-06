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
use mutable_id::{BakedMutableId, CrateLocalMutableId, MutableId};
use report::{LineColumn, Span};

use crate::context::MuttestContext;

pub mod context;
pub mod mutable;
mod mutable_id;
pub mod report;
pub mod transformer;

#[cfg(test)]
pub(crate) mod tests;

/// a module for reexport from `muttest` crate
pub mod api {
    pub use crate::mutable;
    pub use crate::mutable_id::*;

    // TODO: restructure these types
    pub use crate::report::{LineColumn, Span};
    pub use crate::BakedLocation;

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

// recompile core for selftest
#[cfg(test)]
#[allow(dead_code)]
const RECOMPILE_ON_ENVVAR_CHANGE: Option<&str> = option_env!(env_var_muttest_dir!());

lazy_static! {
    static ref MUTTEST_CONTEXT: Option<MuttestContext<File>> =
        MuttestContext::new_from_env().expect("unable to create muttest context");
}

#[derive(Debug)]
pub enum Mutation {
    Unchanged,
    Mutate(Arc<str>),
}

impl Mutation {
    pub fn as_option(&self) -> Option<&str> {
        match self {
            Mutation::Unchanged => None,
            Mutation::Mutate(m) => Some(&*m),
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

pub struct BakedLocation {
    pub file: &'static str,
    pub module: &'static str,
    pub attr_span: Span,
    pub span: Span,
    // TODO: add more end location info for feature proc_macro_span / proc_macro_span_shrink
}

// TODO: this should be done cleaner
macro_rules! with_context {
    ($s:expr, $f:ident($($args:expr),*) $(, $d:expr)?) => {
        match &*MUTTEST_CONTEXT {
            #[cfg(test)]
            _ if $s.is_isolated() => $s.test_context().$f($($args,)*),
            Some(ctx) if ctx.tracks_mutable($s) => ctx.$f($($args,)*),
            _ => {$($d)?}
        }
    };
}
// TODO: should this be method-functions
impl BakedMutableId {
    /// reports details of mutables gathered by static analysis
    pub fn report_details(self, loc: BakedLocation, ty: &str, mutations: &str) {
        with_context!(self, write_details(self.id, loc, ty, mutations))
    }

    // TODO: ensure this function is used everywhere
    pub fn report_coverage(self, behavior: Option<&str>) {
        with_context!(self, write_coverage(self.id, behavior));
    }

    /// get the active mutation for a mutable
    fn get_active_mutation(self) -> Mutation {
        with_context!(self, get_mutation(self.id), Mutation::Unchanged)
    }
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

fn display_or_empty_if_none(d: &Option<impl fmt::Display>) -> &dyn fmt::Display {
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
