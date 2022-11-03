//! Rust Mutation Testing core library.
//!
//! There are some internals here that are not meant for mutation testing users.

use std::{
    collections::BTreeSet,
    fmt,
    fs::File,
    io,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};

use crate::context::MuttestContext;

pub mod context;
pub mod mutable;
pub mod transformer;

#[cfg(test)]
pub(crate) mod tests;

/// a module for reexport from `muttest` crate
pub mod api {
    pub use crate::mutable;
    pub use crate::{BakedLocation, BakedMutableId, LineColumn, Span};

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
    #[error("invalid MutableId: '{0}'")]
    MutableIdFormat(String),
    #[error("not a known mutable: '{0}'")]
    UnknownMutable(usize), // TODO: somehow encode crate_name?
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MutableData {
    pub kind: String,
    pub code: String,
    pub location: MutableLocation,
    pub details: Option<MutableDetails>,
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MutableDetails {
    pub mutable_type: String,
    pub possible_mutations: BTreeSet<String>,
}

#[derive(Debug, Deserialize)]
pub struct MutableCoverage {
    id: usize,
    data: String,
}

pub struct BakedLocation {
    pub file: &'static str,
    pub module: &'static str,
    pub attr_span: Span,
    pub span: Span,
    // TODO: add more end location info for feature proc_macro_span / proc_macro_span_shrink
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[allow(unused)]
pub struct MutableLocation {
    pub file: String,
    pub module: String,
    pub path: Vec<PathSegment>,
    pub attr_span: Option<Span>,
    pub span: Option<Span>,
}

// TODO: use proc-macro2 structs instead??
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: LineColumn,
    pub end: Option<LineColumn>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MutableId {
    pub crate_name: String,
    pub id: usize,
}
impl fmt::Display for MutableId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.id, self.crate_name)
    }
}
impl FromStr for MutableId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let id = s
            .split_once(':')
            .ok_or_else(|| Error::MutableIdFormat(s.to_owned()))?;
        Ok(Self {
            id: id
                .0
                .parse()
                .map_err(|_| Error::MutableIdFormat(s.to_owned()))?,
            crate_name: id.1.to_owned(),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BakedMutableId {
    // TODO: rename to `target`
    pub crate_name: &'static str,
    pub id: usize,
}

impl fmt::Display for BakedMutableId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.id, self.crate_name)
    }
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
impl BakedMutableId {
    pub fn new_isolated(id: usize) -> Self {
        Self::new(id, "")
    }
    pub fn new(id: usize, crate_name: &'static str) -> Self {
        Self { id, crate_name }
    }

    pub fn cloned(self) -> MutableId {
        MutableId {
            id: self.id,
            crate_name: self.crate_name.to_owned(),
        }
    }

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
        with_context!(self, get_mutation(self), Mutation::Unchanged)
    }
}

impl fmt::Display for MutableLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: only print known information
        write!(
            f,
            "{}:{} in {}",
            self.file,
            display_or_empty_if_none(&self.span),
            // TODO: loop instead
            self.path
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" => "), // TODO: use `intersperse` instead
        )
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

impl FromStr for Span {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let start;
        let mut end = None;
        match s.split_once('-') {
            Some((s, e)) => {
                start = parse_lc(s)?;
                end = Some(parse_lc(e)?);
            }
            None => {
                start = parse_lc(s)?;
            }
        }

        Ok(Self { start, end })
    }
}
fn parse_lc(s: &str) -> Result<LineColumn, Error> {
    let (l, c) = s
        .split_once(':')
        .ok_or_else(|| Error::LocationFormat(s.to_owned()))?;
    Ok(LineColumn {
        line: l.parse().map_err(|_| Error::LocationFormat(s.to_owned()))?,
        column: c.parse().map_err(|_| Error::LocationFormat(s.to_owned()))?,
    })
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

fn split_or_empty<T: FromIterator<String>>(s: &str, sep: &str) -> T {
    if s.is_empty() {
        std::iter::empty().collect()
    } else {
        s.split(sep).map(ToOwned::to_owned).collect()
    }
}
