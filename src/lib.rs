//! Rust Mutation Testing core library.
//!
//! There are some internals here that are not meant for mutation testing users.

use std::{
    collections::{BTreeMap, BTreeSet},
    env::VarError,
    fmt,
    fs::File,
    io,
    marker::PhantomData,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use lazy_static::lazy_static;
use serde::Deserialize;

use crate::collector::DataCollector;

pub mod collector;
pub mod mutable;

#[cfg(test)]
mod tests;

/// a module for reexport from `muttest` crate
pub mod api {
    pub use crate::mutable;
    pub use crate::{
        id, mutation_string_from_bool_list, mutation_string_opt, phantom_for_type, BakedLocation,
        BakedMutableId, LineColumn, Span,
    };

    pub use std::{
        borrow::Cow, marker::PhantomData, ops::ControlFlow, option::Option, sync::RwLock,
    };
}

#[macro_export]
macro_rules! env_var_muttest_dir {
    () => {
        "MUTTEST_DIR"
    };
}

pub const ENV_VAR_MUTTEST_DIR: &str = env_var_muttest_dir!();
pub const ENV_VAR_MUTTEST_CRATE: &str = "MUTTEST_CRATE";
pub const ENV_VAR_MUTTEST_MUTATION: &str = "MUTTEST_MUTATION";

lazy_static! {
    static ref ACTIVE_MUTATION: ActiveMutation =
        ActiveMutation::new_from_env().expect("unable to read active mutation");
    static ref DATA_COLLECTOR: DataCollector<File> = DataCollector::new_from_envvar_files()
        .expect("unable to open mutable-data-collector files");
}

#[derive(Debug)]
struct ActiveMutation {
    crate_name: Option<String>,
    mutations: BTreeMap<usize, Arc<str>>,
}

impl ActiveMutation {
    fn new_from_env() -> Result<Self, Error> {
        let crate_name = get_env_var_option(ENV_VAR_MUTTEST_CRATE)?;
        let mut mutations = BTreeMap::new();

        if crate_name.is_some() {
            let mutation = get_env_var(ENV_VAR_MUTTEST_MUTATION)?;
            for m in mutation.split(';') {
                // TODO: report errors
                let (id, m) = m.split_once('=').unwrap();
                let id: usize = id.parse().unwrap();
                mutations.insert(id, Arc::from(m));
            }
        }

        Ok(Self {
            crate_name,
            mutations,
        })
    }
    fn get(&self, m_id: BakedMutableId) -> Option<Arc<str>> {
        if self.crate_name.as_deref() != Some(m_id.crate_name) {
            return None;
        }
        self.mutations.get(&m_id.id).cloned()
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
    UnknownMutable(MutableId),
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

pub fn get_env_var_option(var_name: &'static str) -> Result<Option<String>, Error> {
    match std::env::var(var_name) {
        Ok(s) => Ok(Some(s)),
        Err(VarError::NotPresent) => Ok(None),
        Err(VarError::NotUnicode(_)) => Err(Error::EnvVarUnicode(var_name)),
    }
}
pub fn get_env_var(var_name: &'static str) -> Result<String, Error> {
    match std::env::var(var_name) {
        Ok(s) => Ok(s),
        Err(VarError::NotPresent) => Err(Error::EnvVarMissing(var_name)),
        Err(VarError::NotUnicode(_)) => Err(Error::EnvVarUnicode(var_name)),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MutableData {
    pub kind: String,
    pub code: String,
    pub location: MutableLocation,
    pub details: Option<MutableDetails>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MutableDetails {
    // TODO: merge location with other
    pub mutable_type: String,
    pub possible_mutations: BTreeSet<String>,
}

#[derive(Debug, Deserialize)]
pub struct MutableCoverage {
    id: String,
    data: String,
}

// TODO: name
pub struct BakedLocation {
    pub file: &'static str,
    pub module: &'static str,
    pub attr_span: Span,
    pub span: Span,
    // TODO: add more end location info for feature proc_macro_span / proc_macro_span_shrink
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(unused)]
pub struct MutableLocation {
    pub file: String,
    pub module: String,
    pub path: Vec<PathSegment>,
    pub attr_span: Option<Span>,
    pub span: Option<Span>,
    // TODO: display instance
}

// TODO: use proc-macro2 structs instead??
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: LineColumn,
    pub end: Option<LineColumn>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

// TODO: maybe borrow something?
// TODO: maybe have idents here?
#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub crate_name: &'static str,
    pub id: usize,
}

impl fmt::Display for BakedMutableId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.id, self.crate_name)
    }
}

// TODO: this can surely be done cleaner
macro_rules! with_collector {
    ($s:expr, $f:ident($($args:expr),*)) => {
        match () {
            #[cfg(test)]
            _ if $s.crate_name.is_empty() => tests::DATA_COLLECTOR.$f($($args,)*),
            _ => DATA_COLLECTOR.$f($($args,)*),
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
        with_collector!(self, write_details(self, loc, ty, mutations))
    }

    /// get the active mutation for a mutable
    ///
    /// calling this function also triggers logging its coverage
    fn get_active_mutation(self) -> Option<Arc<str>> {
        with_collector!(self, write_coverage(self, None));

        match () {
            #[cfg(test)]
            _ if self.crate_name.is_empty() => tests::ACTIVE_MUTATION.read().unwrap().get(self),
            _ => ACTIVE_MUTATION.get(self),
        }
    }

    // TODO: this is only a first draft of behavior
    fn report_weak(self, weak: &str) {
        with_collector!(self, write_coverage(self, Some(weak)));
    }
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

impl fmt::Display for MutableLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

pub fn phantom_for_type<T>(_: &T) -> PhantomData<T> {
    PhantomData
}

pub fn id<T>(t: T) -> T {
    t
}
