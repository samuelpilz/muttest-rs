#![cfg_attr(test, feature(const_btree_new))] // unstable features only required for unit tests
//! Rust Mutation Testing core library.
//!
//! There are some internals here that are not meant for mutation testing users.

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    env::VarError,
    fmt,
    fmt::Display,
    fs::File,
    io::{self, Write},
    marker::PhantomData,
    ops::DerefMut,
    path::PathBuf,
    str::FromStr,
    sync::{Mutex, RwLock},
};

use lazy_static::lazy_static;

pub mod mutable;
pub mod transformer;

#[cfg(test)]
mod tests;

/// a module for reexport from `muttest` crate
pub mod api {
    pub use crate::mutable;
    pub use crate::{phantom_for_type, MutableId, MutableLocation};
}

pub const ENV_VAR_MUTTEST_DIR: &str = "MUTTEST_DIR";
pub const ENV_VAR_DETAILS_FILE: &str = "MUTTEST_DETAILS_FILE";
pub const ENV_VAR_COVERAGE_FILE: &str = "MUTTEST_COVERAGE_FILE";
pub const ENV_VAR_MUTTEST_MUTATION: &str = "MUTTEST_MUTATION";

lazy_static! {
    pub static ref MUTTEST_DIR: Option<PathBuf> = {
        match std::env::var(ENV_VAR_MUTTEST_DIR) {
            Ok(d) => Some(PathBuf::from(d)),
            Err(VarError::NotPresent) => None,
            Err(e) => panic!("{}", e),
        }
    };
    static ref ACTIVE_MUTATION: RwLock<BTreeMap<MutableId<'static>, String>> = {
        RwLock::new(parse_mutations(
            &std::env::var(ENV_VAR_MUTTEST_MUTATION).unwrap_or_default(),
        ))
    };
    static ref DATA_COLLECTOR: MutableDataCollector = MutableDataCollector::new_from_envvar_files()
        .expect("unable to open mutable-data-collector files");
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("unicode error env var {0}")]
    EnvVarUnicode(&'static str),
}

fn parse_mutations(env: &str) -> BTreeMap<MutableId<'static>, String> {
    let mut mutations = BTreeMap::new();

    // TODO: report errors
    for m in env.split(";") {
        if m.is_empty() {
            continue;
        }
        let (id, m) = m.split_once("=").unwrap();
        let id = id.parse().unwrap();
        mutations.insert(id, m.to_owned());
    }

    mutations
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MutableId<'a> {
    pub id: usize,
    pub crate_name: Cow<'a, str>,
}
impl fmt::Display for MutableId<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.id, self.crate_name)
    }
}
impl FromStr for MutableId<'static> {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let id = s.split_once(":").ok_or("invalid id format")?;
        Ok(MutableId {
            id: id.0.parse::<usize>().map_err(|_| "invalid id format")?,
            crate_name: Cow::Owned(id.1.to_owned()),
        })
    }
}

impl MutableId<'static> {
    /// reports this mutable's location
    ///
    /// This function should be called as early as possible
    pub fn report_at(&self, loc: MutableLocation) {
        self.get_collector().write_location(self, loc)
    }

    /// reports possible mutations
    ///
    /// The argument is a list of pairs of possible mutations.
    /// The bool value in each item indicates whether the mutation is possible
    /// This design makes the code-generation easier
    pub fn report_possible_mutations(&self, mutations: &[(&str, bool)]) {
        self.get_collector()
            .write_possible_mutations(self, mutations)
    }

    /// get the active mutation for a mutable
    ///
    /// calling this function also triggers logging its coverage
    fn get_active_mutation(&self) -> Option<String> {
        self.get_collector().write_coverage(self);

        ACTIVE_MUTATION
            .read()
            .expect("read-lock active mutations")
            .get(self)
            .cloned()
    }

    fn get_collector(&self) -> &'static MutableDataCollector {
        #[cfg(test)]
        if self.crate_name.is_empty() {
            return &tests::DATA_COLLECTOR;
        }
        &*DATA_COLLECTOR
    }
}

struct MutableDataCollector {
    coverage: Mutex<BTreeSet<MutableId<'static>>>,
    locations: Mutex<BTreeSet<MutableId<'static>>>,
    possible_mutations: Mutex<BTreeSet<MutableId<'static>>>,
    details_file: Mutex<Option<File>>,
    coverage_file: Mutex<Option<File>>,
}

impl MutableDataCollector {
    fn new_from_envvar_files() -> Result<Self, Error> {
        let details_file = open_collector_file(ENV_VAR_DETAILS_FILE)?;
        let coverage_file = open_collector_file(ENV_VAR_COVERAGE_FILE)?;
        Ok(MutableDataCollector {
            coverage: Mutex::new(Default::default()),
            locations: Mutex::new(Default::default()),
            possible_mutations: Mutex::new(Default::default()),
            details_file: Mutex::new(details_file),
            coverage_file: Mutex::new(coverage_file),
        })
    }

    fn write_location(&self, m_id: &MutableId<'static>, loc: MutableLocation) {
        let is_new = self.locations.lock().unwrap().insert(m_id.clone());
        if !is_new {
            return;
        }
        self.write_detail(m_id, "loc", loc);
    }

    fn write_possible_mutations(&self, m_id: &MutableId<'static>, mutations: &[(&str, bool)]) {
        let is_new = self.possible_mutations.lock().unwrap().insert(m_id.clone());
        if !is_new {
            return;
        }

        self.write_detail(
            m_id,
            "mutations",
            mutations
                .iter()
                .filter(|(_, ok)| *ok)
                .map(|(m, _)| *m)
                .collect::<Vec<_>>()
                .join(":"),
        );
    }

    fn write_detail<T: Display>(&self, m_id: &MutableId<'static>, kind: &'static str, data: T) {
        // TODO: where to go to testlib?
        let mut f = self.details_file.lock().unwrap();
        if let Some(f) = f.deref_mut() {
            writeln!(f, "{m_id},{kind},{data}").expect("unable to write mutable detail");
            f.flush().expect("unable to flush mutable detail");
        }
    }

    fn write_coverage(&self, m_id: &MutableId<'static>) {
        let is_new = self.coverage.lock().unwrap().insert(m_id.clone());
        if !is_new {
            return;
        }

        let mut f = self.coverage_file.lock().unwrap();
        if let Some(f) = f.deref_mut() {
            writeln!(f, "{m_id}").expect("unable to write mutable detail");
            f.flush().expect("unable to flush mutable detail");
        }
    }
}

fn open_collector_file(env_var_name: &'static str) -> Result<Option<File>, Error> {
    match std::env::var(env_var_name) {
        Ok(file) => {
            let file = File::options()
                .read(true)
                .write(true)
                .append(true)
                .open(file)?;
            // TODO: fully read the file
            Ok(Some(file))
        }
        Err(VarError::NotPresent) => Ok(None),
        Err(VarError::NotUnicode(_)) => Err(Error::EnvVarUnicode(env_var_name)),
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(unused)]
pub struct MutableLocation {
    pub file: &'static str,
    pub line: u32,
    pub column: u32,
}
impl fmt::Display for MutableLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

pub fn phantom_for_type<T>(_: &T) -> PhantomData<T> {
    PhantomData
}
