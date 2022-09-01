//! Rust Mutation Testing core library.
//!
//! There are some internals here that are not meant for mutation testing users.

use std::{
    borrow::Cow,
    collections::{btree_map, BTreeMap, BTreeSet},
    env::VarError,
    fmt,
    fmt::Display,
    fs::File,
    io::{self, Read, Sink, Write},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    path::PathBuf,
    str::FromStr,
    sync::{Mutex, RwLock},
};

use lazy_static::lazy_static;
use serde::Deserialize;

pub mod mutable;
pub mod transformer;

#[cfg(test)]
mod tests;

/// a module for reexport from `muttest` crate
pub mod api {
    pub use crate::mutable;
    pub use crate::{phantom_for_type, MutableId, MutableLocation};

    pub use std::{marker::PhantomData, borrow::Cow, ops::ControlFlow, sync::RwLock, option::Option};
}

#[macro_export]
macro_rules! env_var_muttest_dir {
    () => {
        "MUTTEST_DIR"
    };
}

pub const ENV_VAR_MUTTEST_DIR: &'static str = env_var_muttest_dir!();
pub const ENV_VAR_MUTTEST_MUTATION: &'static str = "MUTTEST_MUTATION";

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
    #[error("failed to read csv file: {0}")]
    Csv(#[from] csv::Error),
    #[error("not a valid MutableId: '{0}'")]
    MutableIdFormat(String),
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
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let id = s
            .split_once(":")
            .ok_or_else(|| Error::MutableIdFormat(s.to_owned()))?;
        Ok(MutableId {
            id: id
                .0
                .parse::<usize>()
                .map_err(|_| Error::MutableIdFormat(s.to_owned()))?,
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
        self.get_collector().write_coverage(self, |_| None);

        ACTIVE_MUTATION
            .read()
            .expect("read-lock active mutations")
            .get(self)
            .cloned()
    }

    /// report b
    // TODO: name and behavior needs bikeshedding
    fn report_weak(&self, update: impl FnOnce(&str) -> Option<String>) {
        self.get_collector().write_coverage(self, update);
    }

    fn get_collector(&self) -> &'static MutableDataCollector {
        #[cfg(test)]
        if self.crate_name.is_empty() {
            return &tests::DATA_COLLECTOR;
        }
        &*DATA_COLLECTOR
    }
}

pub struct MutableDataCollector {
    locations: Mutex<BTreeSet<MutableId<'static>>>,
    possible_mutations: Mutex<BTreeSet<MutableId<'static>>>,
    coverage: Mutex<BTreeMap<MutableId<'static>, String>>,
    details_file: Mutex<CollectorFile>,
    coverage_file: Mutex<CollectorFile>,
}

impl MutableDataCollector {
    pub const ENV_VAR_DETAILS_FILE: &'static str = "MUTTEST_DETAILS_FILE";
    pub const ENV_VAR_COVERAGE_FILE: &'static str = "MUTTEST_COVERAGE_FILE";
    pub const DETAILS_FILE_HEADER: &'static str = "id,kind,data\n";
    pub const COVERAGE_FILE_HEADER: &'static str = "id,data\n";

    fn new_from_envvar_files() -> Result<Self, Error> {
        let details_file = CollectorFile::open_collector_file(Self::ENV_VAR_DETAILS_FILE)?;
        // TODO: fully read details the file (reading coverage does not make much sense)
        let coverage_file = CollectorFile::open_collector_file(Self::ENV_VAR_COVERAGE_FILE)?;
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
        let mut f = self.details_file.lock().unwrap();
        writeln!(f, "{m_id},{kind},{data}").expect("unable to write mutable detail");
        f.flush().expect("unable to flush mutable detail");
    }

    // TODO: document what update_data means
    fn write_coverage(
        &self,
        m_id: &MutableId<'static>,
        update_data: impl FnOnce(&str) -> Option<String>,
    ) {
        let mut coverage_map = self.coverage.lock().unwrap();
        let mut entry = coverage_map.entry(m_id.clone());

        // insert or update the coverage data
        let update = match &mut entry {
            btree_map::Entry::Occupied(e) => {
                let data = e.get_mut();
                match update_data(data) {
                    Some(d) => {
                        *data = d;
                        Some(&**data)
                    }
                    None => None,
                }
            }
            // create new entry
            _ => {
                let data = entry.or_default();
                if let Some(d) = update_data("") {
                    *data = d;
                }
                Some(&**data)
            }
        };

        if let Some(data) = update {
            let mut f = self.coverage_file.lock().unwrap();
            writeln!(f, "{m_id},{data}").expect("unable to write mutable detail");
            f.flush().expect("unable to flush mutable detail");
        }
    }
}
enum CollectorFile {
    None(Sink),
    File(File),
    #[cfg(test)]
    InMemory(Vec<u8>),
}
impl CollectorFile {
    fn open_collector_file(env_var_name: &'static str) -> Result<Self, Error> {
        match std::env::var(env_var_name) {
            Ok(file) => {
                let file = File::options()
                    .read(true)
                    .write(true)
                    .append(true)
                    .open(file)?;
                Ok(Self::File(file))
            }
            Err(VarError::NotPresent) => Ok(Self::None(std::io::sink())),
            Err(VarError::NotUnicode(_)) => Err(Error::EnvVarUnicode(env_var_name)),
        }
    }
}
impl Deref for CollectorFile {
    type Target = dyn Write;

    fn deref(&self) -> &Self::Target {
        match self {
            CollectorFile::None(s) => s,
            CollectorFile::File(f) => f,
            #[cfg(test)]
            CollectorFile::InMemory(v) => v,
        }
    }
}

impl DerefMut for CollectorFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            CollectorFile::None(s) => s,
            CollectorFile::File(f) => f,
            #[cfg(test)]
            CollectorFile::InMemory(v) => v,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct MutableData {
    pub kind: String,
    pub code: String,
    // TODO: maybe have two loc fields (one from defs, one from details)
    pub location: String,
    pub possible_mutations: Option<Vec<String>>,
}

#[derive(Debug, Clone, Default)]
pub struct CollectedData {
    pub mutables: BTreeMap<MutableId<'static>, MutableData>,
    pub coverage: BTreeMap<MutableId<'static>, String>,
}

impl CollectedData {
    pub fn read_details_csv(&mut self, details: impl Read) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(details);
        for md in reader.deserialize::<MutableDetail>() {
            let md = md?;

            let id = md.id.parse::<MutableId>()?;
            match &*md.kind {
                "mutations" => {
                    self.mutables.entry(id).or_default().possible_mutations = Some(
                        md.data
                            .split(":")
                            .filter(|x| !x.is_empty())
                            .map(ToOwned::to_owned)
                            .collect(),
                    );
                }
                "loc" => {
                    self.mutables.entry(id).or_default().location = md.data;
                }
                k => debug_assert!(false, "unknown detail kind '{}'", k),
            }
        }
        Ok(())
    }
    pub fn read_coverage_csv(&mut self, coverage: impl Read) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(coverage);
        for md in reader.deserialize::<MutableCoverage>() {
            let md = md?;
            let id = md.id.parse::<MutableId>()?;
            self.coverage.insert(id, md.data);
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize)]
pub struct MutableDetail {
    pub id: String,
    pub kind: String,
    pub data: String,
}

#[derive(Debug, Deserialize)]
pub struct MutableCoverage {
    id: String,
    data: String,
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
