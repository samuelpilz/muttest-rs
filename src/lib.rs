//! Rust Mutation Testing core library.
//!
//! There are some internals here that are not meant for mutation testing users.

use std::{
    borrow::Cow,
    collections::{btree_map, BTreeMap, BTreeSet},
    env::VarError,
    fmt,
    fs::File,
    io::{self, Read, Sink, Write},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    path::PathBuf,
    str::FromStr,
    sync::{Arc, Mutex, RwLock},
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
    pub use crate::{
        id, mutation_string_from_bool_list, mutation_string_opt, phantom_for_type, MutableId,
        MutableLocation,
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
pub const ENV_VAR_MUTTEST_MUTATION: &str = "MUTTEST_MUTATION";

lazy_static! {
    pub static ref MUTTEST_DIR: Option<PathBuf> = {
        match std::env::var(ENV_VAR_MUTTEST_DIR) {
            Ok(d) => Some(PathBuf::from(d)),
            Err(VarError::NotPresent) => None,
            Err(e) => panic!("{}", e),
        }
    };
    static ref ACTIVE_MUTATION: RwLock<BTreeMap<MutableId<'static>, Arc<str>>> = {
        RwLock::new(parse_mutations(
            &std::env::var(ENV_VAR_MUTTEST_MUTATION).unwrap_or_default(),
        ))
    };
    static ref DATA_COLLECTOR: DataCollector = DataCollector::new_from_envvar_files()
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
    #[error("not a known mutable: '{0}'")]
    UnknownMutable(MutableId<'static>),
}

fn parse_mutations(env: &str) -> BTreeMap<MutableId<'static>, Arc<str>> {
    let mut mutations = BTreeMap::new();

    // TODO: report errors
    for m in env.split(';') {
        if m.is_empty() {
            continue;
        }
        let (id, m) = m.split_once('=').unwrap();
        let id = id.parse().unwrap();
        mutations.insert(id, Arc::from(m));
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
            .split_once(':')
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
    pub fn new_isolated(id: usize) -> Self {
        Self::new(id, "")
    }
    pub fn new(id: usize, crate_name: &'static str) -> Self {
        MutableId {
            id,
            crate_name: Cow::Borrowed(crate_name),
        }
    }

    /// reports details of mutables gathered by static analysis
    pub fn report_details(&self, loc: MutableLocation, ty: &str, mutations: &str) {
        self.get_collector().write_details(self, loc, ty, mutations)
    }

    /// get the active mutation for a mutable
    ///
    /// calling this function also triggers logging its coverage
    fn get_active_mutation(&self) -> Option<Arc<str>> {
        self.get_collector().write_coverage(self, None);

        ACTIVE_MUTATION
            .read()
            .expect("read-lock active mutations")
            .get(self)
            .cloned()
    }

    // TODO: this is only a first draft of behavior
    fn report_weak(&self, weak: &str) {
        self.get_collector().write_coverage(self, Some(weak));
    }

    fn get_collector(&self) -> &'static DataCollector {
        #[cfg(test)]
        if self.crate_name.is_empty() {
            return &tests::DATA_COLLECTOR;
        }
        &*DATA_COLLECTOR
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

pub struct DataCollector {
    details: Mutex<BTreeSet<MutableId<'static>>>,
    coverage: Mutex<BTreeMap<MutableId<'static>, String>>,
    details_file: Mutex<CollectorFile>,
    coverage_file: Mutex<CollectorFile>,
}

impl DataCollector {
    pub const ENV_VAR_DETAILS_FILE: &'static str = "MUTTEST_DETAILS_FILE";
    pub const ENV_VAR_COVERAGE_FILE: &'static str = "MUTTEST_COVERAGE_FILE";
    pub const DETAILS_FILE_HEADER: &'static str = "id,loc,ty,mutations\n";
    pub const COVERAGE_FILE_HEADER: &'static str = "id,data\n";

    fn new_from_envvar_files() -> Result<Self, Error> {
        let details_file = CollectorFile::open_collector_file(Self::ENV_VAR_DETAILS_FILE)?;
        // TODO: fully read details the file (reading coverage does not make much sense)
        let coverage_file = CollectorFile::open_collector_file(Self::ENV_VAR_COVERAGE_FILE)?;
        Ok(DataCollector {
            details: Mutex::new(Default::default()),
            coverage: Mutex::new(Default::default()),
            details_file: Mutex::new(details_file),
            coverage_file: Mutex::new(coverage_file),
        })
    }

    fn write_details(
        &self,
        m_id: &MutableId<'static>,
        loc: MutableLocation,
        ty: &str,
        mutations: &str,
    ) {
        let is_new = self.details.lock().unwrap().insert(m_id.clone());
        if !is_new {
            return;
        }

        let mut f = self.details_file.lock().unwrap();
        writeln!(f, "{m_id},{loc},{ty},{mutations}").expect("unable to write mutable detail");
        f.flush().expect("unable to flush mutable detail");
    }

    fn write_coverage(&self, m_id: &MutableId<'static>, weak: Option<&str>) {
        let mut coverage_map = self.coverage.lock().unwrap();
        let mut update = false;

        let data = match coverage_map.entry(m_id.clone()) {
            btree_map::Entry::Occupied(e) => e.into_mut(),
            btree_map::Entry::Vacant(e) => {
                update = true;
                e.insert(String::new())
            }
        };

        if let Some(weak) = weak {
            debug_assert!(!weak.contains(':'));
            if data.is_empty() {
                *data = weak.to_owned();
                update = true;
            } else if data.split(':').all(|x| x != weak) {
                data.push(':');
                data.push_str(weak);
                update = true;
            }
        }

        if update {
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

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct MutableData {
    pub kind: String,
    pub code: String,
    pub details: Option<MutableDetails>,
    // TODO: parsed location
    pub loc: String,
}
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct MutableDetails {
    // TODO: merge location with other
    pub loc: String,
    pub mutable_type: String,
    pub possible_mutations: Vec<String>,
}

#[derive(Debug, Clone, Default)]
pub struct CollectedData {
    pub mutables: BTreeMap<MutableId<'static>, MutableData>,
    pub coverage: BTreeMap<MutableId<'static>, BTreeSet<String>>,
}
// TODO: tests

impl CollectedData {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn read_definition_csv(
        &mut self,
        mutated_package: &str,
        definitions: impl Read,
    ) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(definitions);
        for md in reader.deserialize::<MutableDefinition>() {
            let md = md?;
            let id = MutableId {
                id: md.id,
                crate_name: Cow::Owned(mutated_package.to_owned()),
            };
            self.mutables.insert(
                id,
                MutableData {
                    kind: md.kind,
                    code: md.code,
                    loc: md.loc,
                    details: None,
                },
            );
        }
        Ok(())
    }
    // TODO: report errors more gracefully
    pub fn read_details_csv(&mut self, details: impl Read) -> Result<(), Error> {
        #[derive(Debug, Deserialize)]
        pub struct MutableDetailsCsvLine {
            pub id: String,
            pub loc: String,
            pub ty: String,
            pub mutations: String,
        }

        let mut reader = csv::ReaderBuilder::new().from_reader(details);
        for md in reader.deserialize::<MutableDetailsCsvLine>() {
            let md = md?;

            let id = md.id.parse::<MutableId>()?;
            let details = MutableDetails {
                loc: md.loc,
                mutable_type: md.ty,
                possible_mutations: split_or_empty(&md.mutations, ":"),
            };
            self.mutables
                .get_mut(&id)
                .ok_or(Error::UnknownMutable(id))?
                .details = Some(details);
        }
        Ok(())
    }
    pub fn read_coverage_csv(&mut self, coverage: impl Read) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(coverage);
        for md in reader.deserialize::<MutableCoverage>() {
            let md = md?;
            self.coverage
                .insert(md.id.parse()?, split_or_empty(&md.data, ":"));
        }
        Ok(())
    }
}

fn split_or_empty<T: FromIterator<String>>(s: &str, sep: &str) -> T {
    if s.is_empty() {
        std::iter::empty().collect()
    } else {
        s.split(sep).map(ToOwned::to_owned).collect()
    }
}

#[derive(Debug, Deserialize)]
pub struct MutableCoverage {
    id: String,
    data: String,
}

#[derive(Debug, Deserialize)]
struct MutableDefinition {
    id: usize,
    kind: String,
    code: String,
    loc: String,
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

pub fn id<T>(t: T) -> T {
    t
}
