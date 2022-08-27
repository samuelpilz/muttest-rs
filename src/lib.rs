//! Rust Mutation Testing core library.
//!
//! There are some internals here that are not meant for mutation testing users.

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    env::VarError,
    fmt,
    fmt::Display,
    fs,
    io::{self, Write},
    marker::PhantomData,
    ops::DerefMut,
    path::PathBuf,
    str::FromStr,
    sync::{Mutex, RwLock},
};

use lazy_static::lazy_static;

pub mod mutable;
#[cfg(test)]
mod tests;
pub mod transformer;

/// a module for reexport from `muttest` crate
pub mod api {
    pub use crate::mutable;
    pub use crate::{
        phantom_for_type, report_mutable_type, report_possible_mutations, MutableId,
        MutableLocation,
    };
}

lazy_static! {
    pub static ref MUTTEST_DIR: Option<PathBuf> = {
        match std::env::var("MUTTEST_DIR") {
            Ok(d) => Some(PathBuf::from(d)),
            Err(VarError::NotPresent) => None,
            Err(e) => panic!("{}", e),
        }
    };
    static ref COVERAGE_FILE: Mutex<Option<fs::File>> = Mutex::new(None);
    static ref MUTABLE_DETAILS_FILE: Mutex<Option<fs::File>> = Mutex::new(None);
    static ref MUTABLE_DETAILS: Mutex<BTreeSet<(MutableId<'static>, &'static str)>> =
        Default::default();
    static ref ACTIVE_MUTATION: RwLock<BTreeMap<MutableId<'static>, String>> = {
        RwLock::new(parse_active_mutations(
            &std::env::var("MUTTEST_MUTATION").unwrap_or_default(),
        ))
    };
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Io(#[from] io::Error),
}

fn parse_active_mutations(env: &str) -> BTreeMap<MutableId<'static>, String> {
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

pub fn report_coverage(m_id: &MutableId) {
    let mut f = COVERAGE_FILE.lock().unwrap();
    if let Some(f) = f.deref_mut() {
        writeln!(f, "{m_id}").expect("unable to write log");
        f.flush().expect("unable to flush coverage report");
    }
}

/// get the active mutation for a mutable
fn get_active_mutation_for_mutable(m_id: &MutableId) -> Option<String> {
    ACTIVE_MUTATION
        .read()
        .expect("read-lock active mutations")
        .get(m_id)
        .cloned()
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
    fn report_detail<T: Display>(&self, kind: &'static str, data: T) {
        let is_new = MUTABLE_DETAILS.lock().unwrap().insert((self.clone(), kind));
        if !is_new {
            return;
        }
        let mut f = MUTABLE_DETAILS_FILE.lock().unwrap();
        if let Some(f) = f.deref_mut() {
            writeln!(f, "{self},{kind},{data}").expect("unable to write mutable context");
            f.flush()
                .expect("unable to flush possible mutations report");
        }
    }

    pub fn report_at(&self, loc: MutableLocation) {
        self.report_detail("loc", loc)
    }
}
// TODO: `report_*` functions as methods of MutableId

pub fn report_possible_mutations(m_id: &MutableId<'static>, reports: &[(&str, bool)]) {
    let mutations = reports
        .iter()
        .filter(|(_, ok)| *ok)
        .map(|(m, _)| *m)
        .collect::<Vec<_>>();
    let mutations = mutations.join(":");
    m_id.report_detail("mutations", mutations);
}

pub fn report_mutable_type(m_id: &MutableId<'static>, ty: &str) {
    m_id.report_detail("type", ty);
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
