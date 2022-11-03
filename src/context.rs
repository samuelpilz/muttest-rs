use std::{
    collections::{btree_map, BTreeMap, BTreeSet},
    env::VarError,
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use serde::{Deserialize, Serialize};

use crate::{
    env_var_muttest_dir, parse_or_none_if_empty, split_or_empty, BakedLocation, BakedMutableId,
    Error, MutableCoverage, MutableData, MutableDetails, MutableLocation, Mutation,
};

pub const DETAILS_FILE_CSV_HEAD: &str = "id,ty,mutations,file,module,attr_span,span\n";
pub const COVERAGE_FILE_CSV_HEAD: &str = "id,data\n";

pub const ENV_VAR_MUTTEST_DIR: &str = env_var_muttest_dir!();
pub const ENV_VAR_MUTTEST_CRATE: &str = "MUTTEST_CRATE";
pub const ENV_VAR_MUTTEST_MUTATION: &str = "MUTTEST_MUTATION";
pub const ENV_VAR_DETAILS_FILE: &str = "MUTTEST_DETAILS_FILE";
pub const ENV_VAR_COVERAGE_FILE: &str = "MUTTEST_COVERAGE_FILE";

#[derive(Debug)]
pub(crate) struct MuttestContext<F> {
    pub(crate) target: String,
    pub(crate) mutations: MutationsMap,
    pub(crate) collector: DataCollector<F>,
}

pub type MutationsMap = BTreeMap<usize, Arc<str>>;

// TODO: inline into context
#[derive(Debug)]
pub struct DataCollector<F> {
    pub(crate) details: Mutex<BTreeSet<usize>>,
    pub(crate) coverage: Mutex<BTreeMap<usize, String>>,
    pub(crate) details_file: Option<Mutex<F>>,
    pub(crate) coverage_file: Option<Mutex<F>>,
}

impl MuttestContext<File> {
    pub(crate) fn new_from_env() -> Result<Option<Self>, Error> {
        let target = get_env_var_option(ENV_VAR_MUTTEST_CRATE)?;
        let target = match target {
            None => return Ok(None),
            Some(t) => t,
        };

        let mutations = get_env_var_option(ENV_VAR_MUTTEST_MUTATION)?
            .map(|mutation| {
                mutation
                    .split(';')
                    .map(|m| {
                        let (id, m) = m.split_once('=').expect("invalid mutation format");
                        let id: usize = id.parse().expect("invalid mutation format");
                        (id, Arc::from(m))
                    })
                    .collect()
            })
            .unwrap_or_default();

        let details_file = get_env_var_pathbuf_option(ENV_VAR_DETAILS_FILE)
            .as_deref()
            .map(open_collector_file)
            .transpose()?
            .map(Mutex::new);
        // TODO: fully read details the file
        let coverage_file = get_env_var_pathbuf_option(ENV_VAR_COVERAGE_FILE)
            .as_deref()
            .map(open_collector_file)
            .transpose()?
            .map(Mutex::new);

        Ok(Some(MuttestContext {
            target,
            mutations,
            collector: DataCollector {
                details: Mutex::new(Default::default()),
                coverage: Mutex::new(Default::default()),
                details_file,
                coverage_file,
            },
        }))
    }
}

impl<F> MuttestContext<F> {
    pub(crate) fn tracks_mutable(&self, m_id: BakedMutableId) -> bool {
        self.target == m_id.crate_name
    }
    pub(crate) fn get_mutation(&self, m_id: BakedMutableId) -> Mutation {
        debug_assert!(
            self.tracks_mutable(m_id),
            "context for {} called for {m_id}",
            self.target
        );

        match self.mutations.get(&m_id.id).cloned() {
            Some(m) => Mutation::Mutate(m),
            None => Mutation::Unchanged,
        }
    }
}

impl<F: Write> MuttestContext<F> {
    pub(crate) fn write_details(&self, id: usize, loc: BakedLocation, ty: &str, mutations: &str) {
        let is_new = self.collector.details.lock().unwrap().insert(id);
        if !is_new {
            return;
        }

        if let Some(f) = &self.collector.details_file {
            let mut f = f.lock().unwrap();
            let BakedLocation {
                file,
                module,
                attr_span,
                span,
            } = loc;
            writeln!(
                f,
                "{id},{ty},{mutations},{file},{module},{attr_span},{span}"
            )
            .expect("unable to write mutable detail");
            f.flush().expect("unable to flush mutable detail");
        }
    }

    pub(crate) fn write_coverage(&self, id: usize, behavior: Option<&str>) {
        let mut coverage_map = self.collector.coverage.lock().unwrap();
        let mut update = false;

        let data = match coverage_map.entry(id) {
            btree_map::Entry::Occupied(e) => e.into_mut(),
            btree_map::Entry::Vacant(e) => {
                update = true;
                e.insert(String::new())
            }
        };

        if let Some(behavior) = behavior {
            debug_assert!(!behavior.contains(':'));
            if data.is_empty() {
                *data = behavior.to_owned();
                update = true;
            } else if data.split(':').all(|x| x != behavior) {
                data.push(':');
                data.push_str(behavior);
                update = true;
            }
        }

        if update {
            if let Some(f) = &self.collector.coverage_file {
                let mut f = f.lock().unwrap();
                writeln!(f, "{id},{data}").expect("unable to write mutable detail");
                f.flush().expect("unable to flush mutable detail");
            }
        }
    }
}

fn open_collector_file(path: &Path) -> Result<File, Error> {
    Ok(File::options()
        .read(true)
        .write(true)
        .append(true)
        .open(path)?)
}

#[derive(Debug, Clone, Serialize)]
pub struct CollectedData {
    pub mutables: BTreeMap<usize, MutableData>,
    pub coverage: BTreeMap<usize, BTreeSet<String>>,
}
// TODO: tests

impl CollectedData {
    pub fn from_definition_csv(definitions: impl Read) -> Result<Self, Error> {
        #[derive(Debug, Deserialize)]
        struct MutableDefinitionCsvLine {
            id: usize,
            kind: String,
            code: String,
            file: String,
            path: String,
            attr_span: String,
            span: String,
        }

        let mut data = CollectedData {
            mutables: BTreeMap::default(),
            coverage: BTreeMap::default(),
        };

        let mut reader = csv::ReaderBuilder::new().from_reader(definitions);
        for md in reader.deserialize::<MutableDefinitionCsvLine>() {
            let md = md?;
            data.mutables.insert(
                md.id,
                MutableData {
                    kind: md.kind,
                    code: md.code,
                    location: MutableLocation {
                        file: md.file.to_owned(),
                        module: String::new(),
                        path: md
                            .path
                            .split(':')
                            .map(|s| s.parse())
                            .collect::<Result<_, _>>()?,
                        attr_span: parse_or_none_if_empty(&md.attr_span)?,
                        span: parse_or_none_if_empty(&md.span)?,
                    },
                    details: None,
                },
            );
        }
        Ok(data)
    }
    // TODO: report errors more gracefully
    pub fn read_details_csv(&mut self, details: impl Read) -> Result<(), Error> {
        #[derive(Debug, Deserialize)]
        struct MutableDetailsCsvLine {
            id: usize,
            ty: String,
            mutations: String,
            file: String,
            module: String,
            attr_span: String,
            span: String,
        }

        let mut reader = csv::ReaderBuilder::new().from_reader(details);
        for md in reader.deserialize::<MutableDetailsCsvLine>() {
            let md = md?;

            // TODO: enhance location info
            let details = MutableDetails {
                mutable_type: md.ty,
                possible_mutations: split_or_empty(&md.mutations, ":"),
            };
            // TODO: enhance mutable location
            let mutable = self
                .mutables
                .get_mut(&md.id)
                .ok_or(Error::UnknownMutable(md.id))?;

            if mutable.location.span.is_none() {
                mutable.location.span = parse_or_none_if_empty(&md.span)?;
            }
            if mutable.location.attr_span.is_none() {
                mutable.location.attr_span = parse_or_none_if_empty(&md.attr_span)?;
            }
            mutable.location.file = md.file;
            mutable.location.module = md.module;
            mutable.details = Some(details);
        }
        Ok(())
    }
    pub fn read_coverage_csv(&mut self, coverage: impl Read) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(coverage);
        for md in reader.deserialize::<MutableCoverage>() {
            let md = md?;
            self.coverage.insert(md.id, split_or_empty(&md.data, ":"));
        }
        Ok(())
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

pub fn get_env_var_pathbuf_option(var_name: &'static str) -> Option<PathBuf> {
    std::env::var_os(var_name).map(|e| PathBuf::from(e))
}
