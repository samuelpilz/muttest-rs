use std::{
    collections::{btree_map, BTreeMap, BTreeSet},
    env::VarError,
    fs::File,
    io::{Read, Write},
    sync::Mutex,
};

use serde::Deserialize;

use crate::{
    parse_or_none_if_empty, split_or_empty, BakedLocation, BakedMutableId, Error, MutableCoverage,
    MutableData, MutableDetails, MutableId, MutableLocation,
};

pub const ENV_VAR_DETAILS_FILE: &str = "MUTTEST_DETAILS_FILE";
pub const ENV_VAR_COVERAGE_FILE: &str = "MUTTEST_COVERAGE_FILE";
pub const DETAILS_FILE_HEADER: &str = "id,ty,mutations,file,module,attr_span,span";
pub const COVERAGE_FILE_HEADER: &str = "id,data";

pub struct DataCollector<F: Write> {
    pub(crate) details: Mutex<BTreeSet<MutableId>>,
    pub(crate) coverage: Mutex<BTreeMap<MutableId, String>>,
    pub(crate) details_file: Option<Mutex<F>>,
    pub(crate) coverage_file: Option<Mutex<F>>,
}

impl DataCollector<File> {
    pub fn new_from_envvar_files() -> Result<Self, Error> {
        let details_file = open_collector_file(ENV_VAR_DETAILS_FILE)?.map(Mutex::new);
        // TODO: fully read details the file (reading coverage does not make much sense)
        let coverage_file = open_collector_file(ENV_VAR_COVERAGE_FILE)?.map(Mutex::new);
        Ok(DataCollector {
            details: Mutex::new(Default::default()),
            coverage: Mutex::new(Default::default()),
            details_file,
            coverage_file,
        })
    }
}

impl<F: Write> DataCollector<F> {
    pub(crate) fn write_details(
        &self,
        m_id: BakedMutableId,
        loc: BakedLocation,
        ty: &str,
        mutations: &str,
    ) {
        // TODO: avoid unnecessary cloning
        let is_new = self.details.lock().unwrap().insert(m_id.cloned());
        if !is_new {
            return;
        }

        if let Some(f) = &self.details_file {
            let mut f = f.lock().unwrap();
            let BakedLocation {
                file,
                module,
                attr_span,
                span,
            } = loc;
            writeln!(
                f,
                "{m_id},{ty},{mutations},{file},{module},{attr_span},{span}"
            )
            .expect("unable to write mutable detail");
            f.flush().expect("unable to flush mutable detail");
        }
    }

    pub(crate) fn write_coverage(&self, m_id: BakedMutableId, weak: Option<&str>) {
        let mut coverage_map = self.coverage.lock().unwrap();
        let mut update = false;

        let data = match coverage_map.entry(m_id.cloned()) {
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
            if let Some(f) = &self.coverage_file {
                let mut f = f.lock().unwrap();
                writeln!(f, "{m_id},{data}").expect("unable to write mutable detail");
                f.flush().expect("unable to flush mutable detail");
            }
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
            Ok(Some(file))
        }
        Err(VarError::NotPresent) => Ok(None),
        Err(VarError::NotUnicode(_)) => Err(Error::EnvVarUnicode(env_var_name)),
    }
}

#[derive(Debug, Clone, Default)]
pub struct CollectedData {
    pub mutables: BTreeMap<MutableId, MutableData>,
    pub coverage: BTreeMap<MutableId, BTreeSet<String>>,
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

        let mut reader = csv::ReaderBuilder::new().from_reader(definitions);
        for md in reader.deserialize::<MutableDefinitionCsvLine>() {
            let md = md?;
            let id = MutableId {
                id: md.id,
                crate_name: mutated_package.to_owned(),
            };
            self.mutables.insert(
                id,
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
        Ok(())
    }
    // TODO: report errors more gracefully
    pub fn read_details_csv(&mut self, details: impl Read) -> Result<(), Error> {
        #[derive(Debug, Deserialize)]
        struct MutableDetailsCsvLine {
            id: String,
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

            let id = md.id.parse::<MutableId>()?;
            // TODO: enhance location info
            let details = MutableDetails {
                mutable_type: md.ty,
                possible_mutations: split_or_empty(&md.mutations, ":"),
            };
            // TODO: enhance mutable location
            let mutable = self
                .mutables
                .get_mut(&id)
                .ok_or(Error::UnknownMutable(id))?;

            if mutable.location.span.is_none() {
                mutable.location.span = parse_or_none_if_empty(&md.span)?;
            }
            if mutable.location.attr_span.is_none() {
                mutable.location.span = parse_or_none_if_empty(&md.attr_span)?;
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
            self.coverage
                .insert(md.id.parse()?, split_or_empty(&md.data, ":"));
        }
        Ok(())
    }
}
