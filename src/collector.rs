use std::{
    borrow::Cow,
    collections::{btree_map, BTreeMap, BTreeSet},
    env::VarError,
    fs::File,
    io::{Read, Sink, Write},
    ops::{Deref, DerefMut},
    sync::Mutex,
};

use serde::Deserialize;

use crate::{
    parse_or_none_if_empty, split_or_empty, BakedLocation, Error, MutableCoverage, MutableData,
    MutableDetails, MutableId, MutableLocation,
};

// TODO: generics for files
pub struct DataCollector {
    pub(crate) details: Mutex<BTreeSet<MutableId<'static>>>,
    pub(crate) coverage: Mutex<BTreeMap<MutableId<'static>, String>>,
    pub(crate) details_file: Mutex<CollectorFile>,
    pub(crate) coverage_file: Mutex<CollectorFile>,
}

impl DataCollector {
    pub const ENV_VAR_DETAILS_FILE: &'static str = "MUTTEST_DETAILS_FILE";
    pub const ENV_VAR_COVERAGE_FILE: &'static str = "MUTTEST_COVERAGE_FILE";
    pub const DETAILS_FILE_HEADER: &'static str = "id,ty,mutations,file,module,attr_span,span\n";
    pub const COVERAGE_FILE_HEADER: &'static str = "id,data\n";

    pub fn new_from_envvar_files() -> Result<Self, Error> {
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

    pub(crate) fn write_details(
        &self,
        m_id: &MutableId<'static>,
        loc: BakedLocation,
        ty: &str,
        mutations: &str,
    ) {
        let is_new = self.details.lock().unwrap().insert(m_id.clone());
        if !is_new {
            return;
        }

        let mut f = self.details_file.lock().unwrap();
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

    pub(crate) fn write_coverage(&self, m_id: &MutableId<'static>, weak: Option<&str>) {
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
pub enum CollectorFile {
    None(Sink),
    File(File),
    InMemory(Vec<u8>),
}
impl CollectorFile {
    // TODO: rename
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
    // TODO: fn InMemory with_csv_header
}
impl Deref for CollectorFile {
    type Target = dyn Write;

    fn deref(&self) -> &Self::Target {
        match self {
            CollectorFile::None(s) => s,
            CollectorFile::File(f) => f,
            CollectorFile::InMemory(v) => v,
        }
    }
}

impl DerefMut for CollectorFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            CollectorFile::None(s) => s,
            CollectorFile::File(f) => f,
            CollectorFile::InMemory(v) => v,
        }
    }
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
                crate_name: Cow::Owned(mutated_package.to_owned()),
            };
            self.mutables.insert(
                id,
                MutableData {
                    kind: md.kind,
                    code: md.code,
                    location: MutableLocation {
                        file: md.file.to_owned(),
                        module: String::new(),
                        path: md.path.split(':').map(ToOwned::to_owned).collect(),
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
