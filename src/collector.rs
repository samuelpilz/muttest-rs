use std::{
    collections::{btree_map, BTreeMap, BTreeSet},
    fs::File,
    io::{Read, Write},
    path::Path,
    sync::Mutex,
};

use serde::{Deserialize, Serialize};

use crate::{
    parse_or_none_if_empty, split_or_empty, BakedLocation, Error, MutableCoverage, MutableData,
    MutableDetails, MutableLocation, MuttestConf,
};

pub const DETAILS_FILE_HEADER: &str = "id,ty,mutations,file,module,attr_span,span";
pub const COVERAGE_FILE_HEADER: &str = "id,data";

// TODO: make collector only for single crate_name
pub struct DataCollector<F: Write> {
    pub(crate) details: Mutex<BTreeSet<usize>>,
    pub(crate) coverage: Mutex<BTreeMap<usize, String>>,
    pub(crate) details_file: Option<Mutex<F>>,
    pub(crate) coverage_file: Option<Mutex<F>>,
}

impl DataCollector<File> {
    pub(crate) fn new_from_conf(conf: &MuttestConf) -> Result<Self, Error> {
        let details_file = conf
            .details_file
            .as_deref()
            .map(open_collector_file)
            .transpose()?
            .map(Mutex::new);
        // TODO: fully read details the file (reading coverage does not make much sense)
        let coverage_file = conf
            .coverage_file
            .as_deref()
            .map(open_collector_file)
            .transpose()?
            .map(Mutex::new);
        Ok(DataCollector {
            details: Mutex::new(Default::default()),
            coverage: Mutex::new(Default::default()),
            details_file,
            coverage_file,
        })
    }
}

impl<F: Write> DataCollector<F> {
    pub(crate) fn write_details(&self, id: usize, loc: BakedLocation, ty: &str, mutations: &str) {
        // TODO: avoid unnecessary cloning
        let is_new = self.details.lock().unwrap().insert(id);
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
                "{id},{ty},{mutations},{file},{module},{attr_span},{span}"
            )
            .expect("unable to write mutable detail");
            f.flush().expect("unable to flush mutable detail");
        }
    }

    pub(crate) fn write_coverage(&self, id: usize, weak: Option<&str>) {
        let mut coverage_map = self.coverage.lock().unwrap();
        let mut update = false;

        let data = match coverage_map.entry(id) {
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
