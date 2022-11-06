use std::{
    collections::{btree_map, BTreeMap, BTreeSet},
    env::VarError,
    fs::File,
    io::Write,
    path::Path,
    sync::{Arc, Mutex},
};

use crate::{
    env_var_muttest_dir, BakedLocation, BakedMutableId, CrateLocalMutableId, Error, Mutation,
};

pub const DETAILS_FILE_CSV_HEAD: &str = "attr_id,id,ty,mutations,file,module,attr_span,span\n";
pub const COVERAGE_FILE_CSV_HEAD: &str = "attr_id,id,behavior\n";

pub const ENV_VAR_MUTTEST_DIR: &str = env_var_muttest_dir!();
pub const ENV_VAR_MUTTEST_TARGET: &str = "MUTTEST_TARGET";
pub const ENV_VAR_MUTTEST_MUTATION: &str = "MUTTEST_MUTATION";
pub const ENV_VAR_DETAILS_FILE: &str = "MUTTEST_DETAILS_FILE";
pub const ENV_VAR_COVERAGE_FILE: &str = "MUTTEST_COVERAGE_FILE";

#[derive(Debug)]
pub(crate) struct MuttestContext<F> {
    pub(crate) pkg_name: String,
    pub(crate) crate_name: String,
    pub(crate) mutations: BTreeMap<CrateLocalMutableId, Arc<str>>,

    pub(crate) details: Mutex<BTreeSet<CrateLocalMutableId>>,
    pub(crate) coverage: Mutex<BTreeMap<CrateLocalMutableId, String>>,
    pub(crate) details_file: Option<Mutex<F>>,
    pub(crate) coverage_file: Option<Mutex<F>>,
}

impl MuttestContext<File> {
    pub(crate) fn new_from_env() -> Result<Option<Self>, Error> {
        let Some(target) = get_env_var_option(ENV_VAR_MUTTEST_TARGET)? else { return Ok(None) };
        let Some((pkg_name, crate_name)) = target.split_once(':') else {return Err(Error::TargetFormat(target.to_owned())) };

        let mutations = get_env_var_option(ENV_VAR_MUTTEST_MUTATION)?
            .map(|mutation| {
                mutation
                    .split(';')
                    .map(|m| {
                        let (id, m) = m.split_once('=').expect("invalid mutation format");
                        let id = id.parse().expect("invalid mutation format");
                        (id, Arc::from(m))
                    })
                    .collect()
            })
            .unwrap_or_default();

        // TODO: fully read details the file
        let details_file = open_collector_file(ENV_VAR_DETAILS_FILE)?;
        let coverage_file = open_collector_file(ENV_VAR_COVERAGE_FILE)?;

        Ok(Some(MuttestContext {
            pkg_name: pkg_name.to_owned(),
            crate_name: crate_name.to_owned(),
            mutations,
            details: Mutex::new(Default::default()),
            coverage: Mutex::new(Default::default()),
            details_file,
            coverage_file,
        }))
    }
}

impl<F> MuttestContext<F> {
    pub(crate) fn tracks_mutable(&self, m_id: BakedMutableId) -> bool {
        self.pkg_name == m_id.pkg_name && self.crate_name == m_id.crate_name
    }
    pub(crate) fn get_mutation(&self, m_id: CrateLocalMutableId) -> Mutation {
        match self.mutations.get(&m_id).cloned() {
            Some(m) => Mutation::Mutate(m),
            None => Mutation::Unchanged,
        }
    }
}

impl<F: Write> MuttestContext<F> {
    pub(crate) fn write_details(
        &self,
        id: CrateLocalMutableId,
        loc: BakedLocation,
        ty: &str,
        mutations: &str,
    ) {
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
                "{},{},{ty},{mutations},{file},{module},{attr_span},{span}",
                id.attr_id, id.id,
            )
            .expect("unable to write mutable detail");
            f.flush().expect("unable to flush mutable detail");
        }
    }

    pub(crate) fn write_coverage(&self, id: CrateLocalMutableId, behavior: Option<&str>) {
        let mut coverage_map = self.coverage.lock().unwrap();
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
            if let Some(f) = &self.coverage_file {
                let mut f = f.lock().unwrap();
                writeln!(f, "{},{},{data}", id.attr_id, id.id)
                    .expect("unable to write mutable detail");
                f.flush().expect("unable to flush mutable detail");
            }
        }
    }
}

fn open_collector_file(env_var: &str) -> Result<Option<Mutex<File>>, Error> {
    Ok(std::env::var_os(env_var)
        .map(|p| {
            File::options()
                .read(true)
                .write(true)
                .append(true)
                .open(Path::new(&p))
        })
        .transpose()?
        .map(Mutex::new))
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
