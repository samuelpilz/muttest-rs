use core::fmt;
use std::{
    collections::{btree_map, BTreeMap, BTreeSet},
    env::VarError,
    fmt::Display,
    fs::File,
    ops::ControlFlow,
    path::{Path, PathBuf},
    str::FromStr,
    sync::{Mutex, MutexGuard, PoisonError},
    time::{Duration, Instant},
};

use serde::{Deserialize, Serialize};
use strum::{EnumString, IntoStaticStr};

use crate::{
    abort_internal_error,
    report::{CrateId, MutableId, Span, TestId},
    CrateLocalMutableId, Error,
};

pub const MUTABLE_DEFINITIONS_CSV_HEAD: &str = "mut_id,kind,code,type_info\n";
pub const TYPES_FILE_CSV_HEAD: &str = "mut_id,ty,mutations\n";
pub const COVERAGE_FILE_CSV_HEAD: &str = "mut_id,test_id,behavior\n";
pub const ATTR_LOCATION_FILE_CSV_HEAD: &str = "crate_id,id,file,span\n";
pub const LOCATION_FILE_CSV_HEAD: &str = "mut_id,span\n";
pub const TEST_RESULT_FILE_CSV_HEAD: &str = "mutation,test_id,result\n";

pub const ENV_VAR_MUTTEST_MODE: &str = "MUTTEST_MODE";
pub const ENV_VAR_MUTTEST_DIR: &str = "MUTTEST_DIR";
pub const ENV_VAR_MUTTEST_MUTATION: &str = "MUTTEST_MUTATION";
pub const ENV_VAR_MUTTEST_TEST_TIMEOUT: &str = "MUTTEST_TEST_TIMEOUT";
pub const ENV_VAR_MUTTEST_TESTS: &str = "MUTTEST_TESTS";

pub const DEFAULT_TEST_TIMEOUT: Duration = Duration::from_secs(2);

pub fn types_path<P: AsRef<Path>>(muttest_dir: P) -> PathBuf {
    muttest_dir.as_ref().join("types.csv")
}
pub fn coverage_path<P: AsRef<Path>>(muttest_dir: P) -> PathBuf {
    muttest_dir.as_ref().join("coverage.csv")
}
pub fn attr_location_path<P: AsRef<Path>>(muttest_dir: P) -> PathBuf {
    muttest_dir.as_ref().join("attr-locations.csv")
}
pub fn location_path<P: AsRef<Path>>(muttest_dir: P) -> PathBuf {
    muttest_dir.as_ref().join("locations.csv")
}
pub fn test_results_path<P: AsRef<Path>>(muttest_dir: P) -> PathBuf {
    muttest_dir.as_ref().join("test-results.csv")
}

/// Modes in which `muttest`-enabled code can run in
#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoStaticStr, EnumString, strum::Display)]
pub enum MuttestMode {
    /// Build the test suite
    Build,
    /// Run test suite
    Test,
    /// activate a mutation
    Mutate,
    /// Activate a mutation, compute kill matrix
    MutateKillMatrix,
}

/// Context for executing mutations.
///
/// Contains information about which mutations are active and how to static and dynamic analysis data.
#[derive(Debug)]
pub(crate) struct MuttestContext {
    pub(crate) mutation: Option<(MutableId, &'static str)>,
    pub(crate) relevant_tests: Option<BTreeSet<TestId>>,

    pub(crate) test_deadline: Option<Mutex<Instant>>,
    pub(crate) test_default_timeout: Duration,

    pub(crate) test_mutex: Option<Mutex<()>>,

    pub(crate) analysis: Option<AnalysisContext>,
    pub(crate) test_results_file: Option<Writer>,
    // TODO: add reporter for errors
}

#[derive(Debug)]
pub(crate) struct AnalysisContext {
    pub(crate) types_map: Mutex<BTreeSet<MutableId>>,
    pub(crate) coverage_map: Mutex<BTreeMap<MutableId, String>>,
    pub(crate) types_file: Writer,
    pub(crate) coverage_file: Writer,
    pub(crate) location_file: Writer,
    pub(crate) attr_location_file: Writer,
}
// TODO: own struct for analysis-stuff

#[cfg_attr(feature = "selftest", muttest::mutate)]
impl MuttestContext {
    /// crate a mutation context from environment variables
    /// reads the environment variables `MUTTEST_TARGET` and `MUTTEST_MUTATION`
    ///
    /// returns `Ok(None)` if the environment variable `MUTTEST_MODE` is not set or is in mode `Build`
    /// * MUTTEST_DIR is not set
    /// * the current process is run during build, detected by the environment variable `MUTTEST_MODE`
    pub(crate) fn new_from_env() -> Result<Option<Self>, Error> {
        let Some(mode) = get_env_var_option(ENV_VAR_MUTTEST_MODE)? else {
            return Ok(None);
        };
        let mode =
            MuttestMode::from_str(&mode).map_err(|_| Error::InvalidMode(mode.to_string()))?;
        if mode == MuttestMode::Build {
            return Ok(None);
        }

        let muttest_dir = get_env_var(ENV_VAR_MUTTEST_DIR)?;

        let mutation;
        let relevant_tests;
        let test_mutex;
        let analysis;
        let mut test_results_file = None;

        match get_env_var_option(ENV_VAR_MUTTEST_MUTATION)? {
            None => {
                if mode != MuttestMode::Test {
                    return Err(Error::InvalidMode(mode.to_string()));
                }
                mutation = None;
                relevant_tests = None; // `None` means that all tests are executed

                test_mutex = Some(Default::default());
                analysis = Some(AnalysisContext {
                    types_map: Default::default(),
                    coverage_map: Default::default(),
                    types_file: open_collector_file(&types_path(&muttest_dir))?,
                    coverage_file: open_collector_file(&coverage_path(&muttest_dir))?,
                    location_file: open_collector_file(&location_path(&muttest_dir))?,
                    attr_location_file: open_collector_file(&attr_location_path(&muttest_dir))?,
                });
            }
            Some(m) => {
                if mode != MuttestMode::Mutate && mode != MuttestMode::MutateKillMatrix {
                    return Err(Error::InvalidMode(mode.to_string()));
                }

                let (id, m) = m
                    .split_once('=')
                    .ok_or(Error::MutationFormat(m.to_owned()))?;
                let id = id.parse()?;

                // the strings are leaked, because they are never deallocated anyway
                mutation = Some((id, &*String::leak(m.to_owned())));

                relevant_tests = get_env_var_option(ENV_VAR_MUTTEST_TESTS)?
                    .map(|t| t.split('\x1f').map(|x| x.parse()).collect())
                    .transpose()?;

                test_mutex = None;
                analysis = None;

                if mode == MuttestMode::MutateKillMatrix {
                    test_results_file = Some(open_collector_file(&test_results_path(&muttest_dir))?)
                }
            }
        };

        let default_test_timeout = match get_env_var_option(ENV_VAR_MUTTEST_TEST_TIMEOUT)? {
            Some(t) => Duration::from_secs(
                t.parse()
                    .map_err(|_| Error::EnvVarInvalid(ENV_VAR_MUTTEST_TEST_TIMEOUT))?,
            ),
            None => DEFAULT_TEST_TIMEOUT,
        };

        Ok(Some(MuttestContext {
            mutation,
            relevant_tests,
            test_deadline: Some(Mutex::new(Instant::now() + default_test_timeout)),
            test_default_timeout: default_test_timeout,
            test_mutex,
            analysis,
            test_results_file,
        }))
    }

    pub(crate) fn start_test(&self, test_id: TestId) -> ControlFlow<(), Option<MutexGuard<()>>> {
        if matches!(&self.relevant_tests, Some(tests) if !tests.contains(&test_id)) {
            return ControlFlow::Break(());
        }
        let lock = self
            .test_mutex
            .as_ref()
            .map(|m| m.lock().unwrap_or_else(PoisonError::into_inner));
        if let Some(a) = &self.analysis {
            a.coverage_map.lock().unwrap().clear();
        }
        if let Some(deadline) = &self.test_deadline {
            *deadline.lock().unwrap() = Instant::now() + self.test_default_timeout;
        }
        ControlFlow::Continue(lock)
    }

    pub(crate) fn end_test(&self, test_id: &TestId, success: bool) -> ControlFlow<()> {
        if let Some(deadline) = &self.test_deadline {
            *deadline.lock().unwrap() = Instant::now() + self.test_default_timeout;
        }

        if let Some(m) = &self.test_mutex {
            if !matches!(m.try_lock(), Err(std::sync::TryLockError::WouldBlock)) {
                eprintln!("test lock not locked at test end");
                abort_internal_error();
            }
        }

        // commit coverage
        if let Some(a) = &self.analysis {
            for (id, data) in &*a.coverage_map.lock().unwrap() {
                a.coverage_file
                    .write(format_args!("{id},{test_id},{data}\n",));
            }
        }

        if let (Some(f), Some((m_id, m))) = (&self.test_results_file, &self.mutation) {
            f.write(format_args!("{m_id}={m},{test_id},{success:?}"));
        } else if !success {
            return ControlFlow::Break(());
        }
        ControlFlow::Continue(())
    }

    pub fn get_action(&self, mut_id: &MutableId) -> Option<&'static str> {
        self.mutation
            .as_ref()
            .filter(|(id, _)| id == mut_id)
            .map(|(_, m)| *m)
    }

    pub(crate) fn write_types(
        &self,
        id: &MutableId,
        ty: &str,
        mutations: impl Display,
    ) -> Result<(), Error> {
        let Some(a) = &self.analysis else {
            return Ok(());
        };

        let is_new = a.types_map.lock().unwrap().insert(id.clone());
        if !is_new {
            return Ok(());
        }
        a.types_file.write(format_args!("{id},{ty},{mutations}\n",));
        Ok(())
    }

    pub(crate) fn write_coverage<C: Display>(
        &self,
        id: &MutableId,
        behavior: Option<C>,
    ) -> Result<(), Error> {
        let Some(a) = &self.analysis else {
            return Ok(());
        };
        let mut coverage_map = a.coverage_map.lock().unwrap();

        let data = match coverage_map.entry(id.clone()) {
            btree_map::Entry::Occupied(e) => e.into_mut(),
            btree_map::Entry::Vacant(e) => e.insert(String::new()),
        };

        if let Some(behavior) = behavior {
            let behavior = &behavior.to_string();
            debug_assert!(
                !behavior.is_empty() && !behavior.contains('\x1f') && !behavior.contains(',')
            );
            if data.is_empty() {
                *data = behavior.to_owned();
            } else if data.split('\x1f').all(|x| x != behavior) {
                data.push('\x1f');
                data.push_str(behavior);
            }
        }

        Ok(())
    }

    pub(crate) fn write_attr_location(
        &self,
        crate_id: CrateId,
        attr_id: u32,
        file: &str,
        span: Span,
    ) {
        // TODO: use csv write
        if let Some(a) = &self.analysis {
            a.attr_location_file
                .write(format_args!("{crate_id},{attr_id},{file},{span}\n"))
        }
    }

    pub(crate) fn write_mutable_location(&self, m_id: MutableId, span: Span) {
        if let Some(a) = &self.analysis {
            a.location_file.write(format_args!("{m_id},{span}\n"))
        }
    }
}

#[derive(Debug)]
pub(crate) enum Writer {
    #[cfg(test)]
    InMemory(Mutex<String>),
    File(Mutex<File>),
}
#[cfg_attr(feature = "selftest", muttest::mutate)]
impl Writer {
    fn write(&self, fmt: fmt::Arguments) {
        match self {
            #[cfg(test)]
            Writer::InMemory(x) => {
                use std::fmt::Write;
                x.lock().unwrap().write_fmt(fmt).unwrap()
            }
            Writer::File(x) => {
                use std::io::Write;
                let mut f = x.lock().unwrap();
                f.write_fmt(fmt).unwrap();
                f.flush().unwrap();
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MutableDefinitionCsvLine {
    pub mut_id: CrateLocalMutableId,
    pub kind: String,
    pub code: String,
    pub type_info: String,
}

#[derive(Debug, Deserialize)]
pub struct MutableTypesCsvLine {
    pub mut_id: MutableId,
    pub ty: String,
    pub mutations: String,
}
#[derive(Debug, Deserialize)]
pub struct AttrLocationCsvLine {
    pub crate_id: CrateId,
    pub id: u32,
    pub file: String,
    pub span: Span,
}
#[derive(Debug, Deserialize)]
pub struct LocationCsvLine {
    pub mut_id: MutableId,
    pub span: Span,
}
#[derive(Debug, Deserialize)]
pub struct MutableCoverageCsvLine {
    pub mut_id: MutableId,
    pub test_id: Option<TestId>,
    pub behavior: String,
}

fn open_collector_file(path: &Path) -> Result<Writer, Error> {
    let file = File::options()
        .read(true)
        .write(true)
        .append(true)
        .open(path)
        .map_err(|e| Error::CsvFile(path.to_path_buf(), Box::new(Error::from(e))))?;

    Ok(Writer::File(Mutex::new(file)))
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
