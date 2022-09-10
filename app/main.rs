use std::{
    fs::{self, File},
    io::{self, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::{Duration, Instant},
};

use cargo_metadata::camino::{Utf8Path, Utf8PathBuf};
use clap::Parser;
use muttest_core::{
    collector::{self, CollectedData},
    mutable::{
        self, binop_cmp::MutableBinopCmp, lit_int::MutableLitInt, lit_str::MutableLitStr, Mutable,
    },
    MutableData, ENV_VAR_MUTTEST_DIR,
};
use wait_timeout::ChildExt;

#[derive(Debug, Parser)]
struct Opt {
    #[clap(long)]
    workspace: bool,

    #[clap(long)]
    all_features: bool,
}
#[derive(Debug, Parser)]
enum OptCargoPlugin {
    #[clap(name = "muttest", version, author, dont_collapse_args_in_usage = true)]
    Muttest(Opt),
}
impl From<OptCargoPlugin> for Opt {
    fn from(OptCargoPlugin::Muttest(x): OptCargoPlugin) -> Self {
        x
    }
}

fn main() -> Result<(), Error> {
    let is_cargo_plugin = std::env::var_os("CARGO").is_some();
    let opt = if is_cargo_plugin {
        OptCargoPlugin::parse().into()
    } else {
        Opt::parse()
    };
    println!("{opt:?}");

    // TODO: pass features from opts
    // read cargo metadata
    let cargo_exe = std::env::var_os("CARGO");
    let cargo_exe = cargo_exe
        .as_ref()
        .map(Path::new)
        .unwrap_or_else(|| Path::new("cargo"));
    let cargo_metadata = cargo_metadata::MetadataCommand::new().exec()?;
    let muttest_dir = cargo_metadata.target_directory.join("muttest");
    fs::create_dir_all(&muttest_dir)?;

    // compile libs and test cases
    let mut compilation_result = compile(cargo_exe, &muttest_dir)?;

    println!("{compilation_result:?}");

    // read created mutables
    let mut data = read_mutable_defs(&muttest_dir)?;

    println!("mutable definitions {data:?}");

    let details_path = muttest_dir.join("mutable-details.csv");
    setup_csv_file(&details_path, collector::DETAILS_FILE_HEADER)?;
    let coverage_path = muttest_dir.join("coverage.csv");
    setup_csv_file(&coverage_path, collector::COVERAGE_FILE_HEADER)?;

    // run test suites without mutations for coverage
    for test_bin in &mut compilation_result.test_bins {
        println!("call {}", &test_bin.name);
        let start_time = Instant::now();
        let status = Command::new(&test_bin.path)
            .env(ENV_VAR_MUTTEST_DIR, &muttest_dir)
            .env(collector::ENV_VAR_DETAILS_FILE, &details_path)
            .env(collector::ENV_VAR_COVERAGE_FILE, &coverage_path)
            .stdout(Stdio::inherit())
            .spawn()?
            .wait()?;
        test_bin.exec_time = Some(start_time.elapsed());
        // TODO: better error report
        if !status.success() {
            panic!("test suite fails")
        }
    }

    data.read_details_csv(File::open(&details_path)?)
        .map_err(|e| e.in_csv_file(&details_path))?;
    data.read_coverage_csv(File::open(&coverage_path)?)
        .map_err(|e| e.in_csv_file(&coverage_path))?;

    let mut total_mutants = 0;
    let mut killed_mutants = 0;

    // evaluate mutations
    let m_ids = data.mutables.keys().cloned().collect::<Vec<_>>();
    let mut crate_name = None;
    for m_id in &m_ids {
        if crate_name == Some(&m_id.crate_name) {
            println!("crate {}", m_id.crate_name);
            crate_name = Some(&m_id.crate_name);
        }

        let mutable @ MutableData {
            code,
            kind,
            location,
            ..
        } = &data.mutables[m_id];
        let coverage = data.coverage.get(m_id);
        let mutations = mutations_for_mutable(mutable);
        let id = m_id.id;
        println!("{id}: {location} `{code}` -{kind}-> {mutations:?}; coverage: {coverage:?}");

        let mutations = match mutations {
            Some(m) => m,
            None => continue,
        };

        for m in mutations {
            total_mutants += 1;
            println!("  mutation {:5}", format!("{m:?}"));

            let coverage = match coverage {
                Some(c) => c,
                None => {
                    println!("    not covered",);
                    continue;
                }
            };

            // TODO: improve weak surviving
            if *kind == MutableBinopCmp::NAME
                && mutable::binop_cmp::identical_behavior(code, &m, coverage)
            {
                println!("    survived weak mutation testing");
                continue;
            }

            // run test suites without mutations for coverage
            for test_bin in &compilation_result.test_bins {
                println!("    call {}", &test_bin.name);
                let mut test = Command::new(&test_bin.path)
                    .env(
                        "MUTTEST_MUTATION",
                        format!("{}:{}={m}", m_id.id, m_id.crate_name),
                    )
                    // TODO: think about details in mutated runs
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .spawn()?;
                // TODO: make timeout parameters into config options
                let result = test.wait_timeout(
                    Duration::from_millis(500).max(3 * test_bin.exec_time.unwrap()),
                )?;
                let result = match result {
                    Some(r) => r,
                    None => {
                        test.kill()?;
                        test.wait()?
                    }
                };
                let result = if result.success() {
                    "survived"
                } else {
                    killed_mutants += 1;
                    "killed"
                };
                println!("      {}", result);
                // TODO: run tests in finer granularity
            }
        }
    }

    println!("{killed_mutants}/{total_mutants} mutants killed");

    Ok(())
}

#[derive(Debug, Default)]
struct CompilationResult {
    test_bins: Vec<TestBin>,
}

#[derive(Debug)]
struct TestBin {
    path: Utf8PathBuf,
    name: String,
    exec_time: Option<Duration>,
}
/// execute `cargo test --no-run --message-format=json` and collect output
fn compile(cargo_exe: &Path, muttest_dir: &Utf8Path) -> Result<CompilationResult, Error> {
    let mut result = CompilationResult::default();

    // TODO: pass features from opts
    let mut compile_out = Command::new(cargo_exe)
        .args(&["test", "--no-run", "--message-format=json"])
        .env("MUTTEST_DIR", muttest_dir)
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|_| Error::Cargo("test", None))?;
    let reader = std::io::BufReader::new(compile_out.stdout.take().unwrap());
    for msg in cargo_metadata::Message::parse_stream(reader) {
        let test_artifact = match msg? {
            cargo_metadata::Message::CompilerArtifact(a) if a.profile.test => a,
            cargo_metadata::Message::CompilerMessage(m) => {
                eprintln!("{}", m.message);
                continue;
            }
            _ => continue,
        };
        let test_exe = match test_artifact.executable.as_deref() {
            Some(e) => e,
            None => continue,
        };

        let test_exe = TestBin {
            path: test_exe.to_owned(),
            name: test_exe.file_name().expect("test exe file name").to_owned(),
            exec_time: None,
        };

        result.test_bins.push(test_exe);
    }

    let status = compile_out.wait().map_err(|_| Error::Cargo("test", None))?;
    if !status.success() {
        return Err(Error::Cargo("test", status.code()));
    }

    Ok(result)
}

fn read_mutable_defs(muttest_dir: &Utf8Path) -> Result<CollectedData, Error> {
    let mut data = CollectedData::default();
    for file in std::fs::read_dir(muttest_dir)? {
        let file = file?;
        let file_name = file.file_name().into_string().unwrap();
        let mutated_package = file_name
            .strip_prefix("mutable-definitions-")
            .and_then(|f| f.strip_suffix(".csv"));
        let mutated_package = match mutated_package {
            None => continue,
            Some(n) => n,
        };

        // TODO: try to delete outdated muttest files
        let file_path = muttest_dir.join(&file_name);
        data.read_definition_csv(mutated_package, File::open(file_path)?)?;
    }
    Ok(data)
}

fn setup_csv_file(path: &impl AsRef<Path>, head: &str) -> Result<(), CoreError> {
    let mut file = File::create(path)?;
    writeln!(&mut file, "{head}")?;
    file.flush()?;
    file.sync_all()?;
    Ok(())
}

pub fn mutations_for_mutable(mutable: &MutableData) -> Option<Vec<String>> {
    Some(match &*mutable.kind {
        MutableLitInt::NAME => {
            let i = mutable.code.parse::<u128>().expect("unable to parse int");
            let mut m = vec![];
            if i != 0 {
                m.push((i - 1).to_string());
            }
            m.push((i + 1).to_string());
            m
        }
        MutableLitStr::NAME => {
            if mutable.code.is_empty() {
                vec![]
            } else {
                vec![String::new()]
            }
        }
        // fallback to mutable's description of possible mutations
        _ => mutable
            .details
            .iter()
            .flat_map(|m| &m.possible_mutations)
            .filter(|&x| x != &mutable.code)
            .map(ToOwned::to_owned)
            .collect(),
    })
}

type CoreError = muttest_core::Error;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to run `cargo {0}`{}", display_exit_code_msg(.1))]
    Cargo(&'static str, Option<i32>),
    #[error("failed to read cargo metadata")]
    CargoMetadata(#[from] cargo_metadata::Error),
    #[error("{0}")]
    CoreError(#[from] CoreError),
    #[error("failed to read csv file {0}. {1}")]
    Csv(PathBuf, csv::Error),
}
impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::CoreError(e.into())
    }
}

fn display_exit_code_msg(e: &Option<i32>) -> String {
    match e {
        None => String::new(),
        Some(e) => format!(". Exited with code {e}"),
    }
}
