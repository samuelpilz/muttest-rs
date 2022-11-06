use std::{
    fs::{self, File},
    io::{self, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::{Duration, Instant},
};

use cargo_metadata::camino::Utf8Path;
use clap::Parser;
use muttest_core::{
    context,
    mutable::{self, binop_cmp::MutableBinopCmp, mutations_for_mutable, Mutable},
    report::{MutableAnalysis, MuttestReport, MuttestReportForCrate, TestBin},
};
use wait_timeout::ChildExt;

#[derive(Debug, Parser)]
struct Opt {
    #[clap(long)]
    workspace: bool,

    #[clap(long)]
    all_features: bool,

    #[clap(short, long)]
    package: Option<String>,
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

// TODO: enable logging for cargo-calls
fn main() -> Result<(), Error> {
    let is_cargo_plugin = std::env::var_os("CARGO").is_some();
    let opt = if is_cargo_plugin {
        OptCargoPlugin::parse().into()
    } else {
        Opt::parse()
    };
    println!("{opt:?}");

    // read cargo metadata
    let cargo_exe = std::env::var_os("CARGO");
    let cargo_exe = cargo_exe
        .as_ref()
        .map(Path::new)
        .unwrap_or_else(|| Path::new("cargo"));
    // TODO: pass opts to metadata command
    let cargo_metadata = cargo_metadata::MetadataCommand::new().exec()?;
    let muttest_dir = cargo_metadata.target_directory.join("muttest");
    fs::create_dir_all(&muttest_dir)?;

    // TODO: bundle all cargo-relevant data into new struct

    // compile libs and tests and read mutable defs
    let mut report = compile(&opt, cargo_exe, &muttest_dir)?;

    let details_path = muttest_dir.join("mutable-details.csv");
    setup_csv_file(&details_path, context::DETAILS_FILE_CSV_HEAD)?;
    let coverage_path = muttest_dir.join("coverage.csv");
    setup_csv_file(&coverage_path, context::COVERAGE_FILE_CSV_HEAD)?;

    // run test suites without mutations for coverage
    for ((pkg_name, crate_name), data) in &mut report.muttest_crates {
        for test_bin in &mut report.test_bins {
            println!("call {}", &test_bin.name);
            let start_time = Instant::now();
            let status = Command::new(&test_bin.path)
                .env(context::ENV_VAR_MUTTEST_DIR, &muttest_dir)
                .env(
                    context::ENV_VAR_MUTTEST_TARGET,
                    format!("{pkg_name}:{crate_name}"),
                )
                .env(context::ENV_VAR_DETAILS_FILE, &details_path)
                .env(context::ENV_VAR_COVERAGE_FILE, &coverage_path)
                .stdout(Stdio::null())
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
    }

    // TODO: calc&print covered mutables

    for ((pkg_name, crate_name), data) in &report.muttest_crates {
        let total_mutables = data.mutables.len();
        let covered_mutables = data
            .mutables
            .values()
            .filter(|m| m.analysis.covered)
            .count();
        println!("{pkg_name}:{crate_name}: {covered_mutables}/{total_mutables} mutables covered");

        for (m_id, rep) in &data.mutables {
            if rep.analysis.covered {
                println!("cover {m_id}");
            }
        }
    }
    println!();

    let mut total_mutants = 0;
    let mut killed_mutants = 0;

    // evaluate mutations
    for ((pkg_name, crate_name), crate_report) in &mut report.muttest_crates {
        println!("{pkg_name}:{crate_name}");

        let ids = crate_report.mutables.keys().cloned().collect::<Vec<_>>();
        for &id in &ids {
            let mutable @ MutableAnalysis {
                code,
                kind,
                behavior,
                ..
            } = &crate_report.mutables[&id].analysis;
            println!(
                "{id}: {kind} `{code}` in {} ",
                crate_report.location_of(id).unwrap()
            );

            let mutations = mutations_for_mutable(&mutable)?;
            total_mutants += mutations.len();

            let Some(behavior) = &behavior else {
                println!("  not covered ({})", match mutations.len() {
                    0 => format!("no mutations"),
                    1 => format!("1 mutation"),
                    n => format!("{n} mutations"),
                });
                continue;
            };

            if mutations.is_empty() {
                println!("  no mutations");
                continue;
            }

            for m in mutations {
                print!("  mutation `{m}` ... ");
                std::io::stdout().flush()?;

                // TODO: improve weak surviving
                if kind == MutableBinopCmp::NAME
                    && mutable::binop_cmp::identical_behavior(&code, &m, behavior)
                {
                    println!("survived weak mutation testing");
                    continue;
                }

                let mut survived = true;

                // run test suites without mutations for coverage
                for test_bin in &report.test_bins {
                    let mut test = Command::new(&test_bin.path)
                        .env(
                            context::ENV_VAR_MUTTEST_TARGET,
                            format!("{pkg_name}:{crate_name}"),
                        )
                        // TODO: repeating pkg/crate part of id makes little sense here
                        .env(context::ENV_VAR_MUTTEST_MUTATION, format!("{id}={m}"))
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
                    if !result.success() {
                        survived = false;
                    }
                    // TODO: run tests in finer granularity
                }
                if !survived {
                    killed_mutants += 1;
                }
                println!("{}", if survived { "survived" } else { "killed" });
            }
        }
    }
    println!("{killed_mutants}/{total_mutants} mutants killed");

    Ok(())
}

/// execute `cargo test --no-run --message-format=json` and collect relevant output
fn compile(opt: &Opt, cargo_exe: &Path, muttest_dir: &Utf8Path) -> Result<MuttestReport, Error> {
    let mut report = MuttestReport::default();

    // TODO: pass features from opts
    let mut compile_out = Command::new(cargo_exe)
        .args(&["test", "--no-run", "--message-format=json"])
        .args(
            opt.package
                .as_deref()
                .map(|p| vec!["--package", p])
                .unwrap_or_default(),
        )
        .env("MUTTEST_DIR", muttest_dir)
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|_| Error::Cargo("test", None))?;
    let reader = std::io::BufReader::new(compile_out.stdout.take().unwrap());
    for msg in cargo_metadata::Message::parse_stream(reader) {
        let test_artifact = match msg? {
            // TODO: use `fresh` field
            cargo_metadata::Message::CompilerArtifact(a) if a.profile.test => a,
            cargo_metadata::Message::CompilerMessage(m) => {
                eprintln!("{}", m.message);
                continue;
            }
            _ => continue,
        };
        let Some(test_exe) = test_artifact.executable.as_deref() else { continue };

        let test_exe = TestBin {
            path: test_exe.as_std_path().to_owned(),
            name: test_exe.file_name().expect("test exe file name").to_owned(),
            exec_time: None,
        };

        report.test_bins.push(test_exe);
    }

    let status = compile_out.wait().map_err(|_| Error::Cargo("test", None))?;
    if !status.success() {
        return Err(Error::Cargo("test", status.code()));
    }

    for file in std::fs::read_dir(muttest_dir)? {
        let file = file?;
        let file_name = file.file_name().into_string().unwrap();
        let Some(target) = file_name
            .strip_prefix("mutable-definitions-")
            .and_then(|f| f.strip_suffix(".csv")) 
            else { continue };
        let Some((pkg_name, crate_name)) = target.split_once(':') else {continue};

        // skip packages not considered
        if opt.package.is_some() && opt.package.as_deref() != Some(pkg_name) {
            continue;
        }

        // TODO: try to delete outdated muttest files
        let file_path = muttest_dir.join(&file_name);
        let crate_report = MuttestReportForCrate::from_definition_csv(File::open(file_path)?)?;

        report
            .muttest_crates
            .insert((pkg_name.to_owned(), crate_name.to_owned()), crate_report);
    }

    Ok(report)
}

fn setup_csv_file(path: &impl AsRef<Path>, head: &str) -> Result<(), CoreError> {
    let mut file = File::create(path)?;
    write!(&mut file, "{head}")?;
    file.flush()?;
    file.sync_all()?;
    Ok(())
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
