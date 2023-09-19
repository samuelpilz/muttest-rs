use std::{
    cell::RefCell,
    fs::{self, File},
    io::{self, Write},
    ops::Not,
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    rc::Rc,
    time::Instant,
};

use cargo_metadata::camino::Utf8PathBuf;
use clap::Parser;
use log::{debug, info};
use muttest_core::{
    context::{
        self, attr_location_path, coverage_path, location_path, types_path, MuttestMode,
        ENV_VAR_MUTTEST_DIR, ENV_VAR_MUTTEST_MODE,
    },
    display_or_empty_if_none,
    report::{
        CrateId, MutableId, MutationResult, MuttestReport, MuttestReportForCrate, Ratio,
        ReportSummary, TestBin,
    },
    transformer::{SPAN_TEST_PREFIX, TEST_PREFIX},
    EXIT_MUTTEST_ERROR, EXIT_TIMEOUT,
};

#[derive(Debug, Parser)]
struct Opt {
    #[clap(long)]
    workspace: bool,

    #[clap(long)]
    all_features: bool,

    #[clap(short = 'F', long, name = "FEATURES")]
    features: Vec<String>,

    #[clap(long)]
    no_default_features: bool,

    #[clap(long, value_name = "PROFILE-NAME")]
    profile: Option<String>,

    /// run only unit tests
    #[clap(long)]
    lib: bool,

    #[clap(short, long)]
    package: Vec<String>,

    // timeout for test cases (in seconds)
    #[clap(long, value_name = "TIMEOUT")]
    test_timeout: Option<String>,

    /// perform weak mutation testing only
    #[clap(long)]
    weak: bool,

    /// compute a kill matrix
    #[clap(long)]
    kill_matrix: bool,

    /// run all tests, even ones not annotated `#[muttest::tests]`
    #[clap(long)]
    all_tests: bool,

    /// threshold for weak mutation score, in percent (1..100]
    #[clap(long, value_name = "THRESHOLD")]
    weak_threshold: Option<f64>,

    /// threshold for mutation score, in percent (1..100]
    #[clap(long, value_name = "THRESHOLD")]
    threshold: Option<f64>,

    /// select mutators to skip. Repeat this argument for skipping multiple mutants.
    /// Conflicts with `--mutators`
    #[clap(long = "skip-mutator", value_name = "MUTATOR")]
    skip_mutators: Vec<String>,

    /// select mutators to apply. Repeat this argument for selecting multiple mutators.
    #[clap(long = "mutator", value_name = "MUTATOR")]
    mutators: Vec<String>,

    /// If specified, only analyze files matching this glob
    #[clap(name = "FILE")]
    mutate_files: Vec<String>,
}

#[derive(Debug, Parser)]
enum OptCargoPlugin {
    #[clap(name = "muttest", version, author, dont_collapse_args_in_usage = true)]
    Muttest(Opt),
}
impl From<OptCargoPlugin> for Opt {
    fn from(OptCargoPlugin::Muttest(o): OptCargoPlugin) -> Self {
        o
    }
}

fn main() -> Result<(), Error> {
    env_logger::init();

    let is_cargo_plugin = std::env::var_os("CARGO").is_some();
    let opt = if is_cargo_plugin {
        OptCargoPlugin::parse().into()
    } else {
        Opt::parse()
    };

    debug!("cli options {opt:?}");

    if let Some(t) = opt.weak_threshold {
        if !(1.0 < t || t <= 100.0) {
            return Err(Error::ThresholdRange(t));
        }
    }
    if let Some(t) = opt.threshold {
        if !(1.0 < t || t <= 100.0) {
            return Err(Error::ThresholdRange(t));
        }
    }

    // read cargo metadata
    // TODO: pass opts to metadata command
    let cargo_metadata = cargo_metadata::MetadataCommand::new().exec()?;
    let muttest_dir = cargo_metadata.target_directory.join("muttest");
    fs::create_dir_all(&muttest_dir)?;

    let report = Rc::new(RefCell::new(MuttestReport::default()));
    let _write_report_on_exit = WriteReportOnExit(muttest_dir.clone(), Rc::clone(&report));
    let mut report = report.borrow_mut();
    let report = &mut *report;

    // compile libs and tests and read mutable defs
    let cargo_exe = std::env::var_os("CARGO");
    let cargo_exe = cargo_exe
        .as_ref()
        .map(Path::new)
        .unwrap_or_else(|| Path::new("cargo"));

    let mut base_env = vec![(ENV_VAR_MUTTEST_DIR, muttest_dir.as_str())];
    if let Some(t) = &opt.test_timeout {
        base_env.push((context::ENV_VAR_MUTTEST_TEST_TIMEOUT, t));
    }

    info!("build tests");
    debug!("cargo test --no-run --message-format=json ...");
    let mut compile_out = Command::new(cargo_exe)
        .args(["test", "--no-run", "--message-format=json"])
        .args(opt.package.iter().flat_map(|p| ["--package", p]))
        .args(opt.profile.iter().flat_map(|p| ["--profile", p]))
        .args(opt.features.iter().flat_map(|f| ["-F", f]))
        .args(opt.all_features.then_some("--all-features"))
        .args(opt.no_default_features.then_some("--no-default-features"))
        .args(opt.lib.then_some("--lib"))
        .args(opt.workspace.then_some("--workspace"))
        .envs(base_env.iter().copied())
        .env::<_, &'static str>(ENV_VAR_MUTTEST_MODE, MuttestMode::Build.into())
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|_| Error::Cargo("test", None))?;
    let reader = std::io::BufReader::new(compile_out.stdout.take().unwrap());
    for msg in cargo_metadata::Message::parse_stream(reader) {
        let test_artifact = match msg? {
            cargo_metadata::Message::CompilerArtifact(a)
                if a.profile.test && a.target.crate_types.iter().all(|t| t != "proc-macro") =>
            {
                a
            }
            cargo_metadata::Message::CompilerMessage(m) => {
                eprintln!("{}", m.message);
                continue;
            }
            _ => continue,
        };
        let Some(test_exe) = test_artifact.executable.as_deref() else {
            continue;
        };

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

    for file in std::fs::read_dir(&muttest_dir)? {
        let file = file?;
        let file_name = file.file_name().into_string().unwrap();
        let Some(target) = file_name
            .strip_prefix("mutable-definitions-")
            .and_then(|f| f.strip_suffix(".csv"))
        else {
            continue;
        };
        let Ok(crate_id) = target.parse::<CrateId>() else {
            continue;
        };

        // skip packages not considered
        if !opt.package.is_empty() && opt.package.iter().all(|p| p != &*crate_id.pkg_name) {
            continue;
        }
        // TODO: don't fail to read coverage csv if package is given

        // TODO: try to delete outdated muttest files
        let file_path = muttest_dir.join(&file_name);
        debug!("read definitions file {file_path}");
        let crate_report = MuttestReportForCrate::from_definition_csv(File::open(file_path)?)?;

        report.muttest_crates.insert(crate_id, crate_report);
    }

    info!("test without mutation");
    // run test suites without mutations for coverage
    let types_path = utf8_path_buf(types_path(&muttest_dir))?;
    setup_csv_file(&types_path, context::TYPES_FILE_CSV_HEAD)?;
    let coverage_path = utf8_path_buf(coverage_path(&muttest_dir))?;
    setup_csv_file(&coverage_path, context::COVERAGE_FILE_CSV_HEAD)?;
    let attr_location_path = utf8_path_buf(attr_location_path(&muttest_dir))?;
    setup_csv_file(&attr_location_path, context::ATTR_LOCATION_FILE_CSV_HEAD)?;
    let location_path = utf8_path_buf(location_path(&muttest_dir))?;
    setup_csv_file(&location_path, context::LOCATION_FILE_CSV_HEAD)?;
    for test_bin in &mut report.test_bins {
        let start_time = Instant::now();
        let status = run_test_bin(
            &opt,
            &test_bin.path,
            &[
                &*base_env,
                &[(ENV_VAR_MUTTEST_MODE, MuttestMode::Test.into())],
            ],
            Stdio::inherit(),
            Stdio::null(),
            true,
        )?
        .wait_with_output()?;
        // TODO: catch wait-timeout?
        test_bin.exec_time = Some(start_time.elapsed());
        // TODO: better error report
        if !status.status.success() {
            return Err(match status.status.code() {
                Some(EXIT_TIMEOUT) => Error::TestSuiteTimeout,
                // TODO: print hint that timeout flag should be set
                Some(EXIT_MUTTEST_ERROR) => Error::InternalError,
                _ => Error::TestSuiteFails,
            });
        }
    }
    debug!("read {types_path}");
    report
        .read_types_csv(File::open(&types_path)?)
        .map_err(|e| e.in_csv_file(&types_path))?;
    debug!("read {coverage_path}");
    report
        .read_coverage_csv(File::open(&coverage_path)?)
        .map_err(|e| e.in_csv_file(&coverage_path))?;
    debug!("read {attr_location_path}");
    report
        .read_attr_location_csv(File::open(&attr_location_path)?)
        .map_err(|e| e.in_csv_file(&attr_location_path))?;
    debug!("read {location_path}");
    report
        .read_location_csv(File::open(&location_path)?)
        .map_err(|e| e.in_csv_file(&location_path))?;

    if !opt.mutate_files.is_empty() {
        report.filter_files(&opt.mutate_files);
    }
    if !opt.mutators.is_empty() {
        report.filter_mutators(|m| opt.mutators.iter().any(|m1| m.contains(m1)));
    }
    if !opt.skip_mutators.is_empty() {
        report.filter_mutators(|m| !opt.skip_mutators.iter().any(|m1| m.contains(m1)));
    }
    report.weak_mutation_analysis()?;

    let mut total_summary = ReportSummary::default();
    for (crate_id, crate_report) in &report.muttest_crates {
        let summary = crate_report.summary();

        println!("\n# Weak Analysis {crate_id}");
        println!("{} mutables covered", summary.ratio_mutables_covered());
        println!("{} mutations covered", summary.ratio_mutations_covered());
        println!(
            "{} mutants survived weak mutation analysis",
            summary.ratio_mutations_killed_weak()
        );

        total_summary += summary;
    }
    if report.muttest_crates.len() > 1 {
        println!("\n# Total Weak Analysis");
        println!(
            "{} mutables covered",
            total_summary.ratio_mutables_covered()
        );
        println!(
            "{} mutations covered",
            total_summary.ratio_mutations_covered()
        );
        println!(
            "{} mutants killed by weak mutation analysis",
            total_summary.ratio_mutations_killed_weak()
        );
    }
    if let Some(t) = opt.weak_threshold {
        let ratio = total_summary.ratio_mutations_killed_weak();
        if ratio.percent() < t {
            return Err(Error::Threshold(ratio, t));
        }
    }
    if opt.weak {
        return Ok(());
    }

    // evaluate mutations
    let mut total_summary = ReportSummary::default();
    for (crate_id, crate_report) in &mut report.muttest_crates {
        println!("\n# Analysis {crate_id}\n");

        let mut current_file = "";

        // process all mutables
        for (&m_id, mutable) in &mut crate_report.mutables {
            let Some(attr_loc) = crate_report.attrs.get(&m_id.attr_id) else {
                continue;
            };
            if current_file != attr_loc.file {
                println!("\n## File {}\n", attr_loc.file);
                current_file = &attr_loc.file;
            }
            let mutable_id = MutableId {
                crate_id: crate_id.clone(),
                id: m_id,
            };
            let analysis = &mutable.analysis;
            println!(
                "{m_id}: `{}` in {} ",
                analysis.code,
                display_or_empty_if_none(&mutable.location.span)
            );

            println!(
                "  {}, {}",
                match mutable.mutations.len() {
                    0 => "no mutations".to_owned(),
                    1 => "1 mutation".to_owned(),
                    x => format!("{x} mutations"),
                },
                match analysis.behavior.len() {
                    0 => "not covered".to_owned(),
                    1 => "covered by 1 test".to_owned(),
                    x => format!("covered by {x} tests"),
                },
            );
            if !analysis.is_covered() {
                continue;
            }

            for (m, m_report) in &mut mutable.mutations {
                print!("  - mutation `{m}` ... ");
                std::io::stdout().flush()?;

                if m_report.result == Some(MutationResult::IdenticalBehavior) {
                    println!("survived weak mutation analysis");
                    continue;
                }

                let mut result = MutationResult::Survived;

                for test_bin in &report.test_bins {
                    // TODO: filter relevant_tests for this test bin only
                    let relevant_tests = m_report
                        .relevant_tests
                        .iter()
                        .map(|x| x.as_ref().map(|x| x.to_string()).unwrap_or_default())
                        .collect::<Vec<_>>()
                        .join("\x1f");

                    let mutate_mode = if opt.kill_matrix {
                        MuttestMode::MutateKillMatrix
                    } else {
                        MuttestMode::Mutate
                    };
                    let mut test = run_test_bin(
                        &opt,
                        &test_bin.path,
                        &[
                            &*base_env,
                            &[
                                (
                                    context::ENV_VAR_MUTTEST_MUTATION,
                                    &format!("{mutable_id}={m}"),
                                ),
                                (context::ENV_VAR_MUTTEST_MODE, mutate_mode.into()),
                                (context::ENV_VAR_MUTTEST_TESTS, &relevant_tests),
                            ],
                        ],
                        Stdio::null(),
                        Stdio::null(),
                        false,
                    )?;
                    let test_exit_code = test.wait()?;
                    if !test_exit_code.success() {
                        result = match test_exit_code.code() {
                            Some(EXIT_TIMEOUT) => MutationResult::KilledByTimeout,
                            Some(EXIT_MUTTEST_ERROR) => {
                                return Err(Error::InternalError);
                            }
                            _ => MutationResult::KilledByTestFail,
                        };
                        break;
                    }
                }
                m_report.result = Some(result);
                match result {
                    MutationResult::IdenticalBehavior => {}
                    MutationResult::Survived => println!("survived"),
                    MutationResult::KilledByTestFail => println!("killed"),
                    MutationResult::KilledByTimeout => println!("killed (timeout)"),
                }
            }
        }
        let summary = crate_report.summary();
        total_summary += summary;

        println!("{} mutants killed", summary.ratio_mutations_killed());
    }

    if report.muttest_crates.len() > 1 {
        println!("\n# Total Analysis");
        println!("{} mutants killed", total_summary.ratio_mutations_killed());
    }

    if let Some(t) = opt.threshold {
        let ratio = total_summary.ratio_mutations_killed_weak();
        if ratio.percent() < t {
            return Err(Error::Threshold(ratio, t));
        }
    }

    Ok(())
}

fn run_test_bin(
    opt: &Opt,
    bin_path: &Path,
    env: &[&[(&str, &str)]],
    stdout: Stdio,
    stderr: Stdio,
    print_spans: bool,
) -> Result<Child, Error> {
    // TODO: refactor this function. it was developed under assumptions that no longer hold
    let env = || env.iter().copied().flatten().copied();
    let args = [
        opt.all_tests.not().then_some(TEST_PREFIX),
        print_spans.then_some(SPAN_TEST_PREFIX),
        print_spans.not().then_some("--nocapture"),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>();

    debug!(
        "{}{}{}",
        env()
            .map(|(name, var)| format!("{name}={var:?} "))
            .collect::<String>(),
        bin_path.display(),
        args.iter().map(|a| format!(" {a}")).collect::<String>(),
    );

    let child = Command::new(bin_path)
        .args(args)
        .envs(env())
        .stdout(stdout)
        .stderr(stderr)
        .spawn()?;
    Ok(child)
}

fn setup_csv_file(path: &impl AsRef<Path>, head: &str) -> Result<(), CoreError> {
    let mut file = File::create(path)?;
    write!(&mut file, "{head}")?;
    file.flush()?;
    file.sync_all()?;
    Ok(())
}

fn utf8_path_buf(p: PathBuf) -> Result<Utf8PathBuf, Error> {
    Utf8PathBuf::from_path_buf(p).map_err(Error::Utf8Path)
}

struct WriteReportOnExit(Utf8PathBuf, Rc<RefCell<MuttestReport>>);
impl Drop for WriteReportOnExit {
    fn drop(&mut self) {
        fs::write(
            self.0.join("report.json"),
            serde_json::to_string(&*self.1.borrow()).expect("unable to format report"),
        )
        .expect("unable to write report");
    }
}

type CoreError = muttest_core::Error;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to run `cargo {0}`{}", display_exit_code_msg(.1))]
    Cargo(&'static str, Option<i32>),
    #[error("failed to read cargo metadata")]
    CargoMetadata(#[from] cargo_metadata::Error),
    #[error(transparent)]
    CoreError(#[from] CoreError),
    #[error("failed to read csv file {0}. {1}")]
    Csv(Utf8PathBuf, csv::Error),
    #[error("path not utf-8: '{0}'")]
    Utf8Path(PathBuf),
    #[error("test suite fails")]
    TestSuiteFails,
    #[error("test suite timeout")]
    TestSuiteTimeout,
    #[error("threshold out of range {0}. Allowed range (1..100]")]
    ThresholdRange(f64),
    #[error("muttest internal error")]
    InternalError,
    #[error("mutation score too low: {0}. Required {1}%")]
    Threshold(Ratio, f64),
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
