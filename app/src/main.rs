use std::{
    io,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use cargo_metadata::camino::Utf8PathBuf;
use clap::Parser;
use serde::Deserialize;

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

fn main() -> Result<(), MuttestError> {
    let is_cargo_plugin = std::env::var_os("CARGO").is_some();
    let opt = if is_cargo_plugin {
        OptCargoPlugin::parse().into()
    } else {
        Opt::parse()
    };
    println!("{:?}", opt);

    // TODO: pass features from opts
    // read cargo metadata
    let cargo_exe = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_owned());
    let cargo_metadata = cargo_metadata::MetadataCommand::new().exec()?;
    let muttest_dir = cargo_metadata.target_directory.join("muttest");

    // compile libs and test cases
    let compilation_result = compile(&cargo_exe)?;

    println!("{compilation_result:?}");

    // read created mutables
    let mutable_files = read_mutables(muttest_dir.as_std_path())?;

    // run test suites without mutations for coverage
    for test_exe in &compilation_result.test_exes {
        println!("call {}", &test_exe.name);
        Command::new(&test_exe.path)
            .stdout(Stdio::inherit())
            .spawn()?
            .wait()?;
    }
    // TODO: read coverage data and possible mutations

    // evaluate mutations
    for mf in &mutable_files {
        println!("TARGET {}: {}", mf.0, mf.1.len());
        for mutable @ Mutable { id, code, kind, .. } in &mf.1 {
            let mutations = mutable.mutations();
            println!("{}: `{}` -{}-> {:?}", id, code, kind, &mutations);

            for m in mutations {
                // run test suites without mutations for coverage
                for test_exe in &compilation_result.test_exes {
                    println!("call {} with {id}:{m}", &test_exe.name);
                    let result = Command::new(&test_exe.path)
                        .env("MUTTEST_MUTATION", format!("{id}:{m}"))
                        .stdout(Stdio::null())
                        .spawn()?
                        .wait()?;
                    println!("      ... {}", result.code().unwrap_or_default());
                    // TODO: run tests in finer granularity
                }
            }
        }
    }

    // TODO: make report

    Ok(())
}

#[derive(Debug, Default)]
struct CompilationResult {
    test_exes: Vec<TestExe>,
}

#[derive(Debug)]
struct TestExe {
    path: Utf8PathBuf,
    name: String,
}

#[derive(Debug, Deserialize)]
#[allow(unused)]
struct Mutable {
    id: usize,
    kind: String,
    code: String,
    loc: String,
}

impl Mutable {
    fn mutations(&self) -> Vec<String> {
        match &*self.kind {
            "int" => vec!["1000".to_owned()],
            "calc" => vec!["+", "-", "*", "/", "%"]
                .into_iter()
                .filter(|x| x != &self.code)
                .map(ToOwned::to_owned)
                .collect(),
            _ => vec![],
        }
    }
}

/// execute `cargo test --no-run --message-format=json` and collect output
fn compile(cargo_exe: &str) -> Result<CompilationResult, MuttestError> {
    let mut result = CompilationResult::default();

    // TODO: pass features from opts
    let mut compile_out = Command::new(cargo_exe)
        .args(&["test", "--no-run", "--message-format=json"])
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|_| MuttestError::Cargo("test", None))?;
    let reader = std::io::BufReader::new(compile_out.stdout.take().unwrap());
    for msg in cargo_metadata::Message::parse_stream(reader) {
        let test_artifact = match msg? {
            cargo_metadata::Message::CompilerArtifact(a) if a.profile.test => a,
            _ => continue,
        };
        let test_exe = match test_artifact.executable.as_deref() {
            Some(e) => e,
            None => continue,
        };

        let test_exe = TestExe {
            path: test_exe.to_owned(),
            name: test_exe.file_name().expect("test exe file name").to_owned(),
        };

        result.test_exes.push(test_exe);
    }

    let status = compile_out
        .wait()
        .map_err(|_| MuttestError::Cargo("test", None))?;
    if !status.success() {
        return Err(MuttestError::Cargo("test", status.code()));
    }

    Ok(result)
}

fn read_mutables(muttest_dir: &Path) -> Result<Vec<(String, Vec<Mutable>)>, MuttestError> {
    let mut mutables = vec![];
    for file in std::fs::read_dir(muttest_dir)? {
        let file = file?;
        let file_name = file.file_name().into_string().unwrap();
        let mutated_package = file_name
            .strip_prefix("mutable-")
            .and_then(|f| f.strip_suffix(".csv"));
        let mutated_package = match mutated_package {
            None => continue,
            Some(n) => n,
        };
        // TODO: delete outdated mutable files

        // read mutable
        let file_path = muttest_dir.join(&file_name);
        let mut reader = csv::ReaderBuilder::new()
            .from_path(&file_path)
            .map_err(|e| MuttestError::Csv(file_path.clone(), e))?;
        let ms = reader
            .deserialize()
            .collect::<Result<Vec<Mutable>, _>>()
            .map_err(|e| MuttestError::Csv(file_path.clone(), e))?;

        mutables.push((mutated_package.to_owned(), ms));
    }
    Ok(mutables)
}

#[derive(Debug, thiserror::Error)]
pub enum MuttestError {
    #[error("failed to run `cargo {0}`{}", display_exit_code_msg(.1))]
    Cargo(&'static str, Option<i32>),
    #[error("failed to read cargo metadata")]
    CargoMetadata(#[from] cargo_metadata::Error),
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("failed to read file {0}. {1}")]
    Csv(PathBuf, csv::Error),
}

fn display_exit_code_msg(e: &Option<i32>) -> String {
    match e {
        None => String::new(),
        Some(e) => format!(". Exited with code {e}"),
    }
}
