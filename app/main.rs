use std::{
    borrow::Cow,
    fs::{self, File},
    io::{self, Write},
    path::PathBuf,
    process::{Command, Stdio},
};

use cargo_metadata::camino::{Utf8Path, Utf8PathBuf};
use clap::Parser;
use muttest_core::{
    mutable::{binop_cmp::MutableBinopCmp, lit_int::MutableLitInt},
    transformer::Mutable,
    CollectedData, MutableData, MutableDataCollector, MutableId, ENV_VAR_MUTTEST_DIR,
};
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
    let cargo_exe = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_owned());
    let cargo_metadata = cargo_metadata::MetadataCommand::new().exec()?;
    let muttest_dir = cargo_metadata.target_directory.join("muttest");
    fs::create_dir_all(&muttest_dir)?;

    // compile libs and test cases
    let compilation_result = compile(&cargo_exe, &muttest_dir)?;

    println!("{compilation_result:?}");

    // read created mutables
    let mut data = read_mutable_defs(&muttest_dir)?;

    println!("mutable definitions {data:?}");

    let details_path = muttest_dir.join("mutable-details.csv");
    setup_mutable_details(&details_path)?;

    // run test suites without mutations for coverage
    for test_exe in &compilation_result.test_exes {
        println!("call {}", &test_exe.name);
        Command::new(&test_exe.path)
            .env(ENV_VAR_MUTTEST_DIR, &muttest_dir)
            .env(MutableDataCollector::ENV_VAR_DETAILS_FILE, &details_path)
            .stdout(Stdio::inherit())
            .spawn()?
            .wait()?;
    }

    data.read_details_csv(File::open(details_path)?)?;

    // evaluate mutations
    let m_ids = data.mutables.keys().cloned().collect::<Vec<_>>();
    for m_id in m_ids {
        let mutable @ MutableData { code, kind, .. } = &data.mutables[&m_id];
        let mutations = mutations_for_mutable(&mutable);
        println!("{m_id:?}: `{code}` -{kind}-> {mutations:?}");

        let mutations = match mutations {
            Some(m) => m,
            None => continue,
        };

        for m in mutations {
            println!("mutation {m}");
            // run test suites without mutations for coverage
            for test_exe in &compilation_result.test_exes {
                print!("call {}", &test_exe.name);
                io::stdout().flush()?;
                let result = Command::new(&test_exe.path)
                    .env(
                        "MUTTEST_MUTATION",
                        format!("{}:{}={m}", m_id.id, m_id.crate_name),
                    )
                    // TODO: think about details here
                    .stdout(Stdio::null())
                    .stderr(Stdio::inherit())
                    .spawn()?
                    .wait()?;
                let exit_code = match result.code() {
                    None => "TIMEOUT",
                    Some(0) => "survived",
                    Some(_) => "killed",
                };
                println!(" ... {}", exit_code);
                // TODO: if code is none, then error
                // TODO: run tests in finer granularity
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
struct MutableDefinition {
    id: usize,
    kind: String,
    code: String,
    loc: String,
}
/// execute `cargo test --no-run --message-format=json` and collect output
fn compile(cargo_exe: &str, muttest_dir: &Utf8Path) -> Result<CompilationResult, Error> {
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

        let test_exe = TestExe {
            path: test_exe.to_owned(),
            name: test_exe.file_name().expect("test exe file name").to_owned(),
        };

        result.test_exes.push(test_exe);
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

        // read mutable
        let file_path = muttest_dir.join(&file_name);
        let mut reader = csv::ReaderBuilder::new()
            .from_path(&file_path)
            .map_err(|e| Error::Csv(file_path.as_std_path().to_owned(), e))?;
        for md in reader.deserialize::<MutableDefinition>() {
            let md = md.map_err(|e| Error::Csv(file_path.as_std_path().to_owned(), e))?;
            let id = MutableId {
                id: md.id,
                crate_name: Cow::Owned(mutated_package.to_owned()),
            };
            data.mutables.insert(
                id,
                MutableData {
                    kind: md.kind,
                    code: md.code,
                    location: md.loc,
                    ..MutableData::default()
                },
            );
        }
    }
    Ok(data)
}

fn setup_mutable_details(details_filepath: &Utf8Path) -> Result<(), CoreError> {
    let mut details_file = fs::File::create(details_filepath)?;
    writeln!(&mut details_file, "id,kind,data")?;
    details_file.flush()?;
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
        MutableBinopCmp::NAME => ["<", "<=", ">=", ">"]
            .into_iter()
            .filter(|x| x != &mutable.code)
            .map(ToOwned::to_owned)
            .collect(),
        // fallback to mutable's description of possible mutations
        _ => mutable
            .possible_mutations
            .as_ref()?
            .iter()
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
