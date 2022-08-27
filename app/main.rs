use std::{
    borrow::Cow,
    collections::BTreeMap,
    fs,
    io::{self, Write},
    process::{Command, Stdio}, path::PathBuf,
};

use cargo_metadata::camino::{Utf8Path, Utf8PathBuf};
use clap::Parser;
use muttest_core::MutableId;
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
    let mut mutables = read_mutable_defs(&muttest_dir)?;

    println!("Mutables {mutables:?}");

    let details_filepath = muttest_dir.join("mutable-details.csv");
    setup_mutable_details(&details_filepath)?;

    // run test suites without mutations for coverage
    for test_exe in &compilation_result.test_exes {
        println!("call {}", &test_exe.name);
        Command::new(&test_exe.path)
            .env("MUTTEST_DIR", &muttest_dir)
            .stdout(Stdio::inherit())
            .spawn()?
            .wait()?;
    }

    read_mutable_details(&muttest_dir, &mut mutables)?;

    // TODO: read coverage data and possible mutations

    // evaluate mutations
    for (m_id, mutable @ Mutable { code, kind, .. }) in &mutables {
        let mutations = mutable.mutations();
        println!("{m_id:?}: `{code}` -{kind}-> {mutations:?}");

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

#[derive(Debug, Deserialize, Default)]
struct Mutable {
    kind: String,
    code: String,
    loc: Option<String>,
    ty: Option<String>,
    possible_mutations: Option<Vec<String>>,
}
impl Mutable {
    fn from_def(
        MutableDefinition {
            kind, code, loc, ..
        }: MutableDefinition,
    ) -> Self {
        Self {
            kind,
            code,
            loc: Some(loc).filter(String::is_empty),
            ..Self::default()
        }
    }
    fn mutations(&self) -> Vec<String> {
        match &*self.kind {
            "int" => {
                let i = self.code.parse::<u128>().expect("unable to parse int");
                let mut m = vec![];
                if i != 0 {
                    m.push((i - 1).to_string());
                }
                m.push((i + 1).to_string());
                m
            }
            "calc" => self
                .possible_mutations
                .iter()
                .flatten()
                .filter(|&x| x != &self.code)
                .map(ToOwned::to_owned)
                .collect(),
            "cmp" => ["<", "<=", ">=", ">"]
                .into_iter()
                .filter(|x| x != &self.code)
                .map(ToOwned::to_owned)
                .collect(),
            _ => vec![],
        }
    }
}

#[derive(Debug, Deserialize)]
struct MutableDefinition {
    id: usize,
    kind: String,
    code: String,
    loc: String,
}
#[derive(Debug, Deserialize)]
struct MutableDetails {
    id: String,
    kind: String,
    data: String,
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

fn read_mutable_defs(muttest_dir: &Utf8Path) -> Result<BTreeMap<MutableId, Mutable>, Error> {
    let mut mutables = BTreeMap::new();
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
            mutables.insert(id, Mutable::from_def(md));
        }
    }
    Ok(mutables)
}

fn setup_mutable_details(details_filepath: &Utf8Path) -> Result<(), CoreError> {
    let mut details_file = fs::File::create(details_filepath)?;
    writeln!(&mut details_file, "id,kind,data")?;
    details_file.flush()?;
    details_file.sync_all()?;
    Ok(())
}

// TODO: generic function for csv reader
fn read_mutable_details(
    muttest_dir: &Utf8Path,
    mutables: &mut BTreeMap<MutableId, Mutable>,
) -> Result<(), Error> {
    let details_file = muttest_dir.join("mutable-details.csv");
    let mut reader = csv::ReaderBuilder::new()
        .from_path(&details_file)
        .map_err(|e| Error::Csv(details_file.as_std_path().to_owned(), e))?;
    for md in reader.deserialize::<MutableDetails>() {
        let md = md.map_err(|e| Error::Csv(details_file.as_std_path().to_owned(), e))?;

        let id = md.id.parse().unwrap();
        if let Some(m) = mutables.get_mut(&id) {
            match &*md.kind {
                "type" => m.ty = Some(md.data),
                "mutations" => {
                    m.possible_mutations = Some(md.data.split(":").map(ToOwned::to_owned).collect())
                }
                "loc" => {
                    m.loc.get_or_insert(md.data);
                }
                _ => {}
            }
        }
        // mutables.insert((mutated_package.to_owned(), md.id), Mutable::from_def(md));
    }
    // TODO: read details file and add to mutables

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
