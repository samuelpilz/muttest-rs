use std::{
    collections::BTreeMap,
    fs,
    io::{self, Write},
    process::{Command, Stdio},
};

use cargo_metadata::camino::{Utf8Path, Utf8PathBuf};
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
    let mut mutables = read_mutable_defs(&muttest_dir)?;

    let details_filepath = muttest_dir.join("details.csv");
    setup_mutable_details(&details_filepath)?;

    // run test suites without mutations for coverage
    for test_exe in &compilation_result.test_exes {
        println!("call {}", &test_exe.name);
        Command::new(&test_exe.path)
            .env("MUTTEST_DETAILS_FILE", &details_filepath)
            .stdout(Stdio::inherit())
            .spawn()?
            .wait()?;
    }

    read_mutable_details(&muttest_dir, &mut mutables)?;

    // TODO: read coverage data and possible mutations

    // evaluate mutations
    for (m_id, mutable @ Mutable { code, kind, .. }) in &mutables {
        let mutations = mutable.mutations();
        println!("{:?}: `{}` -{}-> {:?}", m_id, code, kind, &mutations);

        for m in mutations {
            println!("mutation {m}");
            // run test suites without mutations for coverage
            for test_exe in &compilation_result.test_exes {
                print!("call {}", &test_exe.name);
                io::stdout().flush()?;
                let result = Command::new(&test_exe.path)
                    .env("MUTTEST_MUTATION", format!("{}:{m}", m_id.1))
                    // .env("MUTTEST_DETAILS_FILE", &details_filepath)
                    // TODO: think about details here
                    .stdout(Stdio::null())
                    .spawn()?
                    .wait()?;
                println!(" ... {}", result.code().unwrap_or_default());
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

fn read_mutable_defs(
    muttest_dir: &Utf8Path,
) -> Result<BTreeMap<(String, usize), Mutable>, MuttestError> {
    let mut mutables = BTreeMap::new();
    for file in std::fs::read_dir(muttest_dir)? {
        let file = file?;
        let file_name = file.file_name().into_string().unwrap();
        let mutated_package = file_name
            .strip_prefix("definitions-")
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
            .map_err(|e| MuttestError::Csv(file_path.clone(), e))?;
        for md in reader.deserialize::<MutableDefinition>() {
            let md = md.map_err(|e| MuttestError::Csv(file_path.clone(), e))?;
            mutables.insert((mutated_package.to_owned(), md.id), Mutable::from_def(md));
        }
    }
    Ok(mutables)
}

fn setup_mutable_details(details_filepath: &Utf8Path) -> Result<(), MuttestError> {
    let mut details_file = fs::File::create(details_filepath)?;
    writeln!(&mut details_file, "id,kind,data")?;
    details_file.flush()?;
    details_file.sync_all()?;
    Ok(())
}

fn read_mutable_details(
    muttest_dir: &Utf8Path,
    mutables: &mut BTreeMap<(String, usize), Mutable>,
) -> Result<(), MuttestError> {
    let details_file = muttest_dir.join("details.csv");
    let mut reader = csv::ReaderBuilder::new()
        .from_path(&details_file)
        .map_err(|e| MuttestError::Csv(details_file.clone(), e))?;
    for md in reader.deserialize::<MutableDetails>() {
        let md = md.map_err(|e| MuttestError::Csv(details_file.clone(), e))?;

        let id = md.id.split_once(":").expect("id format");
        let id = (id.1.to_owned(), id.0.parse::<usize>().expect("id format"));
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

#[derive(Debug, thiserror::Error)]
pub enum MuttestError {
    #[error("failed to run `cargo {0}`{}", display_exit_code_msg(.1))]
    Cargo(&'static str, Option<i32>),
    #[error("failed to read cargo metadata")]
    CargoMetadata(#[from] cargo_metadata::Error),
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("failed to read file {0}. {1}")]
    Csv(Utf8PathBuf, csv::Error),
}

fn display_exit_code_msg(e: &Option<i32>) -> String {
    match e {
        None => String::new(),
        Some(e) => format!(". Exited with code {e}"),
    }
}
