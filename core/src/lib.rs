use std::{
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Debug, thiserror::Error)]
pub enum MuttestError {
    #[error("failed to run `cargo {0}`. Exit code {1}")]
    CargoError(String, i32)
}

pub fn get_muttest_dir() -> Result<PathBuf, MuttestError> {
    let metadata = Command::new("cargo")
        .arg("metadata")
        .output()
        .expect("unable to get output of `cargo metadata`");
    if !metadata.status.success() {
        panic!("{}", std::str::from_utf8(&metadata.stderr).unwrap());
    }
    let meta_json =
        serde_json::from_str::<serde_json::Value>(std::str::from_utf8(&metadata.stdout).unwrap())
            .unwrap();
    let root_dir = Path::new(
        meta_json["target_directory"].as_str().unwrap(),
        // .ok_or_else(|| format_err!("cargo metadata misses workspace_root"))?,
    );
    Ok(root_dir.join("muttest"))
}
