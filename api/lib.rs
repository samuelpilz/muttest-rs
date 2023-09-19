pub use muttest_codegen::mutate;
pub use muttest_codegen::tests;
pub use muttest_core::api;

// ensure that this is inferrable
// see: https://github.com/llogiq/mutagen/issues/164
// in particular, if you include `serde_json`, this will fail to compile
#[cfg(test)]
#[test]
fn ensure_this_is_inferrable() {
    let x = [0u8];
    assert_ne!(&x[..], []);
}

// TODO: remove when `proc_macro::tracked_env` is stable https://github.com/rust-lang/rust/issues/99515
const _RECOMPILE_ON_ENVVAR_CHANGE: Option<&str> = option_env!("MUTTEST_DIR");
