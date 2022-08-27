pub use muttest_core::api::*;
pub use muttest_codegen::mutate;

// ensure that this is inferrable
// see: https://github.com/llogiq/mutagen/issues/164
// in particular, if you include `serde_json`, this will fail to compile
#[cfg(test)]
fn _ensure_this_is_inferrable() {
    let x = [0u8];
    assert_eq!((&x[..]).as_ref(), []);
}
