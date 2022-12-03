#![allow(clippy::all)]

// see: https://github.com/llogiq/mutagen/issues/144
#[muttest_codegen::mutate_isolated]
pub fn iter_over_bool_vec() -> usize {
    let xs = vec![true, false, true, false, true];
    let mut i = 0;
    for _x in xs {
        i += 1;
    }
    i
}
