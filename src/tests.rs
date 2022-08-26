
#[muttest_codegen::mutate_selftest]
fn asdf() -> usize {
    1
}

#[test]
#[ignore]
// this test requires `RUST_TEST_THREADS=1`
fn asdf1() {
    assert_eq!(asdf(), 1);
}

mod lit_str;
mod lit_int;

// TODO: restructure these tests
mod test1;
mod speculative;
