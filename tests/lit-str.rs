#[muttest_codegen::mutate_selftest]
fn empty_str() -> &'static str {
    ""
}
#[muttest_codegen::mutate_selftest]
fn some_str() -> &'static str {
    "mutation testing!"
}

#[test]
fn empty_str_unchanged() {
    assert_eq!(
        ::muttest_core::mock::without_mutation(empty_str),
        ""
    );
}

#[test]
fn empty_str_one() {
    assert_eq!(
        ::muttest_core::mock::with_mutation(1, "1", empty_str),
        "1"
    );
}

#[test]
fn some_str_unchanged() {
    assert_eq!(
        ::muttest_core::mock::without_mutation(some_str),
        "mutation testing!"
    );
}

#[test]
fn some_str_empty() {
    assert_eq!(
        ::muttest_core::mock::with_mutation(1, "", some_str),
        ""
    );
}
