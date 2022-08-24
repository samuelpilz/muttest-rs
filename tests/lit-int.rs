#[muttest_codegen::mutate_selftest]
fn two_usize() -> usize {
    2
}

#[test]
fn test_unchanged() {
    assert_eq!(
        ::muttest_core::mock::without_mutation(two_usize),
        2
    );
}

#[test]
fn test_1() {
    assert_eq!(
        ::muttest_core::mock::with_mutation(1, "1", two_usize),
        1
    );
}

#[test]
fn test_4() {
    assert_eq!(
        ::muttest_core::mock::with_mutation(1, "4", two_usize),
        4
    );
}
