#[muttest_codegen::mutate_isolated]
fn two_usize() -> usize {
    2
}

#[test]
fn two_usize_mutables() {
    assert_eq!(two_usize::NUM_MUTABLES, 1);
    assert_eq!(two_usize::MUTABLES_CSV.lines().count(), 2);
}

#[test]
fn two_usize_unchanged() {
    assert_eq!(crate::mock::without_mutation(two_usize), 2);
}

#[test]
fn two_usize_1() {
    assert_eq!(crate::mock::with_mutation(1, "1", two_usize), 1);
}

#[test]
fn two_usize_4() {
    assert_eq!(crate::mock::with_mutation(1, "4", two_usize), 4);
}

// TODO: also test invalid mutations
