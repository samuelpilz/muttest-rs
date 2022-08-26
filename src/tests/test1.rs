#[muttest_codegen::mutate_isolated]
fn x() -> usize {
    let y = 1;
    1 + 2 + y
}

#[muttest_codegen::mutate_isolated]
fn z() -> bool {
    1 < 2
}

#[muttest_codegen::mutate_isolated]
fn a() {
    for _ in 1..5 + 1 {
        1;
    }
}

#[test]
fn tests() {
    a();
    assert!(z());
    assert_eq!(x(), 4);
}
