#[muttest_codegen::mutate_isolated]
fn x() -> usize {
    let y = 1;
    1 + 2 + y
}

#[muttest_codegen::mutate_isolated]
fn a() {
    for _ in 1..5 + 1 {
        1;
    }
}

#[test]
fn tests() {
    super::without_mutation(|| {
        a();
        assert_eq!(x(), 4);
    });
}
