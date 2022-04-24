#[muttest::mutate]
fn x() -> usize {
    let y = 1;
    1 + 2 + y
}

#[muttest::mutate]
fn z() -> bool {
    1 < 2
}

#[muttest::mutate]
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
