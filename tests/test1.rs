#![allow(unused_parens)]

#[muttest::mutate]
fn x() -> usize {
    let y = 1;
    // let y = 1;
    1 + 2 + y
}
#[muttest::mutate]
fn y() -> usize {
    let v = vec![1, 2, 5];
    v.into_iter().sum::<usize>() + 5
}

#[test]
fn tests() {
    assert_eq!(x(), 4);
    assert_eq!(y(), 13);
}
