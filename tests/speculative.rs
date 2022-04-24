use std::ops::{Add, Sub};

#[muttest::mutate]
fn s() -> String {
    "a".to_owned() + "b"
}

#[muttest::mutate]
fn ints() -> i32 {
    5 * 4
}

#[muttest::mutate]
fn a1() -> O1 {
    A + A
}

#[muttest::mutate]
fn a2() -> O2 {
    A - A
}

struct A;
struct O1;
struct O2;

impl Add for A {
    type Output = O1;

    fn add(self, _: Self) -> Self::Output {
        O1
    }
}
impl Sub for A {
    type Output = O2;

    fn sub(self, _: Self) -> Self::Output {
        O2
    }
}

#[test]
fn tests() {
    assert_eq!(&s(), "ab");
    assert!(matches!(a1(), O1));
    assert!(matches!(a2(), O2));
    assert_eq!(ints(), 20);
}
