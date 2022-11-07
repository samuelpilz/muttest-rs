#![cfg_attr(not(test), allow(dead_code))]
#![allow(clippy::all)]

pub mod sort;
pub mod triangle;

#[muttest::mutate]
fn not_covered() -> isize {
    1 + 2
}
#[test]
fn no_cover() {
    if false {
        not_covered();
    }
}
