use std::{cmp::Ordering, mem::swap};

#[muttest::mutate]
fn triangle(mut x: usize, mut y: usize, mut z: usize) -> &'static str {
    if y > x {
        swap(&mut x, &mut y);
    }
    if z < y {
        swap(&mut z, &mut y);
    }
    if y > x {
        swap(&mut x, &mut y);
    }

    if x + y <= z {
        return "illegal";
    }
    let x2y2 = x * x + y * y;
    match x2y2.cmp(&(z * z)) {
        Ordering::Less => "obtuse",
        Ordering::Equal => "right",
        Ordering::Greater => "acute",
    }
}

#[test]
pub fn illegal_triangle() {
    assert_eq!(triangle(1, 1, 3), "illegal");
}

#[test]
pub fn right_triangle() {
    assert_eq!(triangle(3, 4, 5), "right");
}

#[test]
pub fn acute_triangle() {
    assert_eq!(triangle(5, 4, 6), "acute");
}

#[test]
pub fn obtuse_triangle() {
    assert_eq!(triangle(2, 4, 5), "obtuse");
}
