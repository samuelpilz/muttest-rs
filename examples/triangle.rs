#[muttest::mutate]
fn triangle(x: usize, y: usize, z: usize) -> &'static str {
    if x > y || y > z {
        return "lengths not sorted";
    }
    if x + y <= z {
        return "illegal";
    }
    if x == y || y == z {
        return if x == z { "equilateral" } else { "isoceles" };
    }
    let x2y2 = x * x + y * y;
    let z2 = z * z;
    if x2y2 == z2 {
        return "right angled";
    }
    if x2y2 < z2 {
        return "obtuse angled";
    }
    return "acute angled";
}

#[test]
pub fn illegal_triangle() {
    assert_eq!(triangle(1, 1, 3), "illegal");
}

#[test]
pub fn right_triangle() {
    assert_eq!(triangle(3, 4, 5), "right angled");
}

#[test]
pub fn acute_triangle() {
    assert_eq!(triangle(4, 5, 6), "acute angled");
}

#[test]
pub fn obtuse_triangle() {
    assert_eq!(triangle(2, 4, 5), "obtuse angled");
}
